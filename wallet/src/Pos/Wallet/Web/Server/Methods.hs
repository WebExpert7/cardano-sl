{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Wallet web server.

module Pos.Wallet.Web.Server.Methods
       ( walletApplication
       , walletServer
       , walletServeImpl
       , walletServerOuts

       , bracketWalletWebDB
       , bracketWalletWS

       , addInitialRichAccount
       ) where

import           Universum

import           Control.Concurrent               (forkFinally)
import           Control.Lens                     (each, ix, makeLenses, traversed, (.=))
import           Control.Monad.Catch              (SomeException, try)
import qualified Control.Monad.Catch              as E
import           Control.Monad.Except             (runExceptT)
import           Control.Monad.State              (runStateT)
import qualified Data.Aeson                       as A
import           Data.ByteString.Base58           (bitcoinAlphabet, decodeBase58)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Default                     (Default (def))
import qualified Data.DList                       as DL
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (findIndex, notElem)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Text.Buildable
import           Data.Time.Clock.POSIX            (getPOSIXTime)
import           Data.Time.Units                  (Microsecond, Second)
import           Ether.Internal                   (HasLens (..))
import           Formatting                       (bprint, build, sformat, shown, stext,
                                                   (%))
import qualified Formatting                       as F
import           Network.Wai                      (Application)
import           Pos.ReportServer.Report          (ReportType (RInfo))
import           Serokell.AcidState.ExtendedState (ExtendedState)
import           Serokell.Util                    (threadDelay)
import qualified Serokell.Util.Base64             as B64
import           Serokell.Util.Text               (listJson)
import           Servant.API                      ((:<|>) ((:<|>)),
                                                   FromHttpApiData (parseUrlPiece))
import           Servant.Multipart                (fdFilePath)
import           Servant.Server                   (Handler, Server, ServerT, runHandler,
                                                   serve)
import           Servant.Utils.Enter              ((:~>) (..), enter)
import           System.IO.Error                  (isDoesNotExistError)
import           System.Wlog                      (logDebug, logError, logInfo,
                                                   logWarning)

import           Pos.Aeson.ClientTypes            ()
import           Pos.Aeson.WalletBackup           ()

import           Pos.Block.Logic                  (withBlkSemaphore_)
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Client.Txp.Balances          (getOwnUtxos)
import           Pos.Client.Txp.History           (TxHistoryEntry (..))
import           Pos.Client.Txp.Util              (computeTxFee)
import           Pos.Communication                (OutSpecs, SendActions (..), sendTxOuts,
                                                   submitMTx, submitRedemptionTx)
import           Pos.Constants                    (curSoftwareVersion, isDevelopment)
import           Pos.Context                      (GenesisUtxo)
import           Pos.Core                         (Address (..), Coin, addressF,
                                                   decodeTextAddress, getCurrentTimestamp,
                                                   getTimestamp, makeRedeemAddress,
                                                   mkCoin, sumCoins, unsafeIntegerToCoin)
import           Pos.Crypto                       (EncryptedSecretKey, PassPhrase,
                                                   SafeSigner, aesDecrypt,
                                                   changeEncPassphrase, checkPassMatches,
                                                   deriveAesKeyBS, emptyPassphrase, hash,
                                                   redeemDeterministicKeyGen,
                                                   redeemToPublic, withSafeSigner,
                                                   withSafeSigner)
import           Pos.Genesis                      (genesisDevHdwSecretKeys)
import           Pos.Reporting.MemState           (HasReportServers (..),
                                                   HasReportingContext (..))
import           Pos.Reporting.Methods            (sendReport, sendReportNodeNologs)
import           Pos.Txp                          (TxFee (..), Utxo)
import           Pos.Txp.Core                     (Tx (..), TxAux (..), TxOut (..))
import           Pos.Util                         (eitherToThrow, maybeThrow)
import           Pos.Util.BackupPhrase            (toSeed)
import qualified Pos.Util.Modifier                as MM
import           Pos.Util.Servant                 (encodeCType)
import           Pos.Util.UserSecret              (UserSecretDecodingError (..),
                                                   readUserSecret, usWalletSet)
import           Pos.Wallet.KeyStorage            (addSecretKey, deleteSecretKey,
                                                   getSecretKeys)
import           Pos.Wallet.SscType               (WalletSscType)
import           Pos.Wallet.WalletMode            (applyLastUpdate,
                                                   blockchainSlotDuration, connectedPeers,
                                                   getBalance, getLocalHistory,
                                                   localChainDifficulty,
                                                   networkChainDifficulty, waitForUpdate)
import           Pos.Wallet.Web.Account           (AddrGenSeed, GenSeed (..),
                                                   MonadKeySearch (..), genSaveRootKey,
                                                   genUniqueAccountAddress,
                                                   genUniqueAccountId, getAddrIdx,
                                                   getSKByAccAddr, getSKById,
                                                   myRootAddresses)
import           Pos.Wallet.Web.Api               (WalletApi, walletApi)
import           Pos.Wallet.Web.Backup            (AccountMetaBackup (..),
                                                   StateBackup (..), WalletBackup (..),
                                                   WalletMetaBackup (..), getStateBackup)
import           Pos.Wallet.Web.ClientTypes       (AccountId (..), Addr, CAccount (..),
                                                   CAccountId (..), CAccountInit (..),
                                                   CAccountMeta (..), CAddress (..),
                                                   CCoin, CElectronCrashReport (..), CId,
                                                   CInitialized,
                                                   CPaperVendWalletRedeem (..),
                                                   CPassPhrase (..), CProfile,
                                                   CProfile (..), CTx (..), CTxId,
                                                   CTxMeta (..), CTxs (..),
                                                   CUpdateInfo (..), CWAddressMeta (..),
                                                   CWallet (..), CWalletInit (..),
                                                   CWalletMeta (..), CWalletRedeem (..),
                                                   NotifyEvent (..), SyncProgress (..),
                                                   Wal, addrMetaToAccount, addressToCId,
                                                   encToCId, mkCCoin, mkCTxId, mkCTxs,
                                                   toCUpdateInfo, txIdToCTxId)
import           Pos.Wallet.Web.Error             (WalletError (..), rewrapToWalletError)
import qualified Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.Secret            (WalletUserSecret (..),
                                                   mkGenesisWalletUserSecret, wusAccounts,
                                                   wusWalletName)
import           Pos.Wallet.Web.Server.Sockets    (ConnectionsVar, closeWSConnections,
                                                   getWalletWebSockets, initWSConnections,
                                                   notifyAll, upgradeApplicationWS)
import           Pos.Wallet.Web.State             (AddressLookupMode (Ever, Existing), CustomAddressType (ChangeAddr, UsedAddr),
                                                   addOnlyNewTxMeta, addUpdate,
                                                   addWAddress, closeState, createAccount,
                                                   createWallet, getAccountIds,
                                                   getAccountMeta, getHistoryCache,
                                                   getNextUpdate, getProfile, getTxMeta,
                                                   getWalletAddresses, getWalletMeta,
                                                   getWalletPassLU, isCustomAddress,
                                                   openState, removeAccount,
                                                   removeHistoryCache, removeNextUpdate,
                                                   removeTxMetas, removeWallet,
                                                   setAccountMeta, setProfile,
                                                   setWalletMeta, setWalletPassLU,
                                                   setWalletSyncTip, setWalletTxMeta,
                                                   testReset, updateHistoryCache)
import           Pos.Wallet.Web.State.Storage     (WalletStorage)
import           Pos.Wallet.Web.Tracking          (CAccModifier (..), sortedInsertions,
                                                   syncWalletOnImport,
                                                   syncWalletsWithGState,
                                                   txMempoolToModifier)
import           Pos.Wallet.Web.Util              (coinDistrToOutputs,
                                                   decodeCAccountIdOrFail,
                                                   decodeCCoinOrFail, decodeCIdOrFail,
                                                   getAccountAddrsOrThrow,
                                                   getWalletAccountIds,
                                                   getWalletAddrMetas, rewrapTxError)
import           Pos.Web                          (TlsParams, serveImpl)

----------------------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------------------

-- This constraint used to be abstract (a list of classes), but specifying a
-- concrete monad is quite likely more performant.
type WalletWebMode m = m ~ Pos.Wallet.Web.Mode.WalletWebMode

makeLenses ''SyncProgress

-- | `txMempoolToModifier`, once evaluated, is passed around under this type in
-- scope of single request.
type CachedCAccModifier = CAccModifier

-- | Evaluates `txMempoolToModifier` and provides result as a parameter
-- to given function.
fixingCachedAccModifier
    :: (WalletWebMode m, MonadKeySearch key m)
    => (CachedCAccModifier -> key -> m a)
    -> key -> m a
fixingCachedAccModifier action key =
    findKey key >>= txMempoolToModifier >>= flip action key

fixCachedAccModifierFor
    :: (WalletWebMode m, MonadKeySearch key m)
    => key
    -> (CachedCAccModifier -> m a)
    -> m a
fixCachedAccModifierFor key action =
    fixingCachedAccModifier (const . action) key

walletServeImpl
    :: WalletWebMode m
    => m Application     -- ^ Application getter
    -> Word16            -- ^ Port to listen
    -> Maybe TlsParams
    -> m ()
walletServeImpl app =
    serveImpl app "127.0.0.1"

walletApplication
    :: WalletWebMode m
    => m (Server WalletApi)
    -> m Application
walletApplication serv = do
    wsConn <- getWalletWebSockets
    upgradeApplicationWS wsConn . serve walletApi <$> serv

walletServer
    :: forall ctx m.
       ( WalletWebMode m
       , HasLens GenesisUtxo ctx GenesisUtxo)
    => SendActions m
    -> m (m :~> Handler)
    -> m (Server WalletApi)
walletServer sendActions nat = do
    syncWalletsWithGState @WalletSscType =<< mapM getSKById =<< myRootAddresses
    nat >>= launchNotifier
    (`enter` servantHandlers sendActions) <$> nat

bracketWalletWebDB
    :: ( MonadIO m
       , MonadMask m
       )
    => FilePath  -- ^ Path to wallet acid-state
    -> Bool      -- ^ Rebuild flag for acid-state
    -> (ExtendedState WalletStorage -> m a)
    -> m a
bracketWalletWebDB daedalusDbPath dbRebuild =
    bracket (openState dbRebuild daedalusDbPath)
            closeState

bracketWalletWS
    :: ( MonadIO m
       , MonadMask m
       )
    => (ConnectionsVar -> m a)
    -> m a
bracketWalletWS = bracket initWS closeWSConnections
  where
    initWS = putText "walletServeImpl initWsConnection" >> initWSConnections

----------------------------------------------------------------------------
-- Notifier
----------------------------------------------------------------------------

-- FIXME: this is really inefficient. Temporary solution
launchNotifier :: WalletWebMode m => (m :~> Handler) -> m ()
launchNotifier nat =
    void . liftIO $ mapM startForking
        [ dificultyNotifier
        , updateNotifier
        ]
  where
    cooldownPeriod :: Second
    cooldownPeriod = 5

    difficultyNotifyPeriod :: Microsecond
    difficultyNotifyPeriod = 500000  -- 0.5 sec

    -- networkResendPeriod = 10         -- in delay periods
    forkForever action = forkFinally action $ const $ do
        -- TODO: log error
        -- cooldown
        threadDelay cooldownPeriod
        void $ forkForever action
    -- TODO: use Servant.enter here
    -- FIXME: don't ignore errors, send error msg to the socket
    startForking = forkForever . void . runHandler . ($$) nat
    notifier period action = forever $ do
        liftIO $ threadDelay period
        action
    dificultyNotifier = void . flip runStateT def $ notifier difficultyNotifyPeriod $ do
        whenJustM networkChainDifficulty $
            \networkDifficulty -> do
                oldNetworkDifficulty <- use spNetworkCD
                when (Just networkDifficulty /= oldNetworkDifficulty) $ do
                    lift $ notifyAll $ NetworkDifficultyChanged networkDifficulty
                    spNetworkCD .= Just networkDifficulty

        localDifficulty <- localChainDifficulty
        oldLocalDifficulty <- use spLocalCD
        when (localDifficulty /= oldLocalDifficulty) $ do
            lift $ notifyAll $ LocalDifficultyChanged localDifficulty
            spLocalCD .= localDifficulty

        peers <- connectedPeers
        oldPeers <- use spPeers
        when (peers /= oldPeers) $ do
            lift $ notifyAll $ ConnectedPeersChanged peers
            spPeers .= peers

    updateNotifier = do
        cps <- waitForUpdate
        addUpdate $ toCUpdateInfo cps
        logDebug "Added update to wallet storage"
        notifyAll UpdateAvailable

    -- historyNotifier :: WalletWebMode m => m ()
    -- historyNotifier = do
    --     cAddresses <- myCIds
    --     for_ cAddresses $ \cAddress -> do
    --         -- TODO: is reading from acid RAM only (not reading from disk?)
    --         oldHistoryLength <- length . fromMaybe mempty <$> getAccountHistory cAddress
    --         newHistoryLength <- length <$> getHistory cAddress
    --         when (oldHistoryLength /= newHistoryLength) .
    --             notifyAll $ NewWalletTransaction cAddress

walletServerOuts :: OutSpecs
walletServerOuts = sendTxOuts

----------------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------------

servantHandlers
    :: WalletWebMode m
    => SendActions m
    -> ServerT WalletApi m
servantHandlers sendActions =
     testResetAll
    :<|>

     getWallet
    :<|>
     getWallets
    :<|>
     newWallet
    :<|>
     updateWallet
    :<|>
     restoreWallet
    :<|>
     renameWSet
    :<|>
     deleteWallet
    :<|>
     importWallet
    :<|>
     changeWalletPassphrase
    :<|>

     fixingCachedAccModifier getAccount
    :<|>
     getAccounts
    :<|>
     updateAccount
    :<|>
     newAccount RandomSeed
    :<|>
     deleteAccount
    :<|>

     newAddress RandomSeed
    :<|>

     isValidAddress
    :<|>

     getUserProfile
    :<|>
     updateUserProfile
    :<|>

     newPayment sendActions
    :<|>
     getTxFee
    :<|>
     updateTransaction
    :<|>
     getHistoryLimited
    :<|>

     nextUpdate
    :<|>
     applyUpdate
    :<|>

     redeemAda sendActions
    :<|>
     redeemAdaPaperVend sendActions
    :<|>

     reportingInitialized
    :<|>
     reportingElectroncrash
    :<|>

     (blockchainSlotDuration <&> fromIntegral)
    :<|>
     pure curSoftwareVersion
    :<|>
     syncProgress
    :<|>
     importStateJSON
    :<|>
     exportStateJSON

-- getAddresses :: WalletWebMode m => m [CId]
-- getAddresses = map addressToCId <$> myAddresses

-- getBalances :: WalletWebMode m => m [(CId, Coin)]
-- getBalances = join $ mapM gb <$> myAddresses
--   where gb addr = (,) (addressToCId addr) <$> getBalance addr

getUserProfile :: WalletWebMode m => m CProfile
getUserProfile = getProfile

updateUserProfile :: WalletWebMode m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

getWAddressBalance :: WalletWebMode m => CWAddressMeta -> m Coin
getWAddressBalance addr =
    getBalance <=< decodeCIdOrFail $ cwamId addr

getWAddress
    :: WalletWebMode m
    => CachedCAccModifier -> CWAddressMeta -> m CAddress
getWAddress cachedAccModifier cAddr = do
    let aId = cwamId cAddr
    balance <- getWAddressBalance cAddr

    let getFlag customType accessMod = do
            checkDB <- isCustomAddress customType (cwamId cAddr)
            let checkMempool = elem aId . map (fst . fst) . toList $
                               MM.insertions $ accessMod cachedAccModifier
            return (checkDB || checkMempool)
    isUsed   <- getFlag UsedAddr camUsed
    isChange <- getFlag ChangeAddr camChange
    return $ CAddress aId (mkCCoin balance) isUsed isChange

getAccount :: WalletWebMode m => CachedCAccModifier -> AccountId -> m CAccount
getAccount accMod accId = do
    dbAddrs    <- getAccountAddrsOrThrow Existing accId
    let modifier   = camAddresses accMod
    let allAddrIds = gatherAddresses modifier dbAddrs
    allAddrs <- mapM (getWAddress accMod) allAddrIds
    balance  <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                mapM getWAddressBalance allAddrIds
    meta <- getAccountMeta accId >>= maybeThrow noWallet
    pure $ CAccount (encodeCType accId) meta allAddrs balance
  where
    noWallet =
        RequestError $ sformat ("No account with id "%build%" found") accId
    gatherAddresses modifier dbAddrs = do
        let memAddrs = sortedInsertions modifier
            relatedMemAddrs = filter ((== accId) . addrMetaToAccount) memAddrs
            -- @|relatedMemAddrs|@ is O(1) while @dbAddrs@ is large
            unknownMemAddrs = filter (`notElem` dbAddrs) relatedMemAddrs
        dbAddrs <> unknownMemAddrs

getWallet :: WalletWebMode m => CId Wal -> m CWallet
getWallet cAddr = do
    meta       <- getWalletMeta cAddr >>= maybeThrow noWSet
    wallets    <- getAccounts (Just cAddr)
    let walletsNum = length wallets
    balance    <- mkCCoin . unsafeIntegerToCoin . sumCoins <$>
                     mapM (decodeCCoinOrFail . caAmount) wallets
    hasPass    <- isNothing . checkPassMatches emptyPassphrase <$> getSKById cAddr
    passLU     <- getWalletPassLU cAddr >>= maybeThrow noWSet
    pure $ CWallet cAddr meta walletsNum balance hasPass passLU
  where
    noWSet = RequestError $
        sformat ("No wallet with address "%build%" found") cAddr

getWalletAddrs
    :: WalletWebMode m
    => AddressLookupMode -> CId Wal -> m [CId Addr]
getWalletAddrs = (cwamId <<$>>) ... getWalletAddrMetas

getAccounts
    :: WalletWebMode m
    => Maybe (CId Wal) -> m [CAccount]
getAccounts mCAddr = do
    whenJust mCAddr $ \cAddr -> getWalletMeta cAddr `whenNothingM_` noWSet cAddr
    accIds <- maybe getAccountIds getWalletAccountIds mCAddr
    let groupedAccIds = fmap reverse $ HM.fromListWith mappend $
                        accIds <&> \acc -> (aiWId acc, [acc])
    concatForM (HM.toList groupedAccIds) $ \(wid, walAccIds) ->
         fixCachedAccModifierFor wid $ forM walAccIds . getAccount
  where
    noWSet cAddr = throwM . RequestError $
        sformat ("No account with id "%build%" found") cAddr

getWallets :: WalletWebMode m => m [CWallet]
getWallets = getWalletAddresses >>= mapM getWallet

newPayment
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> AccountId
    -> CId Addr
    -> Coin
    -> m CTx
newPayment sa passphrase srcAccount dstAccount coin =
    sendMoney
        sa
        passphrase
        (AccountMoneySource srcAccount)
        (one (dstAccount, coin))

getTxFee
    :: WalletWebMode m
    => AccountId
    -> CId Addr
    -> Coin
    -> m CCoin
getTxFee srcAccount dstAccount coin = do
    utxo <- getMoneySourceUtxo (AccountMoneySource srcAccount)
    outputs <- coinDistrToOutputs $ one (dstAccount, coin)
    TxFee fee <- eitherToThrow =<< runExceptT (computeTxFee utxo outputs)
    pure $ mkCCoin fee

data MoneySource
    = WalletMoneySource (CId Wal)
    | AccountMoneySource AccountId
    | AddressMoneySource CWAddressMeta
    deriving (Show, Eq)

getMoneySourceAddresses :: WalletWebMode m => MoneySource -> m [CWAddressMeta]
getMoneySourceAddresses (AddressMoneySource addrId) = return $ one addrId
getMoneySourceAddresses (AccountMoneySource accId) =
    getAccountAddrsOrThrow Existing accId
getMoneySourceAddresses (WalletMoneySource wid) =
    getWalletAccountIds wid >>=
    concatMapM (getMoneySourceAddresses . AccountMoneySource)

getSomeMoneySourceAccount :: WalletWebMode m => MoneySource -> m AccountId
getSomeMoneySourceAccount (AddressMoneySource addrId) =
    return $ addrMetaToAccount addrId
getSomeMoneySourceAccount (AccountMoneySource accId) = return accId
getSomeMoneySourceAccount (WalletMoneySource wid) = do
    wAddr <- (head <$> getWalletAccountIds wid) >>= maybeThrow noWallets
    getSomeMoneySourceAccount (AccountMoneySource wAddr)
  where
    noWallets = InternalError "Wallet has no accounts"

getMoneySourceWallet :: MoneySource -> CId Wal
getMoneySourceWallet (AddressMoneySource addrId) = cwamWId addrId
getMoneySourceWallet (AccountMoneySource accId)  = aiWId accId
getMoneySourceWallet (WalletMoneySource wid)     = wid

getMoneySourceUtxo :: WalletWebMode m => MoneySource -> m Utxo
getMoneySourceUtxo =
    getMoneySourceAddresses >=>
    mapM (decodeCIdOrFail . cwamId) >=>
    getOwnUtxos

-- [CSM-237] It should be moved to `Pos.Wallet.Web.Mode`, but
-- to make it possible all this mess should be neatly separated
-- to modules and refactored
instance MonadAddresses Pos.Wallet.Web.Mode.WalletWebMode where
    type AddrData Pos.Wallet.Web.Mode.WalletWebMode = (AccountId, PassPhrase)
    getNewAddress (accId, passphrase) =
        newAddress RandomSeed passphrase accId >>=
        decodeCIdOrFail . cadId

sendMoney
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> MoneySource
    -> NonEmpty (CId Addr, Coin)
    -> m CTx
sendMoney SendActions{..} passphrase moneySource dstDistr = do
    addrMetas' <- getMoneySourceAddresses moneySource
    addrMetas <- nonEmpty addrMetas' `whenNothing`
        throwM (RequestError "Given money source has no addresses!")
    sks <- forM addrMetas $ getSKByAccAddr passphrase
    srcAddrs <- forM addrMetas $ decodeCIdOrFail . cwamId

    withSafeSigners passphrase sks $ \ss -> do
        let hdwSigner = NE.zip ss srcAddrs
        relatedAccount <- getSomeMoneySourceAccount moneySource
        outputs <- coinDistrToOutputs dstDistr
        (TxAux {taTx = tx}, inpTxOuts') <- rewrapTxError "Cannot send transaction" $
            submitMTx enqueueMsg hdwSigner outputs (relatedAccount, passphrase)

        let inpTxOuts = toList inpTxOuts'
            txHash    = hash tx
            inpAddrs  = map txOutAddress inpTxOuts
            dstAddrs  = map txOutAddress . toList $
                        _txOutputs tx
            srcWallet = getMoneySourceWallet moneySource
        logInfo $
            sformat ("Successfully spent money from "%
                     listF ", " addressF % " addresses on " %
                     listF ", " addressF)
            (toList srcAddrs)
            dstAddrs

        ts <- Just <$> getCurrentTimestamp
        ctxs <- addHistoryTx srcWallet $
            THEntry txHash tx inpTxOuts Nothing inpAddrs dstAddrs ts
        ctsOutgoing ctxs `whenNothing` throwM noOutgoingTx
  where
    noOutgoingTx = InternalError "Can't report outgoing transaction"
    -- TODO eliminate copy-paste
    listF separator formatter =
        F.later $ fold . intersperse separator . fmap (F.bprint formatter)

withSafeSigners
    :: (MonadIO m, MonadThrow m)
    => PassPhrase
    -> NonEmpty (EncryptedSecretKey)
    -> (NonEmpty SafeSigner -> m a) -> m a
withSafeSigners passphrase (sk :| sks) action =
    withSafeSigner sk (pure passphrase) $ \mss -> do
        ss <- maybeThrow (RequestError "Passphrase doesn't match") mss
        case nonEmpty sks of
            Nothing -> action (ss :| [])
            Just sks' -> do
                let action' = action . (ss :|) . toList
                withSafeSigners passphrase sks' action'

getFullWalletHistory :: WalletWebMode m => CId Wal -> m ([CTx], Word)
getFullWalletHistory cWalId = do
    addrs <- mapM decodeCIdOrFail =<< getWalletAddrs Ever cWalId

    blockHistory <- getHistoryCache cWalId >>= \case
        Just hist -> pure $ DL.fromList hist
        Nothing -> do
            logWarning $
                sformat ("getFullWalletHistory: history cache is empty for wallet #"%build)
                cWalId
            pure mempty

    localHistory <- getLocalHistory addrs

    let fullHistory = DL.toList $ localHistory <> blockHistory
    ctxs <- forM fullHistory $ addHistoryTx cWalId
    let cHistory = concatMap toList ctxs
    pure (cHistory, fromIntegral $ length cHistory)

getHistory
    :: WalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> m ([CTx], Word)
getHistory mCWalId mAccountId mAddrId = do
    -- FIXME: searching when only AddrId is provided is not supported yet.
    (cWalId, accIds) <- case (mCWalId, mAccountId) of
        (Nothing, Nothing)      -> throwM errorSpecifySomething
        (Just _, Just _)        -> throwM errorDontSpecifyBoth
        (Just cWalId', Nothing) -> do
            accIds' <- getWalletAccountIds cWalId'
            pure (cWalId', accIds')
        (Nothing, Just accId)   -> pure (aiWId accId, [accId])
    accAddrs <- map cwamId <$> concatMapM (getAccountAddrsOrThrow Ever) accIds
    addrs <- case mAddrId of
        Nothing -> pure accAddrs
        Just addr ->
            if addr `elem` accAddrs then pure [addr] else throwM errorBadAddress
    first (filter (fits addrs)) <$> getFullWalletHistory cWalId
  where
    fits :: [CId Addr] -> CTx -> Bool
    fits addrs ctx = any (relatesToAddr ctx) addrs
    relatesToAddr CTx {..} = (`elem` (ctInputAddrs ++ ctOutputAddrs))
    errorSpecifySomething = RequestError $
        "Please specify either walletId or accountId"
    errorDontSpecifyBoth = RequestError $
        "Please do not specify both walletId and accountId at the same time"
    errorBadAddress = RequestError $
        "Specified wallet/account does not contain specified address"

getHistoryLimited
    :: WalletWebMode m
    => Maybe (CId Wal)
    -> Maybe AccountId
    -> Maybe (CId Addr)
    -> Maybe Word
    -> Maybe Word
    -> m ([CTx], Word)
getHistoryLimited mCWalId mAccId mAddrId mSkip mLimit =
    first applySkipLimit <$> getHistory mCWalId mAccId mAddrId
  where
    applySkipLimit = take limit . drop skip
    limit = (fromIntegral $ fromMaybe defaultLimit mLimit)
    skip = (fromIntegral $ fromMaybe defaultSkip mSkip)
    defaultLimit = 100
    defaultSkip = 0

addHistoryTx
    :: WalletWebMode m
    => CId Wal
    -> TxHistoryEntry
    -> m CTxs
addHistoryTx cWalId wtx@THEntry{..} = do
    -- TODO: this should be removed in production
    diff <- maybe localChainDifficulty pure =<<
            networkChainDifficulty
    meta <- CTxMeta <$> case _thTimestamp of
      Nothing -> liftIO $ getPOSIXTime
      Just ts -> return $ fromIntegral (getTimestamp ts) / 1000000
    let cId = txIdToCTxId _thTxId
    addOnlyNewTxMeta cWalId cId meta
    meta' <- fromMaybe meta <$> getTxMeta cWalId cId
    walAddrMetas <- getWalletAddrMetas Ever cWalId
    mkCTxs diff wtx meta' walAddrMetas & either (throwM . InternalError) pure

newAddress
    :: WalletWebMode m
    => AddrGenSeed
    -> PassPhrase
    -> AccountId
    -> m CAddress
newAddress addGenSeed passphrase accId =
    fixCachedAccModifierFor accId $ \accMod -> do
        -- check whether account exists
        _ <- getAccount accMod accId

        cAccAddr <- genUniqueAccountAddress addGenSeed passphrase accId
        addWAddress cAccAddr
        getWAddress accMod cAccAddr

newAccount
    :: WalletWebMode m
    => AddrGenSeed -> PassPhrase -> CAccountInit -> m CAccount
newAccount addGenSeed passphrase CAccountInit {..} =
    fixCachedAccModifierFor caInitWId $ \accMod -> do
        -- check wallet exists
        _ <- getWallet caInitWId

        cAddr <- genUniqueAccountId addGenSeed caInitWId
        createAccount cAddr caInitMeta
        () <$ newAddress addGenSeed passphrase cAddr
        getAccount accMod cAddr

createWalletSafe
    :: WalletWebMode m
    => CId Wal -> CWalletMeta -> m CWallet
createWalletSafe cid wsMeta = do
    wSetExists <- isJust <$> getWalletMeta cid
    when wSetExists $
        throwM $ RequestError "Wallet with that mnemonics already exists"
    curTime <- liftIO getPOSIXTime
    createWallet cid wsMeta curTime
    getWallet cid

-- | Which index to use to create initial account and address on new wallet
-- creation
initialAccAddrIdxs :: Word32
initialAccAddrIdxs = 0

newWalletFromBackupPhrase
    :: WalletWebMode m
    => PassPhrase -> CWalletInit -> m (EncryptedSecretKey, CId Wal)
newWalletFromBackupPhrase passphrase CWalletInit {..} = do
    let CWalletMeta {..} = cwInitMeta

    skey <- genSaveRootKey passphrase cwBackupPhrase
    let cAddr = encToCId skey

    CWallet{..} <- createWalletSafe cAddr cwInitMeta
    -- can't return this result, since balances can change

    let accMeta = CAccountMeta { caName = "Initial account" }
        accInit = CAccountInit { caInitWId = cwId, caInitMeta = accMeta }
    () <$ newAccount (DeterminedSeed initialAccAddrIdxs) passphrase accInit

    return (skey, cAddr)

newWallet :: WalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
newWallet passphrase cwInit = do
    (_, wId) <- newWalletFromBackupPhrase passphrase cwInit
    updateHistoryCache wId []
    -- BListener checks current syncTip before applying update,
    -- thus setting it up to date manually here
    withBlkSemaphore_ $ \tip -> tip <$ setWalletSyncTip wId tip
    getWallet wId

restoreWallet :: WalletWebMode m => PassPhrase -> CWalletInit -> m CWallet
restoreWallet passphrase cwInit = do
    (sk, wId) <- newWalletFromBackupPhrase passphrase cwInit
    syncWalletOnImport sk
    getWallet wId

updateWallet :: WalletWebMode m => CId Wal -> CWalletMeta -> m CWallet
updateWallet wId wMeta = do
    setWalletMeta wId wMeta
    getWallet wId

updateAccount :: WalletWebMode m => AccountId -> CAccountMeta -> m CAccount
updateAccount accId wMeta = do
    setAccountMeta accId wMeta
    fixingCachedAccModifier getAccount accId

updateTransaction :: WalletWebMode m => AccountId -> CTxId -> CTxMeta -> m ()
updateTransaction accId txId txMeta = do
    setWalletTxMeta (aiWId accId) txId txMeta

deleteWallet :: WalletWebMode m => CId Wal -> m ()
deleteWallet wid = do
    accounts <- getAccounts (Just wid)
    mapM_ (deleteAccount <=< decodeCAccountIdOrFail . caId) accounts
    removeWallet wid
    removeTxMetas wid
    removeHistoryCache wid
    deleteSecretKey . fromIntegral =<< getAddrIdx wid

deleteAccount :: WalletWebMode m => AccountId -> m ()
deleteAccount = removeAccount

-- TODO: to add when necessary
-- deleteWAddress :: WalletWebMode m => CWAddressMeta -> m ()
-- deleteWAddress = removeWAddress

renameWSet :: WalletWebMode m => CId Wal -> Text -> m CWallet
renameWSet cid newName = do
    meta <- getWalletMeta cid >>= maybeThrow (RequestError "No such wallet")
    setWalletMeta cid meta{ cwName = newName }
    getWallet cid

data AccountsSnapshot = AccountsSnapshot
    { asExisting :: [CWAddressMeta]
    , asDeleted  :: [CWAddressMeta]
    }

instance Monoid AccountsSnapshot where
    mempty = AccountsSnapshot mempty mempty
    AccountsSnapshot a1 b1 `mappend` AccountsSnapshot a2 b2 =
        AccountsSnapshot (a1 <> a2) (b1 <> b2)

instance Buildable AccountsSnapshot where
    build AccountsSnapshot {..} =
        bprint
            ("{ existing: "%listJson% ", deleted: "%listJson)
            asExisting
            asDeleted

changeWalletPassphrase
    :: WalletWebMode m
    => CId Wal -> PassPhrase -> PassPhrase -> m ()
changeWalletPassphrase wid oldPass newPass = do
    oldSK <- getSKById wid

    unless (isJust $ checkPassMatches newPass oldSK) $ do
        newSK <- maybeThrow badPass $ changeEncPassphrase oldPass newPass oldSK
        deleteSK oldPass
        addSecretKey newSK
        setWalletPassLU wid =<< liftIO getPOSIXTime
  where
    badPass = RequestError "Invalid old passphrase given"
    deleteSK passphrase = do
        let nice k = encToCId k == wid && isJust (checkPassMatches passphrase k)
        midx <- findIndex nice <$> getSecretKeys
        idx  <- RequestError "No key with such address and pass found"
                `maybeThrow` midx
        deleteSecretKey (fromIntegral idx)

-- NOTE: later we will have `isValidAddress :: CId -> m Bool` which should work for arbitrary crypto
isValidAddress :: WalletWebMode m => Text -> m Bool
isValidAddress sAddr =
    pure . isRight $ decodeTextAddress sAddr

-- | Get last update info
nextUpdate :: WalletWebMode m => m CUpdateInfo
nextUpdate = getNextUpdate >>=
             maybeThrow (RequestError "No updates available")

applyUpdate :: WalletWebMode m => m ()
applyUpdate = removeNextUpdate >> applyLastUpdate

redeemAda :: WalletWebMode m => SendActions m -> PassPhrase -> CWalletRedeem -> m CTx
redeemAda sendActions passphrase CWalletRedeem {..} = do
    seedBs <- maybe invalidBase64 pure
        -- NOTE: this is just safety measure
        $ rightToMaybe (B64.decode crSeed) <|> rightToMaybe (B64.decodeUrl crSeed)
    redeemAdaInternal sendActions passphrase crWalletId seedBs
  where
    invalidBase64 =
        throwM . RequestError $ "Seed is invalid base64(url) string: " <> crSeed

-- Decrypts certificate based on:
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L205
--  * https://github.com/input-output-hk/postvend-app/blob/master/src/CertGen.hs#L160
redeemAdaPaperVend
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> CPaperVendWalletRedeem
    -> m CTx
redeemAdaPaperVend sendActions passphrase CPaperVendWalletRedeem {..} = do
    seedEncBs <- maybe invalidBase58 pure
        $ decodeBase58 bitcoinAlphabet $ encodeUtf8 pvSeed
    aesKey <- either invalidMnemonic pure
        $ deriveAesKeyBS <$> toSeed pvBackupPhrase
    seedDecBs <- either decryptionFailed pure
        $ aesDecrypt seedEncBs aesKey
    redeemAdaInternal sendActions passphrase pvWalletId seedDecBs
  where
    invalidBase58 =
        throwM . RequestError $ "Seed is invalid base58 string: " <> pvSeed
    invalidMnemonic e =
        throwM . RequestError $ "Invalid mnemonic: " <> toText e
    decryptionFailed e =
        throwM . RequestError $ "Decryption failed: " <> show e

redeemAdaInternal
    :: WalletWebMode m
    => SendActions m
    -> PassPhrase
    -> CAccountId
    -> ByteString
    -> m CTx
redeemAdaInternal SendActions {..} passphrase cAccId seedBs = do
    (_, redeemSK) <- maybeThrow (RequestError "Seed is not 32-byte long") $
                     redeemDeterministicKeyGen seedBs
    accId <- decodeCAccountIdOrFail cAccId
    -- new redemption wallet
    _ <- fixingCachedAccModifier getAccount accId

    let srcAddr = makeRedeemAddress $ redeemToPublic redeemSK
    dstAddr <- decodeCIdOrFail . cadId =<<
               newAddress RandomSeed passphrase accId
    (TxAux {..}, redeemAddress, redeemBalance) <-
        rewrapTxError "Cannot send redemption transaction" $
        submitRedemptionTx enqueueMsg redeemSK dstAddr
    -- add redemption transaction to the history of new wallet
    let txInputs = [TxOut redeemAddress redeemBalance]
    ts <- Just <$> getCurrentTimestamp
    ctxs <- addHistoryTx (aiWId accId) $
        THEntry (hash taTx) taTx txInputs Nothing [srcAddr] [dstAddr] ts
    ctsIncoming ctxs `whenNothing` throwM noIncomingTx
  where
    noIncomingTx = InternalError "Can't report incoming transaction"

reportingInitialized :: WalletWebMode m => CInitialized -> m ()
reportingInitialized cinit = do
    sendReportNodeNologs (RInfo $ show cinit) `catchAll` handler
  where
    handler e =
        logError $
        sformat ("Didn't manage to report initialization time "%shown%
                 " because of exception "%shown) cinit e

reportingElectroncrash :: forall m. WalletWebMode m => CElectronCrashReport -> m ()
reportingElectroncrash celcrash = do
    servers <- view (reportingContext . reportServers)
    errors <- fmap lefts $ forM servers $ \serv ->
        try $ sendReport [fdFilePath $ cecUploadDump celcrash]
                         []
                         (RInfo $ show celcrash)
                         "daedalus"
                         (toString serv)
    whenNotNull errors $ handler . NE.head
  where
    fmt = ("Didn't manage to report electron crash "%shown%" because of exception "%shown)
    handler :: SomeException -> m ()
    handler e = logError $ sformat fmt celcrash e

importWallet
    :: WalletWebMode m
    => PassPhrase
    -> Text
    -> m CWallet
importWallet passphrase (toString -> fp) = do
    secret <-
        rewrapToWalletError isDoesNotExistError noFile $
        rewrapToWalletError (\UserSecretDecodingError{} -> True) decodeFailed $
        readUserSecret fp
    wSecret <- maybeThrow noWalletSecret (secret ^. usWalletSet)
    wId <- cwId <$> importWalletSecret emptyPassphrase wSecret
    changeWalletPassphrase wId emptyPassphrase passphrase
    getWallet wId
  where
    noWalletSecret = RequestError "This key doesn't contain HD wallet info"
    noFile _ = RequestError "File doesn't exist"
    decodeFailed = RequestError . sformat ("Invalid secret file ("%build%")")

importWalletSecret
    :: WalletWebMode m
    => PassPhrase
    -> WalletUserSecret
    -> m CWallet
importWalletSecret passphrase WalletUserSecret{..} = do
    let key    = _wusRootKey
        wid    = encToCId key
        wMeta  = def { cwName = _wusWalletName }
    addSecretKey key
    importedWallet <- createWalletSafe wid wMeta

    for_ _wusAccounts $ \(walletIndex, walletName) -> do
        let accMeta = def{ caName = walletName }
            seedGen = DeterminedSeed walletIndex
        cAddr <- genUniqueAccountId seedGen wid
        createAccount cAddr accMeta

    for_ _wusAddrs $ \(walletIndex, accountIndex) -> do
        let accId = AccountId wid walletIndex
        newAddress (DeterminedSeed accountIndex) passphrase accId

    void $ syncWalletOnImport key

    return importedWallet

-- | Creates wallet with given genesis hd-wallet key.
addInitialRichAccount :: WalletWebMode m => Int -> m ()
addInitialRichAccount keyId =
    when isDevelopment . E.handleAll wSetExistsHandler $ do
        key <- maybeThrow noKey (genesisDevHdwSecretKeys ^? ix keyId)
        void $ importWalletSecret emptyPassphrase $
            mkGenesisWalletUserSecret key
                & wusWalletName .~ "Precreated wallet full of money"
                & wusAccounts . traversed . _2 .~ "Initial account"
  where
    noKey = InternalError $ sformat ("No genesis key #" %build) keyId
    wSetExistsHandler =
        logDebug . sformat ("Creation of initial wallet was skipped (" %build % ")")

syncProgress :: WalletWebMode m => m SyncProgress
syncProgress = do
    SyncProgress
    <$> localChainDifficulty
    <*> networkChainDifficulty
    <*> connectedPeers

testResetAll :: WalletWebMode m => m ()
testResetAll = deleteAllKeys >> testReset
  where
    deleteAllKeys = do
        keyNum <- length <$> getSecretKeys
        replicateM_ keyNum $ deleteSecretKey 0

----------------------------------------------------------------------------
-- JSON backup methods
----------------------------------------------------------------------------

restoreWalletFromBackup :: WalletWebMode m => WalletBackup -> m (Maybe CWallet)
restoreWalletFromBackup WalletBackup {..} = do
    let wId = encToCId wbSecretKey
    wExists <- isJust <$> getWalletMeta wId

    if wExists
        then do
            logWarning $
                sformat ("Wallet with id "%build%" already exists") wId
            pure Nothing
        else do
            let (WalletMetaBackup wMeta) = wbMeta
                accList = HM.toList wbAccounts
                          & each . _2 %~ \(AccountMetaBackup am) -> am

            addSecretKey wbSecretKey
            for_ accList $ \(idx, meta) -> do
                let aIdx = fromInteger $ fromIntegral idx
                    seedGen = DeterminedSeed aIdx
                accId <- genUniqueAccountId seedGen wId
                createAccount accId meta
            void $ createWalletSafe wId wMeta
            void $ syncWalletOnImport wbSecretKey
            -- Get wallet again to return correct balance and stuff
            Just <$> getWallet wId

restoreStateFromBackup :: WalletWebMode m => StateBackup -> m [CWallet]
restoreStateFromBackup (FullStateBackup walletBackups) =
    catMaybes <$> forM walletBackups restoreWalletFromBackup

importStateJSON :: WalletWebMode m => Text -> m [CWallet]
importStateJSON (toString -> fp) = do
    contents <- liftIO $ BSL.readFile fp
    wState <- either parseErr pure $ A.eitherDecode contents
    restoreStateFromBackup wState
  where
    parseErr err = throwM . RequestError $
        sformat ("Error while reading JSON backup file: "%stext) $
        toText err

exportStateJSON :: WalletWebMode m => Text -> m ()
exportStateJSON (toString -> fp) = do
    wState <- getStateBackup
    liftIO $ BSL.writeFile fp $ A.encode wState

----------------------------------------------------------------------------
-- Orphan instances
----------------------------------------------------------------------------

instance FromHttpApiData Coin where
    parseUrlPiece = fmap mkCoin . parseUrlPiece

instance FromHttpApiData Address where
    parseUrlPiece = decodeTextAddress

instance FromHttpApiData (CId w) where
    parseUrlPiece = fmap addressToCId . decodeTextAddress

instance FromHttpApiData CAccountId where
    parseUrlPiece = fmap CAccountId . parseUrlPiece

-- FIXME: unsafe (temporary, will be removed probably in future)
-- we are not checking whether received Text is really valid CTxId
instance FromHttpApiData CTxId where
    parseUrlPiece = pure . mkCTxId

instance FromHttpApiData CPassPhrase where
    parseUrlPiece = pure . CPassPhrase