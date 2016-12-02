{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

#ifdef WITH_WALLET
import           Data.List          ((!!))

import           Pos.Crypto         (unsafeHash)
import           Pos.DHT            (DHTNodeType (..))
import           Pos.Genesis        (genesisAddresses, genesisSecretKeys)
import           Pos.Launcher       (BaseParams (..), LoggingParams (..), NodeParams (..),
                                     submitTxReal)
import           Pos.Ssc.GodTossing (GtParams (..), SscGodTossing, genesisVssKeyPairs)
#ifdef WITH_WEB
import           Pos.Wallet.Web     (walletServeWeb)
#endif

import           WalletOptions      (WalletCommand (..), WalletOptions (..),
                                     getWalletOptions)

main :: IO ()
main = do
    WalletOptions {..} <- getWalletOptions
    case woCommand of
        SubmitTx {..} -> do
            let i = fromIntegral stGenesisIdx
            let params =
                    NodeParams
                    { npDbPath = Nothing
                    , npRebuildDb = False
                    , npSystemStart = 1477706355381569 --arbitrary value
                    , npSecretKey = genesisSecretKeys !! i
                    , npBaseParams = BaseParams
                                      { bpLoggingParams = LoggingParams
                                                          { lpRunnerTag     = "wallet"
                                                          , lpHandlerPrefix = stLogsPrefix
                                                          , lpConfigPath    = stLogConfig
                                                          }
                                      , bpPort = 24962
                                      , bpDHTPeers = stDHTPeers
                                      , bpDHTKeyOrType = Right DHTClient
                                      , bpDHTExplicitInitial = False
                                      }
                    , npCustomUtxo = Nothing
                    , npTimeLord = False
                    , npJLFile = Nothing
                    }
                gtParams =
                    GtParams
                    {
                      gtpRebuildDb  = False
                    , gtpDbPath     = Nothing
                    , gtpSscEnabled = False
                    , gtpVssKeyPair = genesisVssKeyPairs !! i
                    }
            let addr = genesisAddresses !! i
            let txId = unsafeHash addr
            submitTxReal @SscGodTossing params gtParams (txId, 0) (addr, 10)
#ifdef WITH_WEB
        ServeWallet {..} -> do
            walletServeWeb swPort
#else
            panic "Web is disabled!"
#endif
#else
main :: IO ()
main = panic "Wallet is disabled!"
#endif
