-- | Server part.

{-# LANGUAGE RankNTypes #-}

module Pos.Communication.Server
       ( allListeners
       , serverLoggerName
       , sscRelays
       , txRelays
       , delegationRelays
       , usRelays
       ) where

import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ
import           System.Wlog (LoggerName)

import           Pos.Binary.Communication ()
import           Pos.Block.Network (blockListeners)
import           Pos.Communication.Limits ()
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (EnqueueMsg, MkListeners (..))
import           Pos.Communication.Relay (relayListeners)
import           Pos.Communication.Util (wrapListener)
import           Pos.Delegation.Listeners (delegationRelays)
import           Pos.Network.Types (Bucket, NodeId)
import           Pos.Ssc.Listeners (sscRelays)
import           Pos.Txp.Network.Listeners (txRelays)
import           Pos.Update.Network.Listeners (usRelays)
import           Pos.Util.JsonLog (JLEvent (JLTxReceived))
import           Pos.Util.TimeWarp (jsonLog)
import           Pos.WorkMode.Class (WorkMode)

-- | All listeners running on one node.
allListeners
    :: WorkMode ctx m
    => OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m -> MkListeners m
allListeners oq enqueue = mconcat $
        -- TODO blockListeners should use 'enqueue' rather than its own
        -- block retrieval queue, no?
        [ modifier "block"        $ blockListeners oq
        , modifier "ssc"          $ relayListeners oq enqueue sscRelays
        , modifier "tx"           $ relayListeners oq enqueue (txRelays logTx)
        , modifier "delegation"   $ relayListeners oq enqueue delegationRelays
        , modifier "update"       $ relayListeners oq enqueue usRelays
        ]
  where
    logTx = jsonLog . JLTxReceived
    modifier lname mkL = mkL { mkListeners = mkListeners' }
      where
        mkListeners' v p =
            let ls = mkListeners mkL v p
                f = wrapListener (serverLoggerName <> lname)
            in  map f ls

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
