module Control.Concurrent.STM.ClosableChan where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM.TMChan


class ClosableChan q where
  isClosedChan :: q -> STM Bool
  closeChan :: q -> STM ()
  
instance ClosableChan (TMChan a) where
  isClosedChan = isClosedTMChan
  closeChan = closeTMChan

instance ClosableChan (TBMChan a) where
  isClosedChan = isClosedTBMChan
  closeChan = closeTBMChan
