module Control.Concurrent.STM.ClosableQueue where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue


class ClosableQueue q where
  isClosedQueue :: q -> STM Bool
  closeQueue :: q -> STM ()
  
instance ClosableQueue (TMQueue a) where
  isClosedQueue = isClosedTMQueue
  closeQueue = closeTMQueue

instance ClosableQueue (TBMQueue a) where
  isClosedQueue = isClosedTBMQueue
  closeQueue = closeTBMQueue
