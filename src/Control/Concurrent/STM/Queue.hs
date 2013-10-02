{-# LANGUAGE FunctionalDependencies, FlexibleInstances, MultiParamTypeClasses #-}
module Control.Concurrent.STM.Queue where
  
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue



class Queue q read write | q -> read,q -> write where
  readQueue :: q -> STM  read
  tryReadQueue :: q -> STM (Maybe read)
  peekQueue :: q -> STM  read
  tryPeekQueue :: q -> STM (Maybe read)
  writeQueue :: q -> write -> STM ()
  unGetQueue :: q -> write -> STM ()
  isEmptyQueue :: q -> STM Bool
  


instance Queue (TQueue a) a a  where
    readQueue = readTQueue
    tryReadQueue = tryReadTQueue
    peekQueue = peekTQueue
    tryPeekQueue = tryPeekQueue
    writeQueue = writeTQueue
    unGetQueue = unGetTQueue
    isEmptyQueue = isEmptyTQueue

instance Queue (TBQueue a) a a  where
    readQueue = readTBQueue
    tryReadQueue = tryReadTBQueue
    peekQueue = peekTBQueue
    tryPeekQueue = tryPeekQueue
    writeQueue = writeTBQueue
    unGetQueue = unGetTBQueue
    isEmptyQueue = isEmptyTBQueue
    
    
  
instance Queue (TBMQueue a) (Maybe a)  a  where
    readQueue = readTBMQueue
    tryReadQueue = tryReadTBMQueue
    peekQueue = peekTBMQueue
    tryPeekQueue = tryPeekQueue
    writeQueue = writeTBMQueue
    unGetQueue = unGetTBMQueue
    isEmptyQueue = isEmptyTBMQueue
    
instance Queue (TMQueue a) (Maybe a) a  where
    readQueue = readTMQueue
    tryReadQueue = tryReadTMQueue
    peekQueue = peekTMQueue
    tryPeekQueue = tryPeekQueue
    writeQueue = writeTMQueue
    unGetQueue = unGetTMQueue
    isEmptyQueue = isEmptyTMQueue
