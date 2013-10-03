{-# LANGUAGE FunctionalDependencies, FlexibleInstances, MultiParamTypeClasses #-}
module Control.Concurrent.STM.Chan where
  
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TBChan


class Chan q read write | q -> read,q -> write where
  readChan :: q -> STM  read
  tryReadChan :: q -> STM (Maybe read)
  peekChan :: q -> STM  read
  tryPeekChan :: q -> STM (Maybe read)
  writeChan :: q -> write -> STM ()
  unGetChan :: q -> write -> STM ()
  isEmptyChan :: q -> STM Bool
  


instance Chan (TChan a) a a  where
    readChan = readTChan
    tryReadChan = tryReadTChan
    peekChan = peekTChan
    tryPeekChan = tryPeekChan
    writeChan = writeTChan
    unGetChan = unGetTChan
    isEmptyChan = isEmptyTChan

instance Chan (TBChan a) a a  where
    readChan = readTBChan
    tryReadChan = tryReadTBChan
    peekChan = peekTBChan
    tryPeekChan = tryPeekChan
    writeChan = writeTBChan
    unGetChan = unGetTBChan
    isEmptyChan = isEmptyTBChan
    
    
  
instance Chan (TBMChan a) (Maybe a)  a  where
    readChan = readTBMChan
    tryReadChan = tryReadTBMChan
    peekChan = peekTBMChan
    tryPeekChan = tryPeekChan
    writeChan = writeTBMChan
    unGetChan = unGetTBMChan
    isEmptyChan = isEmptyTBMChan
    
instance Chan (TMChan a) (Maybe a) a  where
    readChan = readTMChan
    tryReadChan = tryReadTMChan
    peekChan = peekTMChan
    tryPeekChan = tryPeekChan
    writeChan = writeTMChan
    unGetChan = unGetTMChan
    isEmptyChan = isEmptyTMChan
