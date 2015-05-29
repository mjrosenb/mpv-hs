{-# LANGUAGE PatternSynonyms #-}
module MPV.Event where
import Foreign.C
import Data.ByteString
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import qualified MPV.Format as Format
import qualified MPV.Error as E
newtype EventID             = EventID CInt
pattern None                = EventID 0
pattern Shutdown            = EventID 1
pattern LogMessage          = EventID 2
pattern GetPropertyReply    = EventID 3
pattern SetPropertyReply    = EventID 4
pattern CommandReply        = EventID 5
pattern StartFile           = EventID 6
pattern EndFile             = EventID 7
pattern FileLoaded          = EventID 8
pattern TracksChanged       = EventID 9
pattern TrackSwitched       = EventID 10
pattern Idle                = EventID 11
pattern Pause               = EventID 12
pattern UnPause             = EventID 13
pattern Tick                = EventID 14
pattern ScriptInputDispatch = EventID 15
pattern ClientMessage       = EventID 16
pattern VideoReconfig       = EventID 17
pattern AudioReconfig       = EventID 18
pattern MetadatUpdate       = EventID 19
pattern Seek                = EventID 20
pattern PlaybackRestart     = EventID 21
pattern PropertyChange      = EventID 22
pattern ChapterChange       = EventID 23
pattern QueueOverflow       = EventID 24
        
instance Show EventID where
    show None = "None"
    show Shutdown = "Shutdown"
    show LogMessage = "LogMessage"
    show GetPropertyReply = "GetPropertyReply"
    show SetPropertyReply = "SetPropertyReply"
    show CommandReply = "CommandReply"
    show StartFile = "StartFile"
    show EndFile = "EndFile"
    show FileLoaded = "FileLoaded"
    show TracksChanged = "TracksChanged"
    show TrackSwitched = "TrackSwitched"
    show Idle = "Idle"
    show Pause = "Pause"
    show UnPause = "UnPause"
    show Tick = "Tick"
    show ScriptInputDispatch = "ScriptInputDispatch"
    show ClientMessage = "ClientMessage"
    show VideoReconfig = "VideoReconfig"
    show AudioReconfig = "AudioReconfig"
    show MetadatUpdate = "MetadatUpdate"
    show Seek = "Seek"
    show PlaybackRestart = "PlaybackRestart"
    show PropertyChange = "PropertyChange"
    show ChapterChange = "ChapterChange"
    show QueueOverflow = "QueueOverflow"
                              
newtype LogLevel = LogLevel CInt deriving Show
pattern LogLevelNone  = LogLevel 0
pattern LogLevelFatal = LogLevel 10
pattern LogLevelError = LogLevel 20
pattern LogLevelWarn  = LogLevel 30
pattern LogLevelInfo  = LogLevel 40
pattern LogLevelV     = LogLevel 50
pattern LogLevelDebug = LogLevel 60
pattern LogLevelTrace = LogLevel 70


newtype EndFileReason = EndFileReason CInt deriving Show
pattern EOF   = EndFileReason 0
pattern Stop  = EndFileReason 2
pattern Quit  = EndFileReason 3
pattern Error = EndFileReason 4

data EventData =
      EProperty { name :: String,
                  format :: Format.Format,
                  dataValue :: Format.Node
                }
    | ELogMessage { prefix :: String,
                    level :: String,
                    text  :: String
                 }
    | EEndFile { reason :: EndFileReason,
                 eofError :: Maybe E.Error
              }
    | EClientMessage { args :: [String] }
      deriving Show
newtype SProperty      = SProperty {fromProperty :: EventData}
newtype SLogMessage    = SLogMessage {fromLogMessage :: EventData}
newtype SEndFile       = SEndFile {fromEndFile :: EventData}
newtype SClientMessage = SClientMessage {fromClientMessage :: EventData}
instance Storable SProperty where
    sizeOf _ = undefined
    peek ptr = return undefined
    poke ptr x = return undefined
instance Storable SLogMessage where
    sizeOf _ = undefined
    peek ptr = do
      prefixP <- peekByteOff ptr 0
      levelP <-  peekByteOff ptr (sizeOf (undefined :: Ptr Char))
      textP <-  peekByteOff ptr (2*sizeOf (undefined :: Ptr Char))
      prefix <- peekCAString prefixP
      level  <- peekCAString levelP
      text   <- peekCAString textP
      return $ SLogMessage $ ELogMessage prefix level text
    poke _ _ = return undefined
instance Storable SEndFile where
    sizeOf _ = undefined
    peek ptr = do
      reason <- EndFileReason <$> peekByteOff ptr 0
      error <- E.Error <$> peekByteOff ptr (sizeOf (undefined :: CInt))
      return $ SEndFile $ EEndFile reason (Just error)
instance Storable SClientMessage where
    sizeOf _ = undefined
    peek ptr = do
      numArgs <- peekByteOff ptr 0
      argv <- peekByteOff ptr (sizeOf (undefined :: Ptr Int)) -- N.B. This is probably aligned to pointer-sized quantities
      rawArgv <- peekArray numArgs argv
      argv <- mapM peekCString rawArgv
      return $ SClientMessage $ EClientMessage argv
data Event = Event {
      eventID :: EventID,
      error :: Maybe E.Error,
      replyUserdata :: Maybe Word64,
      event :: Maybe EventData
    } deriving Show
instance Storable Event where
    sizeOf _ = 2*sizeOf (undefined :: CInt) + sizeOf (undefined :: CLong) + sizeOf (undefined :: Ptr CInt)
    poke = undefined
    peek ptr = do
      eID <- EventID <$> peekByteOff ptr 0
      err <- case eID of
              GetPropertyReply -> Just . E.Error <$> peekByteOff ptr (sizeOf (undefined :: CInt))
              SetPropertyReply -> Just . E.Error <$> peekByteOff ptr (sizeOf (undefined :: CInt))
              CommandReply     -> Just . E.Error <$> peekByteOff ptr (sizeOf (undefined :: CInt))
              _                -> return Nothing
      replyUserdata <- case eID of
                        GetPropertyReply -> Just <$> peekByteOff ptr (2*sizeOf (undefined :: CInt))
                        SetPropertyReply -> Just <$> peekByteOff ptr (2*sizeOf (undefined :: CInt))
                        CommandReply     -> Just <$> peekByteOff ptr (2*sizeOf (undefined :: CInt))
                        PropertyChange   -> Just <$> peekByteOff ptr (2*sizeOf (undefined :: CInt))
                        _                -> return Nothing
      let off = (sizeOf (undefined :: CInt) + sizeOf (undefined :: Ptr CInt))
      dataPtr <- peekByteOff ptr off
      dataEvent <- case eID of
              GetPropertyReply -> Just . fromProperty <$> peekByteOff dataPtr 0
              PropertyChange   -> Just . fromProperty <$> peekByteOff dataPtr 0
              LogMessage       -> Just . fromLogMessage <$> peekByteOff dataPtr 0
              ClientMessage    -> Just . fromClientMessage <$> peekByteOff dataPtr 0
              EndFile          -> Just . fromEndFile <$> peekByteOff dataPtr 0
              _                -> return Nothing
      return $ Event eID err replyUserdata dataEvent
