{-# LANGUAGE PatternSynonyms #-}
module MPV.Event where
import Foreign.C
import Data.ByteString
import Data.Word
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
        

newtype LogLevel = LogLevel CInt
pattern LogLevelNone  = LogLevel 0
pattern LogLevelFatal = LogLevel 10
pattern LogLevelError = LogLevel 20
pattern LogLevelWarn  = LogLevel 30
pattern LogLevelInfo  = LogLevel 40
pattern LogLevelV     = LogLevel 50
pattern LogLevelDebug = LogLevel 60
pattern LogLevelTrace = LogLevel 70


newtype EndFileReason = EndFileReason CInt
pattern EOF   = EndFileReason 0
pattern Stop  = EndFileReason 2
pattern Quit  = EndFileReason 3
pattern Error = EndFileReason 4
                
data Event =
      ELogMessage { prefix :: String,
                    level :: String,
                    text  :: String
                 }
    | EEndFile { reason :: EndFileReason,
                 eofError :: Maybe E.Error
              }
    | EClientMessage { args :: [String] }

data EventShim = EventShim {
      eventID :: EventID,
      error :: Maybe E.Error,
      replyUserdata :: Word64,
      event :: Event
    }
        
