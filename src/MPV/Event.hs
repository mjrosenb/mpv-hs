{-# LANGUAGE PatternSynonyms #-}
module MPV.Event where
import Foreign.C
import Data.ByteString
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import qualified MPV.Format as Format
import qualified MPV.Raw.Format as RawFormat
import qualified MPV.Error as E
import MPV.Raw.Event

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

peekEventProperty ptr =
    do namePtr <- peekByteOff ptr 0
       name <- peekCAString namePtr
       fmt <- RawFormat.Format <$> peekByteOff ptr (sizeOf (undefined :: Ptr Char))
       let valOff = sizeOf (undefined :: Ptr Char) + alignment (undefined :: Ptr Char)
       val <- case fmt of
               RawFormat.None -> return Format.NodeEmpty
               RawFormat.String -> Format.NodeString <$> (peekByteOff ptr valOff >>=  peekCAString)
               RawFormat.Flag -> Format.NodeFlag <$> peekByteOff ptr valOff
               RawFormat.Int64 -> Format.NodeInt64 <$> peekByteOff ptr valOff
               RawFormat.Double -> Format.NodeDouble <$> peekByteOff ptr valOff
               RawFormat.Node -> peekByteOff ptr valOff >>= Format.peekNode
               -- TODO: handle aggregate types
       return $ EProperty name fmt val

peekEventLogMessage ptr = do
      prefixP <- peekByteOff ptr 0
      levelP <-  peekByteOff ptr (sizeOf (undefined :: Ptr Char))
      textP <-  peekByteOff ptr (2*sizeOf (undefined :: Ptr Char))
      prefix <- peekCAString prefixP
      level  <- peekCAString levelP
      text   <- peekCAString textP
      return $ ELogMessage prefix level text

peekEventEndFile ptr = do
      reason <- EndFileReason <$> peekByteOff ptr 0
      error <- E.Error <$> peekByteOff ptr (sizeOf (undefined :: CInt))
      return $ EEndFile reason (Just error)
peekEventClientMessage ptr = do
      numArgs <- peekByteOff ptr 0
      argv <- peekByteOff ptr (sizeOf (undefined :: Ptr Int)) -- N.B. This is probably aligned to pointer-sized quantities
      rawArgv <- peekArray numArgs argv
      argv <- mapM peekCString rawArgv
      return $ EClientMessage argv
data Event = Event {
      eventID :: EventID,
      error :: Maybe E.Error,
      replyUserdata :: Maybe Word64,
      event :: Maybe EventData
    } deriving Show
instance Storable Event where
    sizeOf _ = 2*sizeOf (undefined :: CInt) + sizeOf (undefined :: CLong) + sizeOf (undefined :: Ptr CInt)
    alignment _ = alignment (undefined :: Ptr CInt)
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
      let off = (2*sizeOf (undefined :: CInt) + sizeOf (undefined :: Ptr CInt))
      dataPtr <- peekByteOff ptr off
      dataEvent <- case eID of
              GetPropertyReply -> Just <$> peekEventProperty dataPtr
              PropertyChange   -> Just <$> peekEventProperty dataPtr
              LogMessage       -> Just <$> peekEventLogMessage dataPtr
              ClientMessage    -> Just <$> peekEventClientMessage dataPtr
              EndFile          -> Just <$> peekEventEndFile dataPtr
              _                -> return Nothing
      return $ Event eID err replyUserdata dataEvent
