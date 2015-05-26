module MPV where
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
    
data MPVHandleRaw = MPVHandleRaw
newtype MPVHandle = MPVHandle (Ptr MPVHandleRaw)
data MPVEventRaw = MPVEventRaw
newtype MPVEvent = MPVEvent (Ptr MPVEventRaw)
foreign import ccall "mpv_create" mpv_create :: IO MPVHandle
foreign import ccall "mpv_set_option_string" mpv_set_option_string :: MPVHandle -> CString -> CString -> IO Int
foreign import ccall "mpv_initialize" mpv_initialize :: MPVHandle -> IO Int
foreign import ccall "mpv_command" mpv_command :: MPVHandle -> Ptr CString -> IO Int
foreign import ccall "mpv_wait_event" mpv_wait_event :: MPVHandle -> CDouble -> IO MPVEvent
foreign import ccall "mpv_terminate_destroy" mpv_terminate_destroy :: MPVHandle -> IO ()
