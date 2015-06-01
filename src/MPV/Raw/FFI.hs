module MPV.Raw.FFI where
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import qualified MPV.Error as E
import qualified MPV.Format as Format
import qualified MPV.Event as Event
import qualified MPV.Raw.Event as Event
import qualified MPV.Raw.Format
data MPVHandleRaw = MPVHandleRaw
newtype MPVHandle = MPVHandle (Ptr MPVHandleRaw)
newtype MPVEvent = MPVEvent (Ptr Event.Event)
foreign import ccall "mpv_create" mpv_create :: IO MPVHandle
foreign import ccall "mpv_set_option_string" mpv_set_option_string :: MPVHandle -> CString -> CString -> IO E.Error
foreign import ccall "mpv_initialize" mpv_initialize :: MPVHandle -> IO E.Error
foreign import ccall "mpv_command" mpv_command :: MPVHandle -> Ptr CString -> IO E.Error
foreign import ccall "mpv_wait_event" mpv_wait_event :: MPVHandle -> CDouble -> IO MPVEvent
foreign import ccall "mpv_client_name" mpv_client_name :: MPVHandle -> IO CString
foreign import ccall "mpv_detach_destroy" mpv_detach_destroy :: MPVHandle -> IO ()
foreign import ccall "mpv_terminate_destroy" mpv_terminate_destroy :: MPVHandle -> IO ()
foreign import ccall "mpv_create_client" mpv_create_client :: MPVHandle -> CString -> IO MPVHandle
foreign import ccall "mpv_load_config_file" mpv_load_config_file :: MPVHandle -> CString -> IO ()
foreign import ccall "mpv_suspend" mpv_suspend :: MPVHandle -> IO ()
foreign import ccall "mpv_resume" mpv_resume :: MPVHandle -> IO ()
foreign import ccall "mpv_get_time_us" mpv_get_time_us :: MPVHandle -> IO CLong

foreign import ccall "mpv_set_option" mpv_set_option :: MPVHandle -> CString -> Format.Format -> Ptr a -> IO  E.Error
                                         
foreign import ccall "mpv_command_node" mpv_command_node :: MPVHandle -> Ptr Format.Node -> Ptr Format.Node -> IO E.Error
foreign import ccall "mpv_command_async" mpv_command_async :: MPVHandle -> CLong -> Ptr CString -> IO E.Error
foreign import ccall "mpv_command_node_async" mpv_command_node_async :: MPVHandle -> CLong -> Ptr Format.Node -> IO E.Error
foreign import ccall "mpv_set_property" mpv_set_property :: MPVHandle -> CString -> Format.Format -> Ptr a -> IO E.Error
foreign import ccall "mpv_set_property_string" mpv_set_property_string :: MPVHandle -> CString -> CString -> IO E.Error
foreign import ccall "mpv_set_property_async" mpv_set_property_async :: MPVHandle -> CLong -> CString -> Format.Format -> Ptr a -> IO E.Error

foreign import ccall "mpv_get_property" mpv_get_property :: MPVHandle -> CString -> Format.Format -> Ptr a -> IO E.Error
foreign import ccall "mpv_get_property_string" mpv_get_property_string :: MPVHandle -> CString -> IO CString
foreign import ccall "mpv_get_property_async" mpv_get_property_async :: MPVHandle -> CLong -> CString -> IO E.Error

foreign import ccall "mpv_request_event" mpv_request_event :: MPVHandle -> Event.EventID -> CInt -> IO E.Error
foreign import ccall "mpv_request_log_messages" mpv_request_log_messages :: MPVHandle -> CString -> IO E.Error
foreign import ccall "mpv_wakeup" mpv_wakeup :: MPVHandle -> IO ()
