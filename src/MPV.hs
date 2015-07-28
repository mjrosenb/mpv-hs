module MPV where
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import qualified MPV.Error as E
import qualified MPV.Format as Format
import qualified MPV.Event as Event
import qualified MPV.Raw.FFI as FFI
import qualified MPV.Raw.Format as RawFormat
import Data.Functor
type MPVHandle = FFI.MPVHandle
recCize' f all [] =  f all
recCize' f a (h:t) = withCString h (\cur -> recCize' f (cur:a) t)
recCize :: ([CString] -> IO a) -> [String] -> IO a
recCize f l =
     recCize' (f . reverse) [] l

withArgV :: [String] -> (Ptr CString -> IO a) -> IO a
withArgV argv f =
    let innerCall cstrings = withArray0 nullPtr cstrings f
    in recCize innerCall argv

create :: IO FFI.MPVHandle
create = FFI.mpv_create
setOptionString :: FFI.MPVHandle -> String -> String -> IO E.Error
setOptionString ctx l r = withCString l
                      (\ l' -> withCString r
                         (\r' -> FFI.mpv_set_option_string ctx l' r') )
initialize :: FFI.MPVHandle -> IO E.Error
initialize = FFI.mpv_initialize

command ctx argv = withArgV argv (FFI.mpv_command ctx)

waitEvent :: FFI.MPVHandle -> Double -> IO Event.Event
waitEvent ctx to =
    do FFI.MPVEvent eP <- FFI.mpv_wait_event ctx (CDouble to)
       peek eP
clientName :: FFI.MPVHandle -> IO String
clientName  ctx = FFI.mpv_client_name ctx >>= peekCAString

detachDestroy :: FFI.MPVHandle -> IO ()
detachDestroy = FFI.mpv_detach_destroy

terminateDestroy :: FFI.MPVHandle -> IO ()
terminateDestroy = FFI.mpv_terminate_destroy

createClient :: FFI.MPVHandle -> String -> IO FFI.MPVHandle
createClient ctx str = withCString str (FFI.mpv_create_client ctx)

loadConfigFile :: FFI.MPVHandle -> String -> IO ()
loadConfigFile ctx file = withCString file (FFI.mpv_load_config_file ctx)

suspend :: FFI.MPVHandle -> IO ()
suspend = FFI.mpv_suspend
resume = FFI.mpv_resume
getTimeUS :: FFI.MPVHandle -> IO Int
getTimeUS ctx = fromIntegral <$> FFI.mpv_get_time_us ctx

setOption :: FFI.MPVHandle -> String -> Format.Node -> IO E.Error
setOption ctx name node =
    Format.withNode node (\snode ->
       withCString name (\cname ->
          FFI.mpv_set_option ctx cname RawFormat.Node snode))

commandNode :: FFI.MPVHandle -> Format.Node -> IO (Either E.Error Format.Node)
commandNode ctx node =  Format.withNode node (\inPtr ->
                          Format.withRetPtr (\retPtr ->
                            FFI.mpv_command_node ctx inPtr retPtr)
                          )


commandAsync :: FFI.MPVHandle -> Int -> [String] -> IO E.Error
commandAsync ctx uid cmds =
    withArgV cmds (FFI.mpv_command_async ctx (fromIntegral uid))
commandNodeAsync :: FFI.MPVHandle -> Int -> Format.Node -> IO E.Error
commandNodeAsync ctx uid cmd =
    Format.withNode cmd (FFI.mpv_command_node_async ctx (fromIntegral uid))
setProperty :: FFI.MPVHandle -> String -> Format.Node -> IO E.Error
setProperty ctx name val =
    withCString name (\cname ->
        Format.withNode val (\cval ->
             FFI.mpv_set_property ctx cname RawFormat.Node cval))
setPropertyString :: FFI.MPVHandle -> String -> String -> IO E.Error
setPropertyString ctx name val =
    withCString name (\cname ->
        withCString val (\cval ->
            FFI.mpv_set_property_string ctx cname cval))
setPropertyAsync :: FFI.MPVHandle -> Int -> String -> Format.Node -> IO E.Error
setPropertyAsync ctx uid name node =
    withCString name (\cname ->
        Format.withNode node (\nval ->
            FFI.mpv_set_property_async ctx (fromIntegral uid) cname RawFormat.Node nval))
getProperty  :: FFI.MPVHandle -> String -> IO (Either E.Error Format.Node)
getProperty ctx name =
    withCString name (\cname -> Format.withRetPtr (FFI.mpv_get_property ctx cname RawFormat.Node))
getPropertyString  :: FFI.MPVHandle -> String -> IO String
getPropertyString ctx name =
    withCString name (\cname -> (FFI.mpv_get_property_string ctx cname >>= peekCAString))
getPropertyAsync :: FFI.MPVHandle -> Int -> String -> IO E.Error
getPropertyAsync ctx uid name =
    withCString name (\cname -> FFI.mpv_get_property_async ctx (fromIntegral uid) cname)
observeProperty :: FFI.MPVHandle -> Int -> String -> IO E.Error
observeProperty ctx uid name =
    withCString name (\cname -> FFI.mpv_observe_property ctx (fromIntegral uid) cname RawFormat.Node)
--requestEvent :: FFI.MPVHandle -> MPV.Raw.Event.EventID -> Bool -> IO E.Error
--requestLogMessages :: FFI.MPVHandle -> String -> IO E.Error
wakeup :: FFI.MPVHandle -> IO ()
wakeup ctx = FFI.mpv_wakeup ctx

