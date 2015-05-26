{-# LANGUAGE RecursiveDo #-}
import MPV
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import System.Environment
recCize' f all [] =  f all
recCize' f a (h:t) = withCString h (\cur -> recCize' f (cur:a) t)
recCize :: ([CString] -> IO a) -> [String] -> IO a
recCize f l =
     recCize' (f . reverse) [] l

callWithArray :: (Ptr CString -> IO a) -> [String] -> IO a
callWithArray f argv =
    let innerCall cstrings = withArray0 nullPtr cstrings f
    in recCize innerCall argv
main = do
  ctx <- mpv_create
  withCString "input-default-bindings" (\ name ->
   withCString "yes" (\val -> mpv_set_option_string ctx name val))
  withCString "input-vo-keyboard" (\ name ->
   withCString "yes" (\val -> mpv_set_option_string ctx name val))
  mpv_initialize ctx
  args <- getArgs
  callWithArray (mpv_command ctx) ["loadfile", args!!0]
  let loop = do
         event <- mpv_wait_event ctx 10000
         print "Not Done yet"
         loop
  loop
  mpv_terminate_destroy ctx
