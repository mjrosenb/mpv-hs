{-# LANGUAGE RecursiveDo, PatternSynonyms #-}
import MPV
import MPV.Event
import MPV.Raw.Event
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import System.Environment
import Foreign.Storable
main = do
  ctx <- MPV.create
  MPV.setOptionString ctx "input-default-bindings" "yes"
  MPV.setOptionString ctx "input-vo-keyboard" "yes"
  MPV.initialize ctx
  args <- getArgs
  MPV.command ctx ["loadfile", args!!0]
  let loop = do
         event <- MPV.waitEvent ctx 10000
         print event
         case eventID event of
           Shutdown -> return ()
           _ -> loop
  loop
  MPV.terminateDestroy ctx
