{-# LANGUAGE PatternSynonyms #-}
module MPV.Error where
import Foreign.C
import Foreign.C.String
import System.IO.Unsafe
newtype Error = Error CInt
pattern Success             = Error 0
pattern EventQueueFull      = Error (-1)
pattern NoMem               = Error (-2)
pattern Uninitialized       = Error (-3)
pattern InvalidParameter    = Error (-4)
pattern OptionNotFount      = Error (-5)
pattern OptionFormat        = Error (-6)
pattern OptionError         = Error (-7)
pattern PropertyNotFound    = Error (-8)
pattern PropertyFormat      = Error (-9)
pattern PropertyUnavailable = Error (-10)
pattern PropertyError       = Error (-11)
pattern ErrorCommand        = Error (-12)
pattern LoadingFailed       = Error (-13)
pattern AOInitFailed        = Error (-14)
pattern VOInitFailed        = Error (-15)
pattern NothingToPlay       = Error (-16)
pattern UnknownFormat       = Error (-17)
pattern Unsupported         = Error (-18)
pattern NotImplemented      = Error (-19)
foreign import ccall "mpv_error_string" mpv_error_cstring :: Error -> CString
mpvErrorString :: Error -> String
mpvErrorString err = unsafePerformIO $ peekCAString (mpv_error_cstring err)
instance Show Error where
    show e = mpvErrorString e;
