{-# LANGUAGE PatternSynonyms #-}
module MPV.Raw.Format where
import Foreign.C
import Data.Int
newtype Format    = Format {formatToInt :: CInt} deriving Show
pattern None      = Format 0
pattern String    = Format 1
pattern OSDString = Format 2
pattern Flag      = Format 3
pattern Int64     = Format 4
pattern Double    = Format 5
pattern Node      = Format 6
pattern NodeArray = Format 7
pattern NodeMap   = Format 8
pattern ByteArray = Format 9
