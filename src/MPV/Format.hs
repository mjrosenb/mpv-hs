{-# LANGUAGE PatternSynonyms #-}
module MPV.Format where
import Foreign.C
import Data.ByteString as BS
import Data.Int
newtype Format    = Format CInt
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

data Node = NodeString String
          | NodeFlag   Int32
          | NodeInt64  Int64
          | NodeDouble Double
          | NodeList   [Node]
          | NodeByteArray BS.ByteString
