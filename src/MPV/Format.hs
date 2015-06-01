module MPV.Format where
import Data.Int
import qualified Data.ByteString as BS
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import qualified MPV.Raw.Format as Raw
import Data.List
import MPV.Error as E
type Format = Raw.Format
data Node = NodeEmpty
          | NodeString String
          | NodeFlag   Int32
          | NodeInt64  Int64
          | NodeDouble Double
          | NodeList   [Node]
          | NodeMap [(String, Node)]
          | NodeByteArray BS.ByteString deriving Show

formatFromNode NodeEmpty = Raw.None
formatFromNode (NodeString _) = Raw.String
formatFromNode (NodeFlag _) = Raw.Flag
formatFromNode (NodeInt64 _) = Raw.Int64
formatFromNode (NodeDouble _) = Raw.Double
formatFromNode (NodeList _) = Raw.NodeArray
formatFromNode (NodeMap _) = Raw.NodeMap
formatFromNode (NodeByteArray _) = Raw.ByteArray

-- newtype WithHandler a a' r = WithHandler a -> (a' -> IO b) -> IO b
-- instance Monad (WithHandler a a') where
--     return x = WithHandler (\x f -> return x)
--     WithHandler f >>= g =
--         WithHandler (\ x h -> do a <- f h

peekNode ptr =
    do tag <- Raw.Format <$> peekByteOff ptr 8
       case tag of
         Raw.None -> return NodeEmpty
         Raw.String -> NodeString <$> (peekByteOff ptr 0 >>= peekCString)
         Raw.OSDString -> NodeString <$> (peekByteOff ptr 0 >>= peekCString)
         Raw.Flag -> NodeFlag <$> peekByteOff ptr 0
         Raw.Int64 -> NodeInt64 <$> peekByteOff ptr 0
         Raw.Double -> NodeDouble <$>  peekByteOff ptr 0
         Raw.Node -> peekByteOff ptr 0 >>= peekNode
         Raw.NodeArray ->
             do listPtr <- peekByteOff ptr 0
                num <- peekByteOff listPtr 0
                valsPtr <- peekByteOff listPtr (alignment (undefined :: Ptr a))
                vals <- peekArray num valsPtr
                NodeList <$> mapM peekNode vals
         Raw.NodeMap ->
             do listPtr <- peekByteOff ptr 0
                num <- peekByteOff listPtr 0
                valsPtr <- peekByteOff listPtr (alignment (undefined :: Ptr a))
                keysPtr <- peekByteOff listPtr (2*alignment (undefined :: Ptr a))
                valsRaw <- peekArray num valsPtr
                keysRaw <- peekArray num keysPtr
                vals <- mapM peekNode valsRaw
                keys <- mapM peekCString keysRaw
                return . NodeMap $ zip keys vals
nodeSize = 8 + sizeOf (0 :: CInt)
nodeListSize = sizeOf (undefined :: Ptr Char) * 3

withNode node f =
    allocaBytes nodeSize $ \ nodePtr -> do
      pokeByteOff nodePtr 8 (Raw.formatToInt . formatFromNode $ node)
      case node of
        NodeString str -> withCString str $ \ cstr ->
                          do pokeByteOff nodePtr 0 cstr
                             f nodePtr
        NodeFlag flag ->
            do pokeByteOff nodePtr 0 flag
               f nodePtr
        NodeInt64 int ->
            do pokeByteOff nodePtr 0 int
               f nodePtr
        NodeDouble d ->
            do pokeByteOff nodePtr 0 d
               f nodePtr
        NodeList l -> allocaBytes nodeListSize $ \ nl ->
                      do pokeByteOff nodePtr 0 nl
                         pokeByteOff nl 0 (genericLength l :: CInt)
                         allocaArray (length l) $ \nodeArr ->
                             do pokeByteOff nl (alignment (undefined :: Ptr a)) nodeArr
                                foldr (\ (i,curnode) k ->
                                          withNode curnode (\ cnode ->
                                             pokeElemOff nodeArr i cnode >> k))
                                      (f nodePtr)
                                      (zip [1..] l)
        NodeMap l -> allocaBytes nodeListSize $ \ nl ->
                      do pokeByteOff nodePtr 0 nl
                         pokeByteOff nl 0 (genericLength l :: CInt)
                         allocaArray (length l) $ \valArr ->
                             allocaArray (length l) $ \keyArr ->
                                 do pokeByteOff nl (alignment (undefined :: Ptr a)) valArr
                                    pokeByteOff nl (2*alignment (undefined :: Ptr a)) keyArr
                                    foldr (\ (i, (key, val)) k ->
                                               withNode val (\ valPtr ->
                                                    do pokeElemOff valArr i valPtr
                                                       withCString key (\cstr ->
                                                          do pokeElemOff keyArr i cstr
                                                             k
                                                                       )))
                                          (f nodePtr)
                                          (zip [1..] l)

withRetPtr k =
    allocaBytes nodeSize (\retPtr -> do
                            err <- k retPtr
                            case err of
                              E.Success -> Right <$> peekNode retPtr
                              x -> return $ Left x
                         )
