-- Sherman Pay Jing Hao 
-- Sunday, 22. March 2015
-- Huffman Tree

import Data.Word
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Bits as Bits

import qualified Data.List as List
import qualified System.Environment as Env
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as B

data Tree a = Null
            | Node (Maybe a) (Tree a) (Tree a)
              deriving (Show, Eq)

data Direction = LDir
               | RDir
                 deriving (Show, Enum)
                       
leafNode :: a -> Tree a
leafNode x = Node (Just x) Null Null
             
emptyNode :: Tree a -> Tree a -> Tree a
emptyNode l r = Node Nothing l r
                
emptyLeaf :: Tree a
emptyLeaf = emptyNode Null Null

nullNode :: Tree a -> Bool
nullNode Null = True             
nullNode _ = False             
         
isLeaf :: Tree a -> Bool
isLeaf (Node (Just x) Null Null) = True
isLeaf _  = False

nSpaces :: Int -> String
nSpaces n = take n $ repeat ' '

-- Pretty Prints the tree
printTree :: (Show a) => Tree a -> IO (Tree a)
printTree = printTreeAux 0
    where printTreeAux n Null = do
            putStrLn (nSpaces n ++ "$")
            return Null
          printTreeAux n (Node (Just x) l r) = do
            putStrLn (nSpaces n ++ (show x))
            printTreeAux (n + 2) l
            printTreeAux (n + 2) r
            return (Node (Just x) l r)
          printTreeAux n (Node Nothing l r) = do
            putStrLn (nSpaces n ++ "''")
            printTreeAux (n + 2) l
            printTreeAux (n + 2) r
            return (Node Nothing l r)
                         
-- Given a Huffman tree. Create an assoc list of (symbol, encoding)
encodeTree :: Tree a -> [(a, [Direction])]
encodeTree tree = 
    let encoding = treeEncodingAux [] [] tree
    in map (\(Just x, en) -> (x, en)) encoding
    where treeEncodingAux enc bits (Node (Just x) _ _) = (Just x, bits):enc
          treeEncodingAux enc bits (Node Nothing l r) = lres ++ rres
              where lres = (treeEncodingAux enc (LDir:bits) l)
                    rres = (treeEncodingAux enc (RDir:bits) r)
                           
-- Decode a given symbol
-- Takes a (symbol, encoding) pair and returns a tree with that symbol as a leaf node
decodeSymbol :: (a, [Direction]) -> Tree a -> Tree a
decodeSymbol path Null = 
    decodeSymbol path emptyLeaf
decodeSymbol (sym, b:bs) (Node x l r) = 
    case b of
      LDir -> Node x (decodeSymbol (sym, bs) l) r
      RDir -> Node x l (decodeSymbol (sym, bs) r)
decodeSymbol (sym, []) _ = leafNode sym

bitsPath :: [Int] -> [Direction]
bitsPath = map toEnum 
           
pathBits :: [Direction] -> [Int]
pathBits = map fromEnum 

-- Decode an encoding into a Huffman tree
decodeTree :: [(a, [Direction])] -> Tree a
decodeTree = foldr decodeSymbol Null

listToPQ :: (Ord a) => [(k, a)] -> PQueue.MinPQueue a (Tree k)
listToPQ = PQueue.fromList . map (\(k, x) -> (x, leafNode k))

mapToPQ :: (Ord k, Ord a) => Map.Map k a -> PQueue.MinPQueue a (Tree k)
mapToPQ = PQueue.fromList . map (\(k, x) -> (x, leafNode k)) . Map.toList
              
pqTakeTwo :: (Ord k, Num k) => PQueue.MinPQueue k (Tree a) -> 
             (k, (Tree a), (Tree a), PQueue.MinPQueue k (Tree a))
pqTakeTwo pq = (k1 + k2, x1, x2, resPQ)
    where ((k1, x1), newPQ) = (PQueue.deleteFindMin pq)
          ((k2, x2), resPQ) = PQueue.deleteFindMin newPQ

pqMerge :: (Ord k, Num k) => PQueue.MinPQueue k (Tree a) -> PQueue.MinPQueue k (Tree a)
pqMerge pq = 
    PQueue.insert k (Node Nothing x1 x2) newPQ
    where (k, x1, x2, newPQ) = pqTakeTwo pq

pqToTree :: (Ord k, Num k) => PQueue.MinPQueue k (Tree a) -> Tree a
pqToTree pq =
    if PQueue.size pq == 1 then
        case PQueue.getMin pq of
          Just (_, val) -> val
          Nothing -> error "Priority Queue should have 1 element"
    else
        pqToTree $ pqMerge pq

listToTree :: (Ord k, Ord a, Num a) => [(k, a)] -> (Tree k)
listToTree = pqToTree . listToPQ
             
-- Take a List of 0s and 1s and convert it into a byte
bitsToByte :: [Int] -> Word8
bitsToByte bits = 
    fromIntegral $ foldr (\(x, e) sum -> x * 2^e + sum) 0 $ zip bits exps
    where exps = reverse [0..(length bits - 1)]
                         
huffmanTree :: (Ord k) => [k] -> Tree k
huffmanTree = pqToTree . mapToPQ . countFreqs
                
type Encoding k = Map.Map k Word8
type Decoding v = Map.Map Word8 v

huffmanEncode :: (Ord k) => [k] -> Encoding k
huffmanEncode = Map.fromList . map (\(x, dirs) -> (x, bitsToByte $ pathBits dirs)) . encodeTree . huffmanTree

huffmanDecode :: (Ord k) => [k] -> Decoding k
huffmanDecode = Map.fromList . map (\(x, dirs) -> (bitsToByte $ pathBits dirs, x)) . encodeTree . huffmanTree
               
compressOne :: (Ord k) => Encoding k -> k -> Word8
compressOne = (Map.!) 
    
compress :: Encoding Char -> String -> B.ByteString
compress encoding = B.pack . map (compressOne encoding)
                    
decompressOne :: (Ord k) => Decoding k -> Word8 -> k
decompressOne = (Map.!) 
decompress :: Decoding Char -> B.ByteString -> String
decompress decoding = map (decompressOne decoding) . B.unpack 

countFreqs :: (Ord k, Ord a, Num a) => [k] -> Map.Map k a
countFreqs = foldr (\x res -> Map.insertWith (+) x 1 res) Map.empty

usage :: IO()
usage = do
  progName <- Env.getProgName
  putStrLn $ foldr (++) "" $ List.intersperse " " ["Usage:", "./" ++ progName, "FILE"]

main :: IO()
main = do
  args <- Env.getArgs
  case length args of
    1 -> do contents <- IO.readFile fileName
            let encoding = huffmanEncode contents
                decoding = huffmanDecode contents
                compressed = compress encoding contents
                decompressed = decompress decoding compressed
            putStrLn decompressed
            return ()
        where fileName = args !! 0
    _ -> usage 
