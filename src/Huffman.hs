-- Sherman Pay Jing Hao 
-- Sunday, 22. March 2015
-- Huffman Tree

import Data.Word
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQueue
import qualified Data.Bits.Bitwise as Bitwise

import qualified Data.List as List
import qualified System.Environment as Env
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Char as C
import Data.Int

data Tree a = Null
            | Node (Maybe a) (Tree a) (Tree a)
              deriving (Show, Eq)

data Direction = LDir
               | RDir
                 deriving (Show, Enum)

type Path = [Direction]
    
type Bits = [Bool]
type Encoding k = Map.Map k Bits
                       
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
huffmanAList :: Tree a -> [(a, Path)]
huffmanAList tree = 
    let encoding = treeEncodingAux [] [] tree
    in map (\(Just x, en) -> (x, reverse en)) encoding
    where treeEncodingAux enc bits (Node (Just x) _ _) = (Just x, bits):enc
          treeEncodingAux enc bits (Node Nothing l r) = lres ++ rres
              where lres = (treeEncodingAux enc (LDir:bits) l)
                    rres = (treeEncodingAux enc (RDir:bits) r)
                           
-- Decode a given symbol
-- Takes a (symbol, encoding) pair and returns a tree with that symbol as a leaf node
huffmanToSymbol :: (a, Path) -> Tree a -> Tree a
huffmanToSymbol path Null = 
    huffmanToSymbol path emptyLeaf
huffmanToSymbol (sym, b:bs) (Node x l r) = 
    case b of
      LDir -> Node x (huffmanToSymbol (sym, bs) l) r
      RDir -> Node x l (huffmanToSymbol (sym, bs) r)
huffmanToSymbol (sym, []) _ = leafNode sym

bitsPath :: Bits -> Path
bitsPath = map (toEnum . fromEnum)
           
pathBits :: Path -> Bits
pathBits = map (toEnum . fromEnum)

-- Decode an encoding into a Huffman tree
huffmanToTree :: [(a, Path)] -> Tree a
huffmanToTree = foldr huffmanToSymbol Null
             
getSymbol :: Path -> Tree a -> (a, Path)
getSymbol path (Node m l r) =
    case m of
      Just x -> (x, path)
      Nothing -> 
          case path of
            LDir:dirs -> getSymbol dirs l
            RDir:dirs -> getSymbol dirs r
            [] -> error "End of path. Leaf not found"
getSymbol _ Null = error "Tree invalid. reached Null node"
                   
getSymbols :: Path -> Tree Char -> String
getSymbols path tree = 
    if null path then
        ""
    else
        if sym /= defaultEOF then
            sym:(getSymbols ps tree)
        else
            ""
        where (sym, ps) = getSymbol path tree

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
bitsToByte :: Bits -> Word8
bitsToByte = Bitwise.fromListLE
                         
-- Takes a EOF character and a list of characters
huffmanTree :: (Ord k) => k -> [k] -> Tree k
huffmanTree eof = pqToTree . mapToPQ . (addEOF eof) . countFreqs
                
-- Takes a huffmanTree and encodes it
huffmanMap :: (Ord a) => Tree a -> Encoding a
huffmanMap = Map.fromList . map (\(x, dirs) -> (x, pathBits dirs)) . huffmanAList

toBits :: (Ord k) => Encoding k -> k -> Bits
toBits = (Map.!) 

flattenBits :: [Bits] -> Bits
flattenBits = foldr (++) []

padBits :: Bits -> Bits
padBits bits = 
    case r of
      0 -> bits
      _ -> bits ++ (take r $ repeat False)
    where r = 8 - (length bits `mod` 8)

-- TODO: BOTTLENECK
splitBits :: Bits -> [Bits]
splitBits [] = []
splitBits bits = 
    b:splitBits bs
    where (b, bs) = (splitAt 8 bits)

splitBits' :: Bits -> [Bits]
splitBits' bits = 
    reverse $ aux bits []
    where aux [] acc = acc
          aux bits acc = 
              aux bs (b:acc)
              where (b, bs) = (splitAt 8 bits)

mapN :: ([a] -> b) -> Int -> [a] -> [b]
mapN f n [] = []
mapN f n lst =
    (f x):(mapN f n xs)
    where (x, xs) = (splitAt n lst)

-- compress :: Encoding Char -> String -> B.ByteString
compress encoding = 
    B.pack . map bitsToByte . splitBits . padBits . flattenBits . map (toBits encoding)

decompress :: Tree Char -> B.ByteString -> String
decompress tree bs = 
    getSymbols path tree
    where path = bitsPath $ (flattenBits . (map Bitwise.toListLE) . B.unpack) bs
                 
encodeTree :: Tree Char -> String
encodeTree (Node (Just x) _ _) = ['1', x]
encodeTree (Node Nothing l r) = '0':(encodeTree l) ++ (encodeTree r)
                                
decodeTree :: String -> Tree Char
decodeTree str = 
    fst $ aux str
    where aux ('0':xs) = (emptyNode left right, rs)
              where (left, ls) = aux xs
                    (right, rs) = aux ls
          aux ('1':x:xs) = (leafNode x, xs)

-- Compress a file given the filename
compressFile :: [String] -> IO ()
compressFile [inFile, outFile] = do
  contents <- IO.readFile inFile
  let tree =  huffmanTree defaultEOF contents
      encoding = huffmanMap tree
      compressed = (compress $! encoding) $! (contents ++ [defaultEOF])
      treeEncoding = encodeTree tree
  putStrLn "Compressing: " ++ inFile ++ "..."
  B.writeFile outFile $ BC.pack $ ((show $ length treeEncoding) ++ " ")
  B.appendFile outFile $ BC.pack treeEncoding
  B.appendFile outFile compressed
   
decompressFile :: [String] -> IO ()
decompressFile [inFile, outFile] = do
  -- contents <- IO.readFile inFile
  byteContents <- B.readFile inFile
  let (treeLenStr, rest) = BC.span (/= ' ') byteContents
      treeLen = read (BC.unpack treeLenStr) :: Int64
      (treeStr, body) = BC.splitAt treeLen $ BC.tail rest
      tree = decodeTree $ BC.unpack treeStr
      output = decompress tree body
  putStrLn "Decompressing: " ++ inFile ++ "..."
  B.writeFile outFile $ BC.pack output

countFreqs :: (Ord k, Ord a, Num a) => [k] -> Map.Map k a
countFreqs = foldr (\x res -> Map.insertWith (+) x 1 res) Map.empty

defaultEOF = C.chr (fromIntegral (maxBound :: Word8) :: Int)
addEOF :: (Ord k, Num a) => k -> Map.Map k a -> Map.Map k a
addEOF eof = Map.insert eof 1
             
options = [("-c", "Compress a file"), ("-d", "Decompress a file")]
          
printOptions :: IO ()
printOptions = do
  mapM_ (\(f, h) -> do putStrLn $ ("  " ++ f ++ "\t" ++ h)) options

usage :: [String] -> IO ()
usage _ = do
  progName <- Env.getProgName
  putStrLn $ foldr (++) "" $ List.intersperse " " ["Usage:", "./" ++ progName
                                                  , "[option]", "[args..]"]
  printOptions
  
execOpt :: String -> [String] -> IO ()
execOpt "-c" = compressFile
execOpt "-d" = decompressFile
execOpt _ = usage

main :: IO ()
main = do
  args <- Env.getArgs
  case length args of
    3 -> execOpt (args !! 0) (tail args)
    _ -> usage []

benchmark :: Int -> [Word8]
benchmark n =
    drop n $ mapN bitsToByte 8 (take n $ repeat True)
    
benchmarkMap :: Int -> [Bool]
benchmarkMap n =
    drop n $ map id (take n $ repeat True)

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f lst = 
    (f x):(myMap f xs)
    where ([x],xs) = splitAt 1 lst

