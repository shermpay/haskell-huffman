-- Sherman Pay Jing Hao 
-- Sunday, 22. March 2015
-- Huffman Tree

import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQueue

data Tree a = Null
            | Node (Maybe a) (Tree a) (Tree a)
              deriving (Show, Eq)
                       
leafNode :: a -> (Tree a)
leafNode x = Node (Just x) Null Null
             
emptyNode :: a -> a -> (Tree a)
emptyNode l r = Node Nothing (leafNode l) (leafNode r)

nullNode :: Tree a -> Bool
nullNode Null = True             
nullNode _ = False             
         
isLeaf :: Tree a -> Bool
isLeaf (Node (Just x) Null Null) = True
isLeaf _  = False

nSpaces :: Int -> String
nSpaces n = take n $ repeat ' '

printTree :: (Show a) => Tree a -> IO ()
printTree = printTreeAux 0
    where printTreeAux n Null = return ()
          printTreeAux n (Node (Just x) l r) = do
            putStrLn (nSpaces n ++ (show x))
            printTreeAux (n + 2) l
            printTreeAux (n + 2) r
          printTreeAux n (Node (Nothing) l r) = do
            putStrLn (nSpaces n ++ "$")
            printTreeAux (n + 2) l
            printTreeAux (n + 2) r

listToPQ :: (Ord k) => [(k, a)] -> PQueue.MinPQueue k (Tree a)
listToPQ = PQueue.fromList . map (\(k, x) -> (k, leafNode x))

mapToPQ :: (Ord k) => Map.Map k a -> PQueue.MinPQueue k (Tree a)
mapToPQ = PQueue.fromList . map (\(k, x) -> (k, leafNode x)) . Map.toList
              
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
