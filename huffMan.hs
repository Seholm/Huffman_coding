--readfile
import System.IO
import Data.List
import Data.Maybe
import Data.Bits
import qualified Data.ByteString as B
import Test.QuickCheck
import qualified Data.Map as Map

--main function that reads a file and...
compressAndDecompressFIle :: FilePath -> IO()
compressAndDecompressFIle fp = do f<- readFile fp
                                  let freqList = readAllFromString f
                                  let tree = makeTree $ sortList freqList
                                  let mapp = createMap freqList tree
                                  putStrLn $ "Compressing "++fp++".."
                                  let compStr = compressString f mapp
                                  B.writeFile ("Zipped"++fp) (convertToByte compStr)
                                  putStrLn "DecompressingFile"
                                  nf<- B.readFile ("Zipped"++fp)
                                  writeFile ("Unzipped"++fp) (decompressString (convertFromByte nf) tree)
                                  originalFile <- getFileSize fp
                                  compressedFile <- getFileSize ("Zipped"++fp)
                                  putStrLn $ (take 4 $ show (((fromInteger compressedFile)/( fromInteger originalFile))*100 ))++"% of original size"
                                  putStrLn ("Done")


getFileSize :: FilePath -> IO Integer
getFileSize x = do handle <- openFile x ReadMode
                   size <- hFileSize handle
                   hClose handle
                   return size

------------------------------------Reads and sorts input--------------------------------------------

--calculates the frequency of characters from a string
readAllFromString :: String -> [(Char,Int)]
readAllFromString str     = readAllFromString' (drop 1(sort str)) [((head (sort str)),1)] where
  readAllFromString' [] l      = l
  readAllFromString' (x:xs) l  = if x== fst (head l) then readAllFromString' xs ((inc (head l)):(drop 1 l))
                                 else readAllFromString' xs ((x,1):l) where
                                   inc (ch,i) = (ch,(i+1))

--tests the readAllFromString function
props_readAllFromString :: String -> Bool
props_readAllFromString str = props_readAllFromString' str  (readAllFromString str) where
  props_readAllFromString' [] _     = True
  props_readAllFromString' (x:xs) l = head [snd y|y <- l, fst y == x] == length (filter (== x) (x:xs)) &&
                                      props_readAllFromString' (filter (/=x) xs) l


--Sort a list of frequencyTupels
sortList :: [(Char,Int)] -> [(Char,Int)]
sortList l = sortBy smallest l where
  smallest a b  | snd a < snd b = LT
                | otherwise = GT

--tests the sortList function
props_sortList :: [(Char,Int)] -> Bool
props_sortList list = sort [snd x | x <- sortList list] == [snd x | x <- sortList list]



------------------------------------Creates the HuffmanTree--------------------------------------------

--Tree that represents the HuffmanTree
data HuffTree = Leaf (Char,Int) | Tree Int HuffTree HuffTree | Empty
  deriving Show

--checks if a Hufftree is a leaf
isLeaf :: HuffTree -> Bool
isLeaf (Leaf l) = True
isLeaf _      = False

--Insert values into tree, the smallest values at the bottom of the tree and the most common in the top.
makeTree :: [(Char,Int)] -> HuffTree
makeTree l = makeTree' (map Leaf l) where
  makeTree' l | length l == 0 = Empty
              | length l ==1 = head l
              | otherwise = makeTree' $ inserTree (addSubTree l) (drop 2 l)

--Inserts a tree in the list of all the trees, in the correct order in regard to frequencie
inserTree :: HuffTree -> [HuffTree] -> [HuffTree]
inserTree (Tree i t1 t2) []                = [(Tree i t1 t2)]
inserTree (Tree i t1 t2) (x:xs) | isLeaf x = if (getVal x) < i then x:(inserTree (Tree i t1 t2) xs) else (Tree i t1 t2):x:xs
                                | otherwise = if (getValTree x) < i then x:(inserTree (Tree i t1 t2) xs) else (Tree i t1 t2):x:xs where
                                  getVal (Leaf (_,i)) = i
                                  getValTree (Tree i _ _) = i

--takes the first two trees in a list and combines them to a tree, with the first tree in the list as leftchild
--and the second tree in the list as rightChild
addSubTree :: [HuffTree] -> HuffTree
addSubTree l =  if isLeaf firstNode && isLeaf sndNode then addSubTreeLeaf firstNode sndNode
                      else if not (isLeaf firstNode) && isLeaf sndNode then addSubTreeOneTreeLeft firstNode sndNode
                      else if isLeaf firstNode && not (isLeaf sndNode) then addSubTreeOneTreeRight firstNode sndNode
                      else addSubTreeTwoTrees firstNode sndNode where
                      firstNode = head l
                      sndNode = head (drop 1 l)

--creates a tree of two leafs
addSubTreeLeaf :: HuffTree -> HuffTree -> HuffTree
addSubTreeLeaf (Leaf l1) (Leaf l2) =  Tree (snd l1 + snd l2) (Leaf l1) (Leaf l2)

--creates a tree of one tree as leftChild and one leaf as rightChild
addSubTreeOneTreeLeft :: HuffTree -> HuffTree -> HuffTree
addSubTreeOneTreeLeft (Tree i t1 t2) (Leaf l) = Tree (i + snd l) ((Tree i t1 t2)) (Leaf l)

--creates a tree of one leaf as leftChild and one tree as rightChild
addSubTreeOneTreeRight :: HuffTree -> HuffTree -> HuffTree
addSubTreeOneTreeRight (Leaf l) (Tree i t1 t2)  = Tree (i + snd l) (Leaf l) ((Tree i t1 t2))

--creates a tree of one two trees
addSubTreeTwoTrees :: HuffTree -> HuffTree -> HuffTree
addSubTreeTwoTrees (Tree i1 t1 t2) (Tree i2 t3 t4)  = Tree (i1 + i2) (Tree i1 t1 t2) ((Tree i2 t3 t4))

-----------------------------------------Compress the file---------------------------------------------------

--creates a map of all the paths for the characters
createMap :: [(Char,Int)] -> HuffTree -> Map.Map Char String
createMap l tree = createMap' l Map.empty tree where
  createMap' [] m _         = m
  createMap' (tup:tups) m t =  createMap' tups (Map.insert (fst tup) (fromJust $ traverseTree ( fst tup) t) m) t


--traverses the tree to find the correct character and returns a string representing the "path" in the tree,
-- if it goes left a 0 is given and for right a 1
traverseTree :: Char -> HuffTree -> Maybe String
traverseTree  c  t               = traverseTree' c t ""
traverseTree' c (Tree i t1 t2) s = if isNothing (traverseTree' c t2 (s++"1")) then
                                    (traverseTree' c t1 (s++"0")) else (traverseTree' c t2 (s++"1"))
traverseTree' c (Leaf (ch,_)) s  = if c == ch then Just s else Nothing

--Compresses The string given a string and a HuffManTree for that string
compressString :: String -> Map.Map Char String ->  String
compressString []     m = []
compressString (x:xs) m = (m Map.! x)++(compressString xs m)


--converts a string of size 8 of 0's and 1's to a byteString to minimise the amount of bits used
stringToByteString8bit :: String -> B.ByteString
stringToByteString8bit str = stringToByteString8bit' str zeroBits 0 where
  stringToByteString8bit'  []      b _ = B.singleton b
  stringToByteString8bit' ('0':xs) b i = stringToByteString8bit' xs b (i+1)
  stringToByteString8bit' ('1':xs) b i = stringToByteString8bit' xs ( setBit b i) (i+1)

--converts the whole string to a bytestring
convertToByte :: String -> B.ByteString
convertToByte str = convertToByte' str B.empty where
  convertToByte' []  l = l
  convertToByte' str l = convertToByte' (drop 8 str)  (B.append l (stringToByteString8bit $ take 8 str))

--for testing pruposes, convertToByte is used in real scenarios
compress :: String -> String
compress str = compressString str ( createMap (readAllFromString str) (makeTree $ sortList (readAllFromString str)))

--TODO: Add tree to file

--codeTree

--------------------------------------Decompress the file ---------------------------------------------------


--decompresses a compressed file, 0 represents left in the tree and 1 represents right
decompressString :: String -> HuffTree -> String
decompressString    str t                          = traverseTreeDecode' str t t ""
traverseTreeDecode' []       (Leaf (ch,_))  _    s = reverse (ch:s)
traverseTreeDecode' []          _           _    s = reverse s
traverseTreeDecode' ('1':xs) (Tree i t1 t2) root s = traverseTreeDecode' xs t2 root s
traverseTreeDecode' ('0':xs) (Tree i t1 t2) root s = traverseTreeDecode' xs t1 root s
traverseTreeDecode' l        (Leaf (ch,_))  root s = traverseTreeDecode' l root root (ch:s)

--TODO: decompress tree

--converts a Bytestring back to a regular String
byteStringToString8bit :: B.ByteString -> String
byteStringToString8bit str = [ test1or0 (testBit (B.head str) x) | x<-[0..7] ] where
  test1or0 True = '1'
  test1or0 False = '0'


  --converts the whole string to a bytestring
convertFromByte :: B.ByteString -> String
convertFromByte str = convertFromByte' str "" where
  convertFromByte' bStr  l | B.null bStr = l
                           | otherwise   = convertFromByte' (B.init bStr) ((byteStringToString8bit $ B.singleton (B.last bStr))++l)


--tests if first compressing and then decompressing gives back the same stirng (lossless)
prop_Compression :: String -> Bool
prop_Compression str = decompressString (compress str) tree == str where
  tree = makeTree $ sortList (readAllFromString str)
