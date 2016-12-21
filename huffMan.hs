--readfile
import System.IO
import Data.List
import Data.Maybe
import Data.Bits
import Data.Ord
import Data.Word
import Data.Char
import qualified Data.ByteString as B
import Test.QuickCheck
import qualified Data.Map as Map

--main function that reads a file and...
compressAndDecompressFile :: FilePath -> IO()
compressAndDecompressFile fp = do f<- readFile fp
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
readAllFromString str = Map.toList $ Map.fromListWith (+) (map (\x -> (x,1))str)

--tests the readAllFromString function
props_readAllFromString :: String -> Bool
props_readAllFromString str = props_readAllFromString' str  (readAllFromString str) where
  props_readAllFromString' [] _     = True
  props_readAllFromString' (x:xs) l = head [i |(c,i) <- l, c == x] == length (filter (== x) (x:xs)) &&
                                      props_readAllFromString' (filter (/=x) xs) l

--Sort a list of frequencyTupels
sortList :: [(Char,Int)] -> [(Char,Int)]
sortList l = sortOn snd l

--tests the sortList function
props_sortList :: [(Char,Int)] -> Bool
props_sortList list = sort [i | (c,i) <- sortList list] == [ i | (c,i) <- sortList list]



------------------------------------Creates the HuffmanTree--------------------------------------------

--Tree that represents the HuffmanTree
data HuffTree = Leaf (Char,Int) | Tree Int HuffTree HuffTree | Empty | Stop Int
  deriving Show

frequenc :: HuffTree -> Int
frequenc (Leaf (c,i))     = i
frequenc (Tree i _ _)     = i
frequenc _                = 1

--Insert values into tree, the smallest values at the bottom of the tree and the most common in the top.
makeTree :: [(Char,Int)] -> HuffTree
makeTree l = makeTree' ((Stop 1):(map Leaf l)) where
  makeTree' []         = Empty
  makeTree' (x:[])     = x
  makeTree' (x1:x2:xs) = makeTree' $ inserTree (addSubTree x1 x2 ) xs

--Inserts a tree in the list of all the trees, in the correct order in regard to frequencie
inserTree :: HuffTree -> [HuffTree] -> [HuffTree]
inserTree t lT = insertBy (comparing frequenc) t lT

--takes the first two trees in a list and combines them to a tree, with the first tree in the list as leftchild
--and the second tree in the list as rightChild
addSubTree :: HuffTree -> HuffTree -> HuffTree
addSubTree t1 t2 = Tree (frequenc t1 + frequenc t2) t1 t2

-----------------------------------------Compress the file---------------------------------------------------

--creates a map of all the paths for the characters
createMap :: [(Char,Int)] -> HuffTree -> Map.Map (Maybe Char) String
createMap l tree = createMap' l Map.empty tree where
  createMap' []         m t = (Map.insert Nothing (fromJust $ traverseTree Nothing t) m)
  createMap' ((c,i):tups) m t =  createMap' tups (Map.insert (Just c) (fromJust $ traverseTree (Just c) t) m) t

--traverses the tree to find the correct character and returns a string representing the "path" in the tree,
-- if it goes left a 0 is given and for right a 1
traverseTree :: Maybe Char -> HuffTree -> Maybe String
traverseTree  c  t               = traverseTree' c t ""
traverseTree' c (Tree i t1 t2) s = if isNothing (traverseTree' c t2 (s++"1")) then
                                    (traverseTree' c t1 (s++"0")) else (traverseTree' c t2 (s++"1"))
traverseTree' (Just c) (Leaf (ch,_)) s  = if c == ch then Just s else Nothing
traverseTree' (Just c) (Stop i) s       = Nothing
traverseTree' Nothing  (Stop i) s       = Just s
traverseTree' Nothing  (Leaf _) s       = Nothing

--Compresses The string given a string and a path map for each character for that string
compressString :: String -> Map.Map (Maybe Char) String ->  String
compressString []     m = (m Map.! Nothing)
compressString (x:xs) m = (m Map.! Just x)++(compressString xs m)

--converts a string of size 8 of 0's and 1's to a byteString to minimise the amount of bits used
stringToByteString8bit :: String -> Word8
stringToByteString8bit str = stringToByteString8bit' str zeroBits 0 where
  stringToByteString8bit' []       b _ = b
  stringToByteString8bit' ('0':xs) b i = stringToByteString8bit' xs b (i+1)
  stringToByteString8bit' ('1':xs) b i = stringToByteString8bit' xs ( setBit b i) (i+1)

--converts the whole string to a bytestring
convertToByte :: String -> B.ByteString
convertToByte str = convertToByte' str [] where
  convertToByte' []  l = B.reverse $ B.pack l
  convertToByte' str l = convertToByte' (drop 8 str)  ((stringToByteString8bit $ take 8 str):l)

--for testing pruposes, convertToByte is used in real scenarios
compress :: String -> String
compress str = compressString str ( createMap (readAllFromString str) (makeTree $ sortList (readAllFromString str)))

--TODO: Add tree to file

--------------------------------------Decompress the file ---------------------------------------------------


--decompresses a compressed file, 0 represents left in the tree and 1 represents right
decompressString :: String -> HuffTree -> String
decompressString    str t                          = traverseTreeDecode' str t t ""
traverseTreeDecode' []       (Leaf (ch,_))  _    s = reverse (ch:s)
traverseTreeDecode' []          _           _    s = reverse s
traverseTreeDecode' ('1':xs) (Tree i t1 t2) root s = traverseTreeDecode' xs t2 root s
traverseTreeDecode' ('0':xs) (Tree i t1 t2) root s = traverseTreeDecode' xs t1 root s
traverseTreeDecode' l        (Leaf (ch,_))  root s = traverseTreeDecode' l root root (ch:s)
traverseTreeDecode' l        (Stop _)       root s = reverse s

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
