import Data.List

-- entry point of a Haskell program
main :: IO ()
main = undefined

-- predefined in Prelude:
-- putChar :: Char -> IO ()

putString :: String -> IO ()
putString [] = return ()
-- putString (x:xs) = putChar x >>= \ _ -> putString xs
putString (x:xs) = putChar x >> putString xs

-- type signature is needed to avoid nasty error
putString', putString'' :: String -> IO ()
putString' = foldr (\ c io -> (putChar c >>) io) (return ())
-- eta reduce!
putString'' = foldr (\ c -> (putChar c >>)) (return ())


{-
(>>) :: IO a -> IO b -> IO b
io1 >> io2 = io1 >>= \ _ -> io2
-}

copyFile source target =
  readFile source >>= \xs -> (writeFile target) xs
  

copyFile' source target = do
  xs <- readFile source
  writeFile target xs
  
doTwice io = do
  io
  io
  
doNot io = do
  return ()

sortFile :: FilePath -> FilePath -> IO ()
-- sortFile inFile outFile
-- reads inFile, sorts its lines, and writes the result to out
-- recall
-- sort :: Ord a => [a] -> [a]
-- lines :: String -> [String]
-- unlines :: [String] -> String
sortFile inFile outFile = do
  xs <- readFile inFile
  writeFile outFile (unlines (sort (lines xs)))

sortFile' inFile outFile =
  readFile inFile >>= \xs -> writeFile outFile (unlines (sort (lines xs)))

sortFile'' inFile outFile =
  readFile inFile >>= \xs -> (writeFile outFile . unlines . sort . lines) xs

sortFile''' inFile outFile =
  readFile inFile >>= (writeFile outFile . unlines . sort . lines)
  
sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (io:ios) = do
  x <- io
  xs <- sequence' ios
  return (x:xs)
  


printTable :: [String] -> IO ()
{-
printTable ["New York", "Rio", "Tokio"]
outputs
1: New York
2: Rio
3: Tokio
-}
printTable ss =
  sequence_
  (map g
  (zip [1..length ss] ss))
  where
    g (i, str) = putStrLn (show i ++ ": " ++ str)

printTable' =
  sequence_ . map g . zip [1..]
  where
    g (i, str) = putStrLn (show i ++ ": " ++ str)

printTable'' =
  sequence_ . zipWith g [1..]
  where
    g i str = putStrLn (show i ++ ": " ++ str)

