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

copyFile source target = do
  content <- readFile source
  writeFile target content

doTwice :: IO a -> IO a
doTwice io = do
  io
  io

doNot :: IO a -> IO Int
doNot io = do
  let x = 3 + 2
  return x

sortFile :: FilePath -> FilePath -> IO ()
-- sortFile inFile outFile
-- reads inFile, sorts its lines, and writes the result to out
-- recall
-- sort :: Ord a => [a] -> [a]
-- lines :: String -> [String]
-- unlines :: [String] -> String
sortFile inFile outFile = do
  content <- readFile inFile
  let content2 = unlines $ sort $ lines content
  writeFile outFile content2

sortFile2 inFile outFile =
  readFile inFile >>= \content ->
      let content2 = unlines $ sort $ lines content in
      writeFile outFile content2


sortFile3 inFile outFile =
  readFile inFile >>=
               (writeFile outFile . unlines . sort . lines)
   

            
sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (h:t) = do
  h2 <- h
  t2 <- sequence' t
  return (h2:t2)
         
sequence_' :: [IO a] -> IO ()
sequence_' [] = return ()
sequence_' (h:t) = do
  h
  sequence_' t
         

realPrintTable :: [String] -> IO ()
{-
printTable ["New York", "Rio", "Tokio"]
outputs
1: New York
2: Rio
3: Tokio
-}
printTable _ [] = return ()
printTable n (h:t) = do
  putString (show n ++ ": ")
  putString h
  putString "\n"
  printTable (n+1) t

printTable2 ss =
    let ss = map putString ss in
    sequence' ss
             
realPrintTable ss = printTable 1 ss

