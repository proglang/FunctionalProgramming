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
  

copyFile' source target =
    undefined
  
doTwice io =
    undefined
    
doNot io =
    undefined

sortFile :: FilePath -> FilePath -> IO ()
-- sortFile inFile outFile
-- reads inFile, sorts its lines, and writes the result to out
-- recall
-- sort :: Ord a => [a] -> [a]
-- lines :: String -> [String]
-- unlines :: [String] -> String
sortFile inFile outFile =
  undefined
  
sequence' :: [IO a] -> IO [a]
sequence' = undefined  


printTable :: [String] -> IO ()
{-
printTable ["New York", "Rio", "Tokio"]
outputs
1: New York
2: Rio
3: Tokio
-}
printTable ss = undefined

