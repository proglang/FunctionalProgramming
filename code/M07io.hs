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
  
