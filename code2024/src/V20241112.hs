module V20241112 where

copyFile :: String -> String -> IO ()
copyFile source target =
  readFile source >>= writeFile target

copyFile' :: String -> String -> IO ()
copyFile' source target = do
  xs <- readFile source
  writeFile target xs

copyFile'' source target =
  readFile source >>= \xs ->
  writeFile target xs

doTwice :: IO a -> IO a
doTwice io = do
  io
  io

doNot :: IO a -> IO ()
doNot io = do
  return ()

-- definition of (>>)
(>>>) :: IO a -> IO b -> IO b
ma >>> mb = ma >>= \_ -> mb

