module V10 where

import Test.QuickCheck

-- traditional using bind

copyFile :: String -> String -> IO ()
copyFile source target =
  readFile source >>= \xs ->
  writeFile target xs

doTwice :: IO a -> IO (a, a)
doTwice io =
  io >>= \a1 -> io >>= \a2 -> return (a1, a2)

doNot :: IO a -> IO ()
doNot io =
  return ()

-- now: using do notation

copyFileDo :: String -> String -> IO ()
copyFileDo source target = do
  xs <- readFile source
  writeFile target xs

doTwiceDo io = do
  a1 <- io
  a2 <- io
  return (a1, a2)

-- the "other" bind (real name: >>)

(>>-) :: IO a -> IO b -> IO b
ma >>- mb = ma >>= \ _ -> mb

sequence' :: [IO a] -> IO [a]
sequence' ios = foldr (\iox ioxs -> iox >>= \x -> ioxs >>= \xs -> return (x : xs)) (return []) ios

sequence_' :: [IO a] -> IO ()
sequence_' ios = sequence' ios >> return ()
-- sequence_' = (>> return ()) . sequence'


-----------------------------------------------------

data Suit = Spades | Hearts | Diamonds | Clubs
     deriving (Show, Eq)

data Rank = Numeric Integer | Jack | Queen | King | Ace
          deriving (Show, Eq, Ord)

data Card = Card { rank :: Rank, suit :: Suit }
     deriving (Show)

genSuit :: Gen Suit
genSuit = elements [Spades, Hearts, Diamonds, Clubs]

genNumeric :: Gen Rank
genNumeric = choose (2, 10) >>= (return . Numeric)

genPicture :: Gen Rank
genPicture = elements [Jack, Queen, King, Ace]

genRank :: Gen Rank
genRank = frequency [(9, genNumeric), (4, genPicture)]

genCard :: Gen Card
genCard = do
  suit <- genSuit
  rank <- genRank
  return (Card rank suit)

genEven :: Gen Integer
genEven = arbitrary >>= (return . (*2))

-- testing the generator

validRank :: Rank -> Bool
validRank (Numeric n) = 2 <= n && n <= 10
validRank _ = True
prop_all_validRank = forAll genRank (\r -> collect r (validRank r))
