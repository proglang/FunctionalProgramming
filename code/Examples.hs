import Test.QuickCheck

dollarRate = 1.3671

-- |convert euros to usd
usd euros = euros * dollarRate

-- |convert usd to eur
euro usds = usds / dollarRate

-- |euro and usd are inverses
prop_EuroUSD x = euro (usd x) == x

-- |almost equals
x ~== y = abs (x - y) <= abs x * 10E-15

-- |euro and usd are almost inverses
prop_EuroUSD' x = euro (usd x) ~== x

price = 79

price' :: Double
price' = 79

