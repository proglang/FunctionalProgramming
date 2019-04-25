import Test.QuickCheck

dollarRate = 1.3671

-- |convert euros to dollar
usd euros = euros * dollarRate

-- |convert dollars to euro
euro usds = usds / dollarRate

-- |bad property
prop_EuroUSD x =
  euro (usd x) == x

-- |next try
prop_EuroUSD1 x =
  abs (euro (usd x) - x) < 10e-15

-- |final try
prop_EuroUSD2 x =
  euro (usd x) ~== x

(~==) :: Double -> Double -> Bool
x ~== y = abs(x - y) <= 10e-15 * abs x

price :: Double
price = 79

