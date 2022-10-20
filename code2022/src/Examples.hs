-- import Test.QuickCheck

dollarRate = 0.98546541

-- | convert euro to dollar
usd euro = euro * dollarRate

-- | convert dollar to euro
euro usd = usd / dollarRate

prop_EuroUSD x = euro (usd x) == x

price = 79

price' :: Double
price' = 79

price'' :: Num a => a
price'' = 79
