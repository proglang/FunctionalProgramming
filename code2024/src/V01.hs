import Test.QuickCheck

dollarRate = 0.91691801

-- | convert EUR to USD
usd euros = euros * dollarRate

-- | convert USD to EUR
euro usds = usds / dollarRate

-- a property test
prop_EuroUSD x = x < 0 || euro (usd x) ~== x

-- | nearly equals
x ~== y = abs (x - y) < 10e-15

-- price :: Num a => a
price = 79
