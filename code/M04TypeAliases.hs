type Name = String
type Title = String
type Year = Int
type Age = Int

type User = (Name,    Year)
--           ^ name   ^ Year of birth 
type Film = (Title, Age)
--                  ^ fsk           
type Purchase = (Name, Title, Year) -- <---+
--             ^ user name ^ item name     ^ date of purchase
users :: [User]
users = undefined

useful_function :: (String, Int) -> String
useful_function (str, i) = take i str
