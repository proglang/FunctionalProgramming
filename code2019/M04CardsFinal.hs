data Suit = Spades | Hearts | Diamonds | Clubs
          deriving (Show, Eq, Ord)
          
data Color = Black | Red
             deriving (Show, Eq, Ord)
           
-- Define a color function by pattern matching
color :: Suit -> Color
color Spades = Black
color Hearts = Red
color Diamonds = Red
color Clubs = Black
          
data Rank = Numeric Integer | Jack | Queen
          | King | Ace
            deriving (Show,Eq,Ord)

-- rankBeats r1 r2 returns True, if r1 beats r2
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 >= r2


data Card = Card { rank :: Rank, suit :: Suit }
          deriving (Show,Eq,Ord)
          
cardBeats :: Card -> Card -> Bool
cardBeats givenCard c =
    ((suit givenCard) == (suit c))
    &&
    (rankBeats (rank givenCard) (rank c))

aceOfSpades = Card Ace Spades 
tenOfHearts = Card (Numeric 10) Hearts
queenOfHearts = undefined
jackOfClub = undefined

data Hand = Last Card | Next Card Hand
          deriving (Show)
          
-- choose a card from the hand that beats the given card if possible
-- but it does not follow suit!
chooseCard :: Card -> Hand -> Card
chooseCard c (Last c2) = c2
chooseCard c (Next c2 h) | cardBeats c2 c = c2
chooseCard c (Next c2 h) = chooseCard c h


                           
lowestCard :: Hand -> Card
lowestCard (Next c h) = lowestCard' c h
lowestCard (Last c) = c
              
lowestCard' :: Card -> Hand -> Card
lowestCard' cmin (Last c) = min cmin c
lowestCard' cmin (Next c h) = lowestCard' (min cmin c) h
                           
hand :: Hand
hand = Next  tenOfHearts (Last aceOfSpades)


data Foo = Foo | Bar | Baz
         deriving (Eq)


data List a = Empty | Cons a (List a)
            deriving (Show)
