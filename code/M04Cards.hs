
data Suit = Spades | Hearts | Diamonds | Clubs
     deriving (Show)

data Color = Black | Red
  deriving (Show)

-- Define a color function by pattern matching
color :: Suit -> Color
color Spades = Black
color Clubs = Black
color Diamonds = Red
color Hearts = Red

data Rank = Numeric Integer | Jack | Queen
          | King | Ace
          deriving (Show)
          