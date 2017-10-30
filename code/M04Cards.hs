import Test.QuickCheck

data Suit = Spades | Hearts | Diamonds | Clubs
     deriving (Show, Eq)

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
          deriving (Show, Eq, Ord)

-- rankBeats r1 r2 returns True, if r1 beats r2
rankBeats :: Rank -> Rank -> Bool
rankBeats _  Ace    = False
rankBeats Ace _     = True
rankBeats _ King    = False
rankBeats King _    = True
rankBeats _  Queen  = False
rankBeats Queen _   = True
rankBeats _ Jack    = False
rankBeats Jack _    = True
rankBeats (Numeric n1) (Numeric n2) = n1 > n2
--          ^^ pattern match on constructor
--             yields its argument

rankBeats' r1 r2 = r1 > r2

prop_rankBeats :: Rank -> Rank -> Bool
prop_rankBeats r1 r2 = rankBeats r1 r2 == rankBeats' r1 r2

rankEquals :: Rank -> Rank -> Bool
rankEquals Ace Ace = True
rankEquals King King = True
rankEquals Queen Queen = True
rankEquals Jack Jack = True
rankEquals (Numeric m) (Numeric n) = m == n
rankEquals _ _ = False

{- this is a block comment {- that can be nested -}
-}

-- |playing cards
{-
data Card = Card Rank Suit
  deriving (Show)
rank :: Card -> Rank
rank (Card r s) = r
suit :: Card -> Suit
suit (Card r s) = s
-}


data Card = Card { rank :: Rank, suit :: Suit }
     deriving (Show)

cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = suit givenCard == suit c
                        && rankBeats (rank givenCard)
                                     (rank c)
aceOfSpades = Card Ace Spades
tenOfHearts = Card (Numeric 10) Hearts
queenOfHearts = Card Queen Hearts
jackOfClub = Card Jack Clubs

data Hand = Last Card | Next Card Hand
  deriving (Show)

-- choose a card from the hand that beats the given card if possible
-- but it does not follow suit!
chooseCard :: Card -> Hand -> Card
chooseCard c (Last card) = card
chooseCard c (Next card hand) =
    if cardBeats card c
    then card
    else chooseCard c hand

exampleHand = Next jackOfClub (Next queenOfHearts (Next  tenOfHearts (Last aceOfSpades)))
tenOfClubs = Card (Numeric 10) Clubs

-- the Maybe type
data Maybe' a = Nothing' | Just' a

-- like chooseCard, but follow suit
chooseCard' :: Card -> Hand -> Card
chooseCard' c h =
    chooseCardFollowing c h Nothing

-- | take given card, current hand, and maybe a card of same suit as given card
chooseCardFollowing :: Card -> Hand -> Maybe Card -> Card
chooseCardFollowing c (Last card) Nothing = card
chooseCardFollowing c (Last card) (Just cardSameSuit) =
    if cardBeats card c
    then card
    else cardSameSuit
chooseCardFollowing c (Next card hand) m =
    if cardBeats card c
    then card
    else if suit c == suit card
    then chooseCardFollowing c hand (Just card)
    else chooseCardFollowing c hand m

{- alternative way of writing this:
    else chooseCardFollowing c hand
           (if suit c == suit card then Just card else m)
-}
