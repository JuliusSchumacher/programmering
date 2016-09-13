-- Lab 2, Black Jack

module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck


-- Task A

hand2 = Add (Card (Numeric 2) Hearts)
          (Add (Card Jack Spades) Empty)  

{-
 size hand2
  = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2
-}

aCard1 :: Card
aCard1 = Card Queen Diamonds

aCard2 :: Card
aCard2 = Card (Numeric 2) Spades

aHand :: Hand
aHand = Add (Card Ace Diamonds) (Add (Card Ace Spades)(Add aCard2 (Empty)))

aHand1 :: Hand
aHand1 = Add (Card Ace Hearts) (Add (Card Ace Clubs)(Add aCard1 (Empty)))

hand3 = Add (Card (Numeric 2) Hearts)
          (Add (Card (Numeric 8) Spades) 
		  (Add (Card Ace Clubs ) Empty))

-- Task B 

valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank King = 10
valueRank Queen = 10
valueRank Jack = 10
valueRank (Numeric n) = n  

valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h) = numberOfAces h 

value :: Hand -> Integer
value Empty = 0
value (Add c h) | valueCard c + value h > 21 
                            = value h + valueCard c - numberOfAces (Add c h) * 10
                | otherwise = valueCard c + value h

gameOver :: Hand -> Bool
gameOver h = value h > 21

winner :: Hand -> Hand -> Player
winner gh bh 	| gameOver gh           = Bank
             	| gameOver bh           = Guest
		| value gh == value bh  = Bank
		| value gh < value bh   = Bank
		| otherwise             = Guest 


(<+) :: Hand -> Hand -> Hand
Empty <+ bot                                    = bot
(Add c h) <+ bot        | h == Empty            = Add c bot
                        | otherwise             = h <+ (Add c bot)



