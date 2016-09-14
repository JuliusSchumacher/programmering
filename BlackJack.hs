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


aceHand :: Hand
aceHand = Add (Card Ace Hearts) (Add (Card Ace Diamonds) (Add (Card Ace Clubs) (Add (Card Ace Spades) Empty)))

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
value h         | trueValue h > 21  = trueValue h - ((numberOfAces h) * 10)
                | otherwise         = trueValue h

trueValue :: Hand -> Integer
trueValue Empty = 0
trueValue (Add c h) = valueCard c + trueValue h

gameOver :: Hand -> Bool
gameOver h = value h > 21

winner :: Hand -> Hand -> Player
winner gh bh    | gameOver gh           = Bank
             	| gameOver bh           = Guest
		        | value gh == value bh  = Bank
		        | value gh < value bh   = Bank
		        | otherwise             = Guest 


-- Task C

(<+) :: Hand -> Hand -> Hand
Empty <+ bot     = bot
top <+ bot =  (top <++ Empty) <++ bot
    where 
    -- Helper: used to invert top hand
    (<++) :: Hand -> Hand -> Hand
    Empty <++ bot     = bot
    (Add c h) <++ bot = h <++ (Add c bot)


prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2



-- Task D

fullDeck :: Hand
fullDeck = suitCards Diamonds <+ suitCards Hearts <+ suitCards Clubs <+ suitCards Spades
    where
    --Helper: used to obfuscate rankList
        suitCards :: Suit -> Hand
        suitCards s = rankList Empty s
            where
            --helper: Creates a Hand from all the cards from a suit
                rankList :: Hand -> Suit -> Hand
                rankList h s                        | h == Empty                                                = rankList (Add(Card (Numeric 2) s) h) s
                rankList (Add (Card r s') h) s      | valueCard (Card r s) > 1  && valueCard (Card r s) < 10    = rankList (Add (Card (Numeric ((valueCard(Card r s)) + 1)) s) (Add (Card r s) h)) s 
                                                    | r == Numeric 10                                           = rankList (Add (Card Jack s) (Add (Card r s) h)) s
                                                    | r == Jack                                                 = rankList (Add (Card Queen s) (Add (Card r s) h)) s
                                                    | r == Queen                                                = rankList (Add (Card King s) (Add (Card r s) h)) s
                                                    | r == King                                                 = rankList (Add (Card Ace s) (Add (Card r s) h)) s
                                                    | r == Ace                                                  = (Add (Card r s) h)


-- Task E

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty h = error "draw: The deck is empty."
draw (Add c h') h = (h', Add c h)


-- Task F

playBank :: Hand -> Hand
playBank d = playHelper d Empty
    where
        playHelper :: Hand -> Hand -> Hand
        playHelper Empty h = error "playBank: The deck is empty"
        playHelper (Add c d) h  | value h >= 16 = h
                                | otherwise     = playHelper d (Add c h)



