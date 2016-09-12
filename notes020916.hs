
-- anteckningar från föreläsning 02-09-2016

import Test.QuickCheck

--The Suit
data Suit = Spades | Clubs | Hearts | Diamonds
	deriving (Show,Eq)

--The Colour
data Colour = Black | Red
	deriving Show

colour :: Suit -> Colour
colour Spades 	= Black
colour Clubs	= Black
colour _		= Red

{-
Another function that does the same thing

colour s 	| s == Spades 	= Black
			| s == Clubs	= Black
			| otherwise		= Red
-}

data Rank = Numeric Int | Jack | Queen | King | Ace
	deriving	(Eq,Show)

legal :: Rank -> Bool
legal (Numeric n)	= n>1 && n<11 
legal _				= True


rankBeats :: Rank -> Rank -> Bool
rankBeats _		Ace 	= False
rankBeats Ace	_		= True
rankBeats _		King	= False
rankBeats King	_		= True
rankBeats _		Queen	= False
rankBeats Queen	_		= True
rankBeats _		Jack	= False
rankBeats Jack	_		= True
rankBeats (Numeric m) (Numeric n) = m > n

prop_rankBeats r1 r2 = rankBeats r1 r2 || rankBeats r2 r1 || r1 == r2

data Card = Card Rank Suit
  deriving (Eq,Show)
 
acespades :: Card
acespades = Card Ace Spades

rank :: Card -> Rank
suit :: Card -> Suit
rank (Card r _) = r
suit (Card _ s) = s

cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) =  s1 == s2 && rankBeats r1 r2

data Hand = Empty | Add Card Hand 
  deriving Show

egHand1 :: Hand 
egHand1 = Add acespades Empty

size :: Hand -> Int
size Empty     = 0
size (Add _ h) = 1 + size h

jacks :: Hand -> Int
jacks Empty = 0
jacks (Add c h) | rank c == Jack = 1 + jacks h
                | otherwise      = jacks h





