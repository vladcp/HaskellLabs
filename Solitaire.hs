{-
Solitaire.hs
Stage One of the Graded Assignment

Vlad-Cristian Prisacariu
last modified: 21/10/2021
-}

module Solitaire where
    import Data.List
    import System.Random

    -- datatypes - basics
    data Suit = Hearts | Clubs | Spades | Diamonds deriving (Show, Eq, Enum)
    data Pip = Two | Three | Four | Five | Six | Seven
              | Eight | Nine | Ten | Jack | Queen
              | King | Ace deriving (Show,Eq, Enum)
    type Card = (Pip, Suit) 
    type Deck = [Card] 

    -- constants and untilities
    -- Suit and Pip must derive Enum for this
    pack :: Deck 
    pack = [(pip, suit) | suit <- [Hearts .. Diamonds], pip <- [Two .. Ace]]

    -- sCard returns successor card
    sCard :: Card -> Card
    sCard (p, s) 
        | p == Ace = (Two, s) --if Ace - go back to two
        | otherwise = (succ p, s)

    --pCard returns predecessor card
    pCard :: Card -> Card
    pCard (p,s) 
        | p == Two = (Ace, s)
        | otherwise = (pred p, s)

    --check if a card is an Ace
    isAce :: Card -> Bool
    isAce (p,s) = p == Ace

    --check if a card is an Ace
    isKing :: Card -> Bool
    isKing (p,s) = p == King

    -- shuffle a deck of cards
    cmp (x1,y1) (x2,y2) = compare y1 y2
    shuffle :: Int -> Deck -> Deck
    shuffle n d = [c | (c,m) <- sortBy cmp (zip d (randoms (mkStdGen n) :: [Int]))]

    -- datatypes - eight-off board  
    type Foundations = [Deck]
    type Columns = [Deck]
    type Reserve = [Deck]
    data Board = EOBoard [Foundations, Columns, Reserve] deriving (Show)

    initialLayout :: EOBoard 
    initialLayout = 

    testFunc :: [Card] -> Int
    testFunc c = length(pack)

