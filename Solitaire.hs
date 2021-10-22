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
    type Foundations = [Card] 
    type Columns = [Deck]
    type Reserve = [Card]
    data Board = EOBoard Foundations Columns Reserve 


    --declaring custom instance of Show
    instance (Show Board) where 
        show (EOBoard f c r) = "EOBoard " ++ "\n" ++
            "Foundations" ++ (show f) ++ "\n" ++ 
            "Columns" ++ "\n" ++
            --show columns on different lines
            show c ++ 
            "Reserve " ++ (show r) 

    initialLayout :: Board 
    initialLayout = EOBoard [(Two,Clubs), (Two, Spades)] [[(Seven,Diamonds),(Ace,Hearts),(Queen,Hearts),(King,Clubs),(Four,Spades)],
                    [(Five,Diamonds),(Queen,Spades),(Three,Diamonds),(Five,Spades),(Six,Spades),(Seven,Hearts)],
                    [(King,Hearts),(Ten,Diamonds),(Seven,Spades),(Queen,Diamonds),(Five,Hearts),(Eight,Diamonds)]] [(Two,Hearts),(Six,Clubs),(Five,Clubs),(Jack,Diamonds)]

    testFunc :: [Card] -> Int
    testFunc c = length(pack)

