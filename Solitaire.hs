{-
Solitaire.hs
Stage One of the Graded Assignment

Vlad-Cristian Prisacariu
last modified: 25/10/2021
-}

module Solitaire where
    import Data.List
    import System.Random
    import Debug.Trace
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
            "Foundations  " ++ (show f) ++ "\n" ++ 
            "Columns" ++ "\n" ++
            --show columns on different lines
            (showColumns c) ++ 
            "Reserve  " ++ (show r) 
         where
             --show columns shows decks of columns
             -- on separate lines
             showColumns col = 
                 concatMap (showOnNewLine) col
                 where
                     showOnNewLine d = (show d) ++ "\n"

    initialLayout :: Board 
    initialLayout = EOBoard []
                    [[(Six,Clubs),(Seven,Diamonds),(Ace,Hearts),(Queen,Hearts),(King,Clubs),(Four,Spades)],
                    [(Five,Diamonds), (Queen,Clubs),(Three,Diamonds),(Five,Spades),(Six,Spades),(Seven,Hearts)],
                    [(King,Hearts),(Ten,Diamonds),(Seven,Spades),(Queen,Diamonds),(Five,Hearts),(Eight,Diamonds)],
                    [(Jack,Spades),(Six,Hearts),(Seven,Clubs),(Eight,Spades),(Ten,Clubs),(Queen,Clubs)],
                    [(Ace,Spades),(Eight,Clubs),(Ace,Diamonds),(King,Diamonds),(Jack,Hearts),(Four,Clubs)],
                    [(Two,Diamonds),(Three,Hearts),(Two,Hearts),(Ten,Hearts),(Six,Diamonds),(Jack,Clubs)],
                    [(Nine,Spades),(Four,Diamonds),(Nine,Clubs),(Nine,Hearts),(Three,Spades),(Ten,Spades)],
                    [(Two,Clubs),(Two,Spades),(Four,Hearts),(Nine,Diamonds),(King,Spades),(Eight,Hearts)]
                    ] [(Three,Clubs),(Ace,Clubs),(Five,Clubs),(Jack,Diamonds)]

    --eODeal takes a seed as a paramater
    -- and deals a randomly shuffled deck for eight off 
    eODeal :: Int -> Board
    eODeal n = EOBoard [] columns reserve where
        d = shuffle n pack
        reserve = take 4 d 
        d' = drop 4 d
        columns = splitIntoColumns d'
         where 
             splitIntoColumns [] = []
             splitIntoColumns deck = 
                 [(take 8 deck)] ++ splitIntoColumns (drop 8 deck)
    
    --TODO
    toFoundations :: Board -> Board 
    toFoundations (EOBoard f c r) = toFoundationsColumns [] (EOBoard f c r)

    --retry to place reserves to foundations every time we place a card from columns to foundations
    toFoundationsColumns :: Columns -> Board -> Board
    toFoundationsColumns aux (EOBoard f [] r) = EOBoard f aux r
    toFoundationsColumns aux (EOBoard f (c:cols) r) 
         | canBePlaced top f = toFoundationsColumns [] (EOBoard f' (aux ++ ((tail c):cols)) r')
         | otherwise = toFoundationsColumns (aux ++ [c]) (EOBoard f cols r)
         where
             top = head c
             (r',f') = toFoundationsReserve [] r (placeOnFoundation top f)


    toFoundationsReserve :: Reserve -> Reserve -> Foundations -> (Reserve,Foundations)
    toFoundationsReserve [] [] f = ([], f)
    toFoundationsReserve aux [] f = (aux, f)
    toFoundationsReserve aux (r:rs) f 
                |  canBePlaced r f = toFoundationsReserve [] (aux++rs) (placeOnFoundation r f)
                |  otherwise = toFoundationsReserve (aux ++ [r]) rs f
    
    --can a card be placed into the foundations
    canBePlaced :: Card -> Foundations -> Bool
    canBePlaced c [] 
                | isAce c = True
                | otherwise = False
    canBePlaced c (c':fnd)
                | isAce c = True
                | c == sCard c' = True
                | otherwise = canBePlaced c fnd

    placeOnFoundation :: Card -> Foundations -> Foundations
    placeOnFoundation c []
                        | isAce c = [c]
                        | otherwise = []
    placeOnFoundation c f@(c':fnd) 
                        | isAce c = c:f
                        | c == sCard c' = c:fnd
                        | otherwise = c':(placeOnFoundation c fnd)