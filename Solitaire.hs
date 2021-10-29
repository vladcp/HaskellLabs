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
    type Deck = [SCard] 

    --SPIDER SOLITAIRE TODO
    data SCard = Card Card Bool  
    instance (Show SCard) where
        show (Card c visible) = if visible then show c else "<unknown>"
    
    --return only the (pip,suit) of a card, regardless of visibility
    getCard :: SCard -> Card
    getCard (Card c v) = c

    toggleVisibility :: SCard -> SCard
    toggleVisibility (Card c v) = Card c (not v)
    -- Suit and Pip must derive Enum for this
    -- we can choose to have cards facing up or facing down
    -- True -> all cards are visible
    pack :: Bool -> Deck 
    pack visible = [Card (pip, suit) visible | suit <- [Hearts .. Diamonds], pip <- [Two .. Ace]]

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
    type Foundations = [SCard] 
    type Columns = [Deck]
    type Reserve = [SCard]
    data Board = EOBoard Foundations Columns Reserve | SBoard Foundations Columns Stock

    -- datatypes - Spider Solitaire
    data Stock = Stock Deck
    instance (Show Stock) where
        show (Stock d) = (show x) ++ " Deals remaining" 
                    where  x = (length d) `div` 10
    --     -- foundations can be the same
    --     -- there are 10 columns, and some cards are not visible
    --     -- 6 cards in first 4 piles, 5 cards in the rest 6 piles
    --     --stock has 50 cards initially, and you can deal 10 cards from the stock at any time
    --             --if there are no empty columns

    -- --declaring custom instance of Show
    instance (Show Board) where 
        show b = boardShow b
            where 
            boardShow (EOBoard f c r) = "EOBoard " ++ "\n" ++
                "Foundations  " ++ (show f) ++ "\n" ++ 
                "Columns" ++ "\n" ++
                --show columns on different lines
                (showColumns c) ++ 
                "Reserve  " ++ (show r) 

            boardShow (SBoard f c s) = "SBoard " ++ "\n" ++ 
                "Foundations " ++ (show f) ++ "\n" ++ 
                "Columns" ++ "\n" ++ 
                (showColumns c) ++ 
                "Stock " ++ (show s)
                --show columns shows decks of columns
                -- on separate lines
            showColumns col = concatMap (showOnNewLine) col
                where
                    showOnNewLine d = (show d) ++ "\n"   

    -- --initial layout from the assignment brief
    -- initialLayout :: Board 
    -- initialLayout = EOBoard []
    --                 [[(Six,Clubs),(Seven,Diamonds),(Ace,Hearts),(Queen,Hearts),(King,Clubs),(Four,Spades)],
    --                 [(Five,Diamonds), (Queen,Clubs),(Three,Diamonds),(Five,Spades),(Six,Spades),(Seven,Hearts)],
    --                 [(King,Hearts),(Ten,Diamonds),(Seven,Spades),(Queen,Diamonds),(Five,Hearts),(Eight,Diamonds)],
    --                 [(Jack,Spades),(Six,Hearts),(Seven,Clubs),(Eight,Spades),(Ten,Clubs),(Queen,Clubs)],
    --                 [(Ace,Spades),(Eight,Clubs),(Ace,Diamonds),(King,Diamonds),(Jack,Hearts),(Four,Clubs)],
    --                 [(Two,Diamonds),(Three,Hearts),(Two,Hearts),(Ten,Hearts),(Six,Diamonds),(Jack,Clubs)],
    --                 [(Nine,Spades),(Four,Diamonds),(Nine,Clubs),(Nine,Hearts),(Three,Spades),(Ten,Spades)],
    --                 [(Two,Clubs),(Two,Spades),(Four,Hearts),(Nine,Diamonds),(King,Spades),(Eight,Hearts)]
    --                 ] [(Three,Clubs),(Ace,Clubs),(Five,Clubs),(Jack,Diamonds)]

    initialSBoard :: Board 
    initialSBoard = SBoard [Card (King, Hearts) True]
                    [[Card (Eight,Diamonds) True, Card (Nine,Hearts) True],
                    [Card (Ace,Spades) True, Card (Two,Spades) True, Card (Three,Spades) True, Card (Four,Spades) True ,Card (Five,Spades) True, Card (Six,Clubs) True, 
                    Card (Seven,Clubs) True, Card (Eight,Clubs) True, Card (Nine,Clubs) True, Card (Ten,Diamonds) True, Card (Jack,Diamonds) True,Card (Queen,Diamonds) True, Card (King,Diamonds) True],
                    [Card (Seven,Clubs) True, Card (Eight, Diamonds) True, Card (Nine,Diamonds) True, Card (Ten,Diamonds) True, Card (Jack,Diamonds) True, Card (Queen, Diamonds) True, Card(King,Diamonds) True,
                    Card (Nine,Clubs) True, Card (Ten,Hearts) True, Card (Jack,Clubs) True],
                    [Card (Ace, Hearts) True, Card (Two,Hearts) True, Card (Three, Hearts) True, Card (Four, Hearts) True, Card (Five,Hearts) True, Card (Six,Diamonds) True, 
                    Card (Seven,Diamonds) True, Card (Queen,Clubs) True, Card (King, Hearts) True],
                    [Card (Two,Diamonds) True, Card (Three, Diamonds) True, Card (Four, Diamonds) True],
                    [Card (Jack,Clubs) True, Card (Queen,Clubs) True, Card (King,Clubs) True, Card (Two, Spades) True, Card (Three,Spades) True, Card (Four,Diamonds) True, Card (Five,Diamonds) True,
                    Card (Six,Diamonds) True, Card (Seven, Hearts) True, Card (Eight, Clubs) True]
                    ]
                    [[]]

    ---------- EIGHT OFF FUNCTIONS ----------
    --eODeal takes a seed as a paramater
    -- and deals a randomly shuffled deck for eight off 
    eODeal :: Int -> Board
    eODeal n = EOBoard [] columns reserve where
        d = shuffle n (pack True) --True means all cards are visible
        reserve = take 4 d 
        columns = splitIntoColumns (drop 4 d)
         where 
             splitIntoColumns [] = []
             splitIntoColumns deck = 
                 [(take 6 deck)] ++ splitIntoColumns (drop 6 deck)
    
    toFoundations :: Board -> Board 
    toFoundations (EOBoard f c r) = toFoundationsColumns [] (EOBoard f' c r') --start with trying the reserve
        where
            (r',f') = toFoundationsReserve [] r f
    --retry to place reserves to foundations every time we place a card from columns to foundations
    toFoundationsColumns :: Columns -> Board -> Board
    toFoundationsColumns aux (EOBoard f [] r) = EOBoard f aux r
    toFoundationsColumns aux (EOBoard f (c:cols) r) 
        -- if a column card can be placed, 
         | canBePlaced top f = toFoundationsColumns [] (EOBoard f' (aux ++ ((tail c):cols)) r') 
         | otherwise = toFoundationsColumns (aux ++ [c]) (EOBoard f cols r)
         where
             top = head c
             (r',f') = toFoundationsReserve [] r (placeOnFoundation top f) 

            -- f' -> foundation after we placed all possible reserves
    toFoundationsReserve :: Reserve -> Reserve -> Foundations -> (Reserve,Foundations)
    toFoundationsReserve [] [] f = ([], f)
    toFoundationsReserve aux [] f = (aux, f)
    toFoundationsReserve aux (r:rs) f 
                |  canBePlaced r f = toFoundationsReserve [] (aux++rs) (placeOnFoundation r f)
                |  otherwise = toFoundationsReserve (aux ++ [r]) rs f

    --can a card be placed into the foundations?
    canBePlaced :: SCard -> Foundations -> Bool
    canBePlaced card [] 
                | isAce c = True
                | otherwise = False
                where c = getCard card
    canBePlaced card (card':fnd)
                | isAce c = True
                | c == sCard c' = True
                | otherwise = canBePlaced card fnd
                where 
                    c = getCard card
                    c' = getCard card'

    -- place a card to the foundations, return the foundations
    placeOnFoundation :: SCard -> Foundations -> Foundations
    placeOnFoundation card []
                        | isAce c = [card]
                        | otherwise = []
                        where c = getCard card
    placeOnFoundation card f@(card':fnd) 
                        | isAce c = card:f
                        | c == sCard c' = card:fnd
                        | otherwise = card':(placeOnFoundation card fnd)
                        where 
                            c = getCard card
                            c' = getCard card'

    ---------- SPIDER SOLITAIRE FUNCTIONS ----------
    sDeal :: Int -> Board
    sDeal n = SBoard [] c s where
        -- start with a facing down deck (containing 2 packs)
        d = (shuffle n (pack False)) ++ (shuffle n (pack False))
        s = Stock (take 50 d)
        c = makeFstCrdVis (splitIntoColumns (drop 50 d))
         where 
            makeFstCrdVis [] = []
            --make first card of each column visible
            makeFstCrdVis (c:crds) =
                 ((toggleVisibility (head c)) : (drop 1 c)) : (makeFstCrdVis crds)

            splitIntoColumns [] = []
            splitIntoColumns d = (splitFirst (take 24 d)) ++ (splitSecond (drop 24 d))
             where 
                 --first 4 columns have 6 cards each
                splitFirst [] = []
                splitFirst d = [(take 6 d)] ++ splitFirst (drop 6 d)
                -- last 6 columns have 5 cards each
                splitSecond [] = []
                splitSecond d = [(take 5 d)] ++ splitSecond (drop 5 d)