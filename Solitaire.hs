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
    data Pip = Ace | Two | Three | Four | Five | Six | Seven
              | Eight | Nine | Ten | Jack | Queen
              | King deriving (Show,Eq, Enum)
    type Card = (Pip, Suit) 
    type Deck = [SCard] 

    --SPIDER SOLITAIRE TODO
    data SCard = Card Card Bool deriving Eq
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
        | p == King = (Ace, s) --if Ace - go back to two
        | otherwise = (succ p, s)

    --pCard returns predecessor card
    pCard :: Card -> Card
    pCard (p,s) 
        | p == Ace = (King, s)
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
    
    -- board 
    data Board = EOBoard Foundations Columns Reserve | SBoard Foundations Columns Stock 
    instance Eq  (Board) where
        (EOBoard f1 c1 r1) == (EOBoard f2 c2 r2) = f1 == f2 && c1 == c2 && r1 == r2
    -- --declaring custom instance of Show
    instance (Show Board) where 
        show b = boardShow b
            where 
            boardShow (EOBoard f c r) = "EOBoard " ++ "\n" ++
                "Foundations  " ++ (show f) ++ "\n" ++ 
                "Columns" ++ "\n" ++
                --show columns on different lines
                (showColumns c) ++ 
                "Reserve  " ++ (show r) ++ "\n\n" 

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
    initialLayout :: Board 
    initialLayout = EOBoard []
                    [[Card (Ace,Clubs) True,Card(Seven,Diamonds)True,Card(Ace,Hearts) True,Card(Queen,Hearts) True,Card(King,Clubs) True,Card(Four,Spades)True],
                    [Card(Five,Diamonds)True, Card(Queen,Clubs)True,Card(Three,Diamonds)True,Card(Five,Spades)True,Card(Six,Spades)True,Card(Seven,Hearts)True],
                    [Card(King,Hearts)True,Card(Ten,Diamonds)True,Card(Seven,Spades)True,Card(Queen,Diamonds)True,Card(Five,Hearts)True,Card(Eight,Diamonds)True],
                    [Card(Jack,Spades)True,Card(Six,Hearts)True,Card(Seven,Clubs)True,Card(Eight,Spades)True,Card(Ten,Clubs)True,Card(Queen,Clubs)True],
                    [Card(Ace,Spades)True,Card(Eight,Clubs)True,Card(Ace,Diamonds)True,Card(King,Diamonds)True,Card(Jack,Hearts)True,Card(Four,Clubs)True],
                    [Card(Two,Diamonds)True,Card(Three,Hearts)True,Card(Three,Clubs)True,Card(Ten,Hearts)True,Card(Six,Diamonds)True,Card(Jack,Clubs)True],
                    [Card(Nine,Spades)True,Card(Four,Diamonds)True,Card(Nine,Clubs)True,Card(Nine,Hearts)True,Card(Three,Spades)True,Card(Ten,Spades)True],
                    [Card(Two,Clubs)True,Card(Two,Spades)True,Card(Four,Hearts)True,Card(Nine,Diamonds)True,Card(King,Spades)True,Card(Eight,Hearts)True]
                    ] [Card(Two,Hearts)True,Card(Six,Clubs)True,Card(Five,Clubs)True,Card(Jack,Diamonds)True]

    -- initialSBoard :: Board 
    -- initialSBoard = SBoard [Card (King, Hearts) True]
    --                 [[Card (Eight,Diamonds) True, Card (Nine,Hearts) True],
    --                 [Card (Ace,Spades) True, Card (Two,Spades) True, Card (Three,Spades) True, Card (Four,Spades) True ,Card (Five,Spades) True, Card (Six,Clubs) True, 
    --                 Card (Seven,Clubs) True, Card (Eight,Clubs) True, Card (Nine,Clubs) True, Card (Ten,Diamonds) True, Card (Jack,Diamonds) True,Card (Queen,Diamonds) True, Card (King,Diamonds) True],
    --                 [Card (Seven,Clubs) True, Card (Eight, Diamonds) True, Card (Nine,Diamonds) True, Card (Ten,Diamonds) True, Card (Jack,Diamonds) True, Card (Queen, Diamonds) True, Card(King,Diamonds) True,
    --                 Card (Nine,Clubs) True, Card (Ten,Hearts) True, Card (Jack,Clubs) True],
    --                 [Card (Ace, Hearts) True, Card (Two,Hearts) True, Card (Three, Hearts) True, Card (Four, Hearts) True, Card (Five,Hearts) True, Card (Six,Diamonds) True, 
    --                 Card (Seven,Diamonds) True, Card (Queen,Clubs) True, Card (King, Hearts) True],
    --                 [Card (Two,Diamonds) True, Card (Three, Diamonds) True, Card (Four, Diamonds) True],
    --                 [Card (Jack,Clubs) True, Card (Queen,Clubs) True, Card (King,Clubs) True, Card (Two, Spades) True, Card (Three,Spades) True, Card (Four,Diamonds) True, Card (Five,Diamonds) True,
    --                 Card (Six,Diamonds) True, Card (Seven, Hearts) True, Card (Eight, Clubs) True]
    --                 ]
    --                 [[]]

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

    toFoundations :: Board -> Board 
    toFoundations (EOBoard f c r) = toFoundationsColumns [] (EOBoard f' c r') 
        where
            --start with trying to put the reserve to foundations
            (r',f') = toFoundationsReserve [] r f

    ---------- PART 2 FUNCTIONS ---------- 
    --getter functions that might come in handy
    getReserve :: Board -> Reserve
    getReserve (EOBoard f c r) = r

    getColumns :: Board -> Columns
    getColumns (EOBoard f c r) = c

    getFoundations :: Board -> Foundations
    getFoundations (EOBoard f c r) = f

    -- try to move every reserve card to some column - output every outcome board
    --fres and fcols store the reserves and columns (so far) at a certain point in the recursion
    findMovesReserves :: Reserve -> Columns -> Board -> [Board] 
    findMovesReserves fres fcols (EOBoard f [] r) = []
    findMovesReserves fres fcols (EOBoard f (c:cols) (rCard:res))
        | canMoveToColumn rCard c = [toFoundations (EOBoard f (fcols ++ ((moveToColumn rCard c):cols)) (fres ++ res))] 
            ++ findMovesReserves (fres ++ [rCard]) [] (EOBoard f (fcols ++ (c:cols)) res)
        | otherwise = findMovesReserves fres (fcols ++ [c]) (EOBoard f cols (rCard:res))

    -- move every first card from every column to reserve - output every outcome board
    -- fcols stores columns at a certain point in recursion
    findMovesColstoRes :: Columns -> Board -> [Board]
    findMovesColstoRes fcols (EOBoard f [] r) = []
    findMovesColstoRes fcols (EOBoard f (c:cols) r)
        | isReserveFull r = []
        | otherwise =  [toFoundations(EOBoard f (fcols ++ (tail c):cols) (moveToReserve card r))] ++ 
            findMovesColstoRes (fcols ++ [c]) (EOBoard f cols r)
        where
            card = head c
    
    -- NOTE TODO: if card under head card is movable to foundations -> then that's the best move
    -- try to move every first card from every column to every other column - output every outcome board 
    -- fcols contains all columns before current column in recursion
    findMovesColstoCols :: Columns -> Board -> [Board]
    findMovesColstoCols fcols (EOBoard f [] r) = []
    findMovesColstoCols fcols (EOBoard f (c:cols) r)
    -- (fcols ++ cols) - all columns except the one where the current card belongs
        | canMoveToAnyColumn card (fcols ++ cols) = [toFoundations (EOBoard f (map (moveToColumn card) (fcols ++ (c:cols))) r)] ++
            findMovesColstoCols (fcols ++ [c]) (EOBoard f cols r)
            -- [EOBoard f fcols ++ ((tail c):((moveToColumn card currentCol):(tail cols)) r] 
        | otherwise = findMovesColstoCols (fcols++[c]) (EOBoard f (cols) r)
        where
         card = head c
         currentCol = head fcols

    -- does the reserve have max number of cards?
    isReserveFull :: Reserve -> Bool
    isReserveFull reserve 
        | length reserve == 8 = True
        | otherwise = False

    --move a card to reserve
    moveToReserve :: SCard -> Reserve -> Reserve
    moveToReserve card reserve
        | isReserveFull reserve = reserve
        | otherwise = reserve ++ [card]
    
    --move a card to a column
    moveToColumn :: SCard -> Deck -> Deck
    moveToColumn card deck 
        | canMoveToColumn card deck = [card] ++ deck
        -- when moving a card from a column to another, 
        | card == (head deck) = tail deck 
        | otherwise = deck

    -- can this card be moved to any column?
    canMoveToAnyColumn :: SCard -> Columns -> Bool
    canMoveToAnyColumn _ [] = False
    canMoveToAnyColumn card (c:cols) 
        | canMoveToColumn card c = True
        | otherwise = canMoveToAnyColumn card cols

    --can this card be placed on this column?
    canMoveToColumn :: SCard -> Deck -> Bool
    canMoveToColumn (Card c b) []
        | isKing c = True --king can be placed on empty column
        | otherwise = False
    canMoveToColumn card (c:column) 
        | not (isAce c') && (pCard c' == card') = True
        | otherwise = False
        where
            c' = getCard c
            card' = getCard card
    
    -- can the nth card in a column can be moved to foundations?
    isNthCardMoveable :: Columns -> Foundations -> Int -> Bool
    isNthCardMoveable [] _ _ = False
    isNthCardMoveable (headcol:restcols) found nth 
      | length headcol <= (nth-1) = isNthCardMoveable (filter (not.null) restcols) found nth
      | canBePlaced (headcol !! (nth-1)) found = True
      | otherwise = isNthCardMoveable (filter (not.null) restcols) found nth
{-
NOTES
- possible moves:
    - moving card to reserve - DONE
    - moving card to foundations (to foundations) - DONE
    - moving card from columns on top of another card in columns - DONE
    - moving card from reserve on top of another card in columns - DONE
    - moving King on empty column - DONE
-}

    -- CHOOSE THE NEXT MOVE -- 
    --return a list of all possible board states after a single move
    findMoves :: Board -> [Board]
    findMoves b = findMovesColstoCols [] b ++ findMovesReserves [] [] b ++ 
        findMovesColstoRes [] b 
  
   -- findBestMoves :: Board -> [Board]
-- there is atleast once space in reserve, and the 2nd card in a column can be moved to foundation, then move first card in column to reserve

    chooseMove :: Board -> Board
    chooseMove b 
        | length (findMoves b) == 0 = EOBoard [] [] []
        | otherwise  = head (findMoves b)

    solve :: Board -> [Board]
    solve (EOBoard [] [] []) = []
    solve (b) 
      | move == EOBoard [] [] [] = []
      | otherwise = [move] ++ solve(move)
       where move = chooseMove b
    ---------- HELPER FUNCTIONS FOR NEXT MOVE CHOICE ----------
-- move king from res to empty column
-- 
   -- moveKingToEmptyColumn :: Board -> Board

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
                 ((toggleVisibility (head c)) : (tail c)) : (makeFstCrdVis crds)

            splitIntoColumns [] = []
            splitIntoColumns d = (splitFirst (take 24 d)) ++ (splitSecond (drop 24 d))
             where 
                 --first 4 columns have 6 cards each
                splitFirst [] = []
                splitFirst d = [(take 6 d)] ++ splitFirst (drop 6 d)
                -- last 6 columns have 5 cards each
                splitSecond [] = []
                splitSecond d = [(take 5 d)] ++ splitSecond (drop 5 d)
    
    ------------- TESTING BOARDS -------------

    testBoard :: Board 
    testBoard = EOBoard []
                    [[Card (Six,Clubs) True,Card(Seven,Diamonds)True,Card(Ace,Hearts) True,Card(Queen,Hearts) True,Card(King,Clubs) True,Card(Four,Spades)True],
                    [Card(Five,Diamonds)True, Card(Queen,Clubs)True,Card(Three,Diamonds)True,Card(Five,Spades)True,Card(Six,Spades)True,Card(Seven,Hearts)True],
                    [Card(King,Hearts)True,Card(Ten,Diamonds)True,Card(Seven,Spades)True,Card(Queen,Diamonds)True,Card(Five,Hearts)True,Card(Eight,Diamonds)True],
                    [Card(Jack,Spades)True,Card(Six,Hearts)True,Card(Seven,Clubs)True,Card(Eight,Spades)True,Card(Ten,Clubs)True,Card(Queen,Clubs)True],
                    [Card(Ace,Spades)True,Card(Eight,Clubs)True,Card(Ace,Diamonds)True,Card(King,Diamonds)True,Card(Jack,Hearts)True,Card(Four,Clubs)True],
                    [Card(Two,Diamonds)True,Card(Three,Hearts)True,Card(Two,Hearts)True,Card(Ten,Hearts)True,Card(Six,Diamonds)True,Card(Jack,Clubs)True],
                    [Card(Nine,Spades)True,Card(Three,Clubs)True,Card(Nine,Clubs)True,Card(Nine,Hearts)True,Card(Three,Spades)True,Card(Ten,Spades)True],
                    [Card(Two,Clubs)True,Card(Two,Spades)True,Card(Four,Hearts)True,Card(Nine,Diamonds)True,Card(King,Spades)True,Card(Eight,Hearts)True]
                    ] [Card(Five,Clubs)True,Card(Ace,Clubs)True,Card(Four,Diamonds)True,Card(Jack,Diamonds)True]
-- a board meant for testing, it does have repeated cards
    testBoard2 :: Board 
    testBoard2 = EOBoard []
                    [[Card (Six,Clubs) True,Card(Seven,Diamonds)True,Card(Ace,Hearts) True,Card(Queen,Hearts) True,Card(King,Clubs) True,Card(Four,Spades)True],
                    [Card(Queen,Diamonds)True, Card(Queen,Clubs)True,Card(Three,Diamonds)True,Card(Five,Spades)True,Card(Six,Spades)True,Card(Seven,Hearts)True],
                    [Card(King,Hearts)True,Card(Ten,Diamonds)True,Card(Seven,Spades)True,Card(Queen,Diamonds)True,Card(Five,Hearts)True,Card(Eight,Diamonds)True],
                    [Card(Eight,Clubs)True,Card(Six,Hearts)True,Card(Seven,Clubs)True,Card(Eight,Spades)True,Card(Ten,Clubs)True,Card(Queen,Clubs)True],
                    [Card(Ace,Spades)True,Card(Eight,Clubs)True,Card(Ace,Diamonds)True,Card(King,Diamonds)True,Card(Jack,Hearts)True,Card(Four,Clubs)True],
                    [Card(Two,Spades)True,Card(Three,Hearts)True,Card(Two,Hearts)True,Card(Ten,Hearts)True,Card(Six,Diamonds)True,Card(Jack,Clubs)True],
                    [Card(Ten,Diamonds)True,Card(Three,Clubs)True,Card(Nine,Clubs)True,Card(Nine,Hearts)True,Card(Three,Spades)True,Card(Ten,Spades)True],
                    [Card(Jack,Diamonds)True,Card(Two,Spades)True,Card(Four,Hearts)True,Card(Nine,Diamonds)True,Card(King,Spades)True,Card(Eight,Hearts)True]
                    ] [Card(Five,Clubs)True,Card(Ace,Clubs)True,Card(Four,Diamonds)True,Card(Jack,Diamonds)True]
  
    testColumns :: Columns
    testColumns = [[Card (Six,Clubs) True,Card(Seven,Diamonds)True,Card(Ace,Hearts) True,Card(Queen,Hearts) True,Card(King,Clubs) True,Card(Four,Spades)True],
                    [Card(Five,Diamonds)True, Card(Queen,Clubs)True,Card(Three,Diamonds)True,Card(Five,Spades)True,Card(Six,Spades)True,Card(Seven,Hearts)True],
                    [Card(King,Hearts)True,Card(Ten,Diamonds)True,Card(Seven,Spades)True,Card(Queen,Diamonds)True,Card(Five,Hearts)True,Card(Eight,Diamonds)True],
                    [Card(Jack,Spades)True,Card(Six,Hearts)True,Card(Seven,Clubs)True,Card(Eight,Spades)True,Card(Ten,Clubs)True,Card(Queen,Clubs)True],
                    [Card(Ace,Spades)True,Card(Eight,Clubs)True,Card(Ace,Diamonds)True,Card(King,Diamonds)True,Card(Jack,Hearts)True,Card(Four,Clubs)True],
                    [Card(Two,Diamonds)True,Card(Three,Hearts)True,Card(Two,Hearts)True,Card(Ten,Hearts)True,Card(Six,Diamonds)True,Card(Jack,Clubs)True],
                    [Card(Nine,Spades)True,Card(Three,Clubs)True,Card(Nine,Clubs)True,Card(Nine,Hearts)True,Card(Three,Spades)True,Card(Ten,Spades)True],
                    []
                    ]
    testReserve :: Reserve
    testReserve = [Card(Three,Clubs)True,Card(Ace,Clubs)True,Card(Five,Clubs)True,Card(Jack,Diamonds)True]

    testColumn :: Deck
    testColumn = [Card (Six,Clubs) True,Card(Seven,Diamonds)True,Card(Ace,Hearts) True,Card(Queen,Hearts) True,Card(King,Clubs) True,Card(Four,Spades)True]