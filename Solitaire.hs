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
              | King deriving (Show, Eq, Enum)
    type Card = (Pip, Suit) 
    type Deck = [SCard] 

    --SPIDER SOLITAIRE
    data SCard = Card Card Bool 
    instance (Eq SCard) where
        (Card card1 b1) == (Card card2 b2) = card1 == card2
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
    pack visible = [Card (pip, suit) visible | suit <- [Hearts .. Diamonds], pip <- [Ace .. King]]

    -- sCard returns successor card
    sCard :: SCard -> SCard
    sCard (Card (p, s) b)
        | p == King = Card (Ace, s) b--if King, go to Ace
        | otherwise = Card (succ p, s) b
        
    --pCard returns predecessor card
    pCard :: SCard -> SCard
    pCard (Card (p,s) b)
        | p == Ace = Card (King, s) b
        | otherwise = Card (pred p, s) b

    --check if a card is an Ace
    isAce :: SCard -> Bool
    isAce (Card (p,s) b) = p == Ace

    --check if a card is an Ace
    isKing :: SCard -> Bool
    isKing (Card (p,s) b) = p == King

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

     --can a card be placed into the foundations?
    canBeMovedToFoundation :: SCard -> Foundations -> Bool
    canBeMovedToFoundation card f 
        | isAce card = True
        | not (null (filter (\x -> x == pCard card) f)) = True
        | otherwise = False

    -- place a card to the foundations, return the foundations
    placeOnFoundation :: SCard -> Foundations -> Foundations
    placeOnFoundation card f 
        | isAce card = f ++ [card]
        | otherwise = map (\f -> if (not (isKing f) && ((sCard f) == card)) then (sCard f) else f) f
        
    toFoundationsColumns :: Board -> Board
    toFoundationsColumns b@(EOBoard _ [] _) = b
    toFoundationsColumns b@(EOBoard f c r)
        | not (null movableCards) = (EOBoard (foldr (\x f -> placeOnFoundation x f) f movableCards) 
           (filter (not.null) (map removeHeads c)) r)
        | otherwise = b
         where 
             colHeads = map head (filter (not.null) c)
             movableCards = (filter (\x -> canBeMovedToFoundation x f) colHeads)
             removeHeads [] = []
             removeHeads col@(c:cs) = if (elem c movableCards) then cs else col

    -- move all cards from reserve to foundations (that can be moved) in one go
    toFoundationsReserve :: Board -> Board
    toFoundationsReserve b@(EOBoard _ _ []) = b
    toFoundationsReserve b@(EOBoard f c reserve) 
        | not (null movableCards) = (EOBoard (foldr (\x f -> placeOnFoundation x f) f movableCards) c (filter (\x -> not (elem x movableCards)) reserve))
        | otherwise = b
        where
            movableCards = (filter (\x -> canBeMovedToFoundation x f) reserve)

    toFoundations :: Board -> Board
    toFoundations board 
        | board /= toFRes = toFoundations toFRes
        | board /= toFCol = toFoundations toFCol
        | otherwise = board
         where 
             toFRes = toFoundationsReserve board
             toFCol = toFoundationsColumns board 

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

    -- ===================================== --
---------- HELPER FUNCTIONS FOR NEXT MOVE CHOICE ----------

     -- does the reserve have max number of cards?
    isReserveFull :: Reserve -> Bool
    isReserveFull reserve 
        | length reserve == 8 = True
        | otherwise = False

    --given a card in the reserve, remove it from reserve
    removeFromRes :: SCard -> Board -> Board
    removeFromRes card (EOBoard f c r) = EOBoard f c (delete card r)

    -- given a card, it will remove it from the columns
    -- this will mainly be used to remove a card from column heads
    removeFromCol :: SCard -> Board -> Board
    removeFromCol card (EOBoard f cols r) = (EOBoard f newCols r)
        where
            newCols = [filter (/= card) c | c <- cols] --(smart) filter

    --is there any empty column on the board
    isAnyEmptyCol :: Board -> Bool
    isAnyEmptyCol (EOBoard f c r) = (length c) < 8

    --move a card to reserve
    moveToReserve :: SCard -> Reserve -> Reserve
    moveToReserve card reserve
        | isReserveFull reserve = reserve
        | otherwise = reserve ++ [card]
    
    --move a card to a column
    moveToColumn :: SCard -> Deck -> Deck
    moveToColumn card [] = [card]
    moveToColumn card deck 
        | canMoveToColumn card deck = [card] ++ deck
        | card == (head deck) = tail deck --if it's the head card, delete it
        | otherwise = deck

    -- can this card be moved to any column?
    canMoveToAnyColumn :: SCard -> Columns -> Bool
    canMoveToAnyColumn _ [] = False
    canMoveToAnyColumn card (c:cols) 
        | canMoveToColumn card c = True
        | otherwise = canMoveToAnyColumn card cols

    --can this card be placed on this column?
    canMoveToColumn :: SCard -> Deck -> Bool
    canMoveToColumn card []
        | isKing card = True --only a king can be placed on empty column
        | otherwise = False
    canMoveToColumn card (c:column) 
        | not (isAce c) && (pCard c == card) = True
        | otherwise = False
    
    -- can the nth card in a column be moved to foundations?
    -- if yes, return first card of the column that contains that card
    isNthCardMoveable :: Columns -> Foundations -> Int -> (SCard,Bool)
    isNthCardMoveable [] _ _ = ((Card (Three,Spades) False), False)
    isNthCardMoveable (headcol:restcols) found nth 
      | length headcol <= (nth) = isNthCardMoveable (filter (not.null) restcols) found nth
      | canBeMovedToFoundation (headcol !! nth) found = ((head headcol),True)
      | otherwise = isNthCardMoveable (filter (not.null) restcols) found nth

    -- move to reserve the head of the column whose nth card can be moved to foundations
    moveNthMvblColHeadRes :: Board -> Int -> Board 
    moveNthMvblColHeadRes board@(EOBoard f cols res) nth = 
        EOBoard f (getColumns(removeFromCol nthCard board)) (res ++ [nthCard])
        where nthCard = fst (isNthCardMoveable cols f nth)

    --return first King in res, return random card and False otherwise
    getKingRes :: Reserve -> (SCard,Bool) 
    getKingRes [] = (Card (Three,Spades) False ,False) -- if no king, return a useless face down card
    getKingRes (r:res) 
        | isKing r = (r,True)
        | otherwise = getKingRes res
    
    getKingColHead :: Columns -> (SCard, Bool)
    getKingColHead [] = (Card (Three,Spades) False ,False) -- if no king, return a useless face down card
    getKingColHead (c:cols)
        | isKing (head c) = (head c, True)
        | otherwise = getKingColHead cols
-- ===================================== --

-- ===================================== --
    -- move king to empty col, but move the best king. try to move one from reserves first, to free up space
    moveKingResToEmptyCol :: Board -> Board
    moveKingResToEmptyCol board@(EOBoard f cols res) =
        (EOBoard f (cols ++ [[king]]) (getReserve (removeFromRes king board)))
        where king = fst (getKingRes res)
        -- | getEmptyCol cols == Nothing = board
        -- | snd (getKingRes res) == False = board

    -- move king from head of a col to empty column
    -- note: if card under this king can be moved to foundations, then choose this king (?)
    moveKingColToEmptyCol :: Board -> Board
    moveKingColToEmptyCol board@(EOBoard f cols r) =
        (EOBoard f newCols r)
        where 
            king = fst (getKingColHead cols)
            newCols = (getColumns (removeFromCol king board)) ++ [[king]]

    -- CHOOSE THE NEXT MOVE -- 
    --return a list of all possible board states after a single move
    findMoves :: Board -> [Board]
    findMoves b = findMovesColstoCols [] b ++ findMovesReserves [] [] b ++ 
        findMovesColstoRes [] b 
    
    findMoves' :: Board ->[Board]
    findMoves' board@(EOBoard f cols res) =
        filter (/= (EOBoard [] [] []))
        [(if (board /= (toFoundations board)) then (toFoundations board) else (EOBoard [] [] [])),
        -- if the second card of a column can move to foundations, move the card on top of it to reserve
         (if ((not (isReserveFull res)) && (snd (isNthCardMoveable cols f 1))) then (moveNthMvblColHeadRes board 1) else (EOBoard [] [] [])),
         -- move king from reserves to empty column, if possible
         (if ((isAnyEmptyCol board) && (snd (getKingRes res))) then (moveKingResToEmptyCol board) else (EOBoard [] [] [])),
         -- move king from column to an empty column, if possible
         (if ((isAnyEmptyCol board) && (snd (getKingColHead cols))) then (moveKingColToEmptyCol board) else (EOBoard [] [] [])) 
         ]

    maybeTo :: Maybe a -> a
    maybeTo (Just x) = x 

    chooseMove :: Board -> Maybe Board
    chooseMove b 
        | length (findMoves' b) >= 1 = Just ((head (findMoves' b)))
        | otherwise = Nothing 

    haveWon :: Board -> Bool
    haveWon (EOBoard f c r) = (null c) && (null r) 

    score :: Board -> Int
    score (EOBoard f c r) = 52- (length r) - (foldr (+) 0 (map length c))

    playSolitaire :: Board -> Int
    playSolitaire board@(EOBoard f c r) 
        | null c && null r = score board
        | chooseMove board /= Nothing = playSolitaire (maybeTo (chooseMove board))
        | otherwise = score board

    analyseEO :: Int -> Int -> [Int]
    analyseEO _ 0 = []
    analyseEO seed nGames = if playSolitaire (eODeal seed) == 52 then [seed] ++ (analyseEO (seed + 11) (nGames - 1)) else (analyseEO (seed + 1) (nGames - 1))

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
                    [Card(King,Diamonds)True,Card(Three,Clubs)True,Card(Nine,Clubs)True,Card(Nine,Hearts)True,Card(Three,Spades)True,Card(Ten,Spades)True]
                    ] [Card(Five,Clubs)True,Card(Ace,Clubs)True,Card(Four,Diamonds)True,Card(Jack,Diamonds)True, Card (King,Spades) True]
  
    testColumns :: Columns
    testColumns = [[Card (Six,Clubs) True,Card(Seven,Diamonds)True,Card(Ace,Hearts) True,Card(Queen,Hearts) True,Card(King,Clubs) True,Card(Four,Spades)True],
                    [Card(King,Diamonds)True, Card(Queen,Clubs)True,Card(Three,Diamonds)True,Card(Five,Spades)True,Card(Six,Spades)True,Card(Seven,Hearts)True],
                    [Card(Three,Hearts)True,Card(Ten,Diamonds)True,Card(Seven,Spades)True,Card(Queen,Diamonds)True,Card(Five,Hearts)True,Card(Eight,Diamonds)True],
                    [Card(Jack,Spades)True,Card(Six,Hearts)True,Card(Seven,Clubs)True,Card(Eight,Spades)True,Card(Ten,Clubs)True,Card(Queen,Clubs)True],
                    [Card(Ace,Spades)True,Card(Eight,Clubs)True,Card(Ace,Diamonds)True,Card(King,Diamonds)True,Card(Jack,Hearts)True,Card(Four,Clubs)True],
                    [Card(Two,Diamonds)True,Card(Three,Hearts)True,Card(Two,Hearts)True,Card(Ten,Hearts)True,Card(Six,Diamonds)True,Card(Jack,Clubs)True],
                    [Card(Nine,Spades)True,Card(Three,Clubs)True,Card(Nine,Clubs)True,Card(Nine,Hearts)True,Card(Three,Spades)True,Card(Ten,Spades)True],
                    [Card(Ace,Hearts) True, Card(Ace,Spades)True]
                    ]
    testReserve :: Reserve
    testReserve = [Card(King,Clubs)True,Card(Ace,Clubs)True,Card(Five,Clubs)True,Card(Jack,Diamonds)True]

    testColumn :: Deck
    testColumn = [Card (Six,Clubs) True,Card(Seven,Diamonds)True,Card(Ace,Hearts) True,Card(Queen,Hearts) True,Card(King,Clubs) True,Card(Four,Spades)True]

    testB :: Board
    testB = EOBoard [Card (Ace,Spades) True] [] [] 