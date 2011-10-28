import Control.Monad.Trans
-- Point is an x-y coordinate on the board.
data Point = Point (Int, Int) | NullPoint deriving (Eq, Show)

-- Helper functions to access the x and y coordinates of a Point.
px (Point (x, _)) = x
py (Point (_, y)) = y

-- Move datatype stores x-y coordinate of starting and ending positions.
data Move = Move (Point, Point) deriving (Show, Eq)

-- BoardState stores a list of white's pieces as points and a list of
-- black's pieces as a tuple, a boolean flag indicating whether or not it
-- is white's turn to move, and an Int for the dimension of the board 
-- minus 1, which is useful for printing the board.
data BoardState = BoardState ( [Point], [Point] ) Bool Int

-- Print the board to the screen using a helper function. ap is a list of
-- all possible Points on the board. pp creates a list of Strings where
-- each String is either "W", "B" or "_" based on whether the point contains
-- a white pawn, black pawn or is empty, respectively.
instance Show BoardState where
    show (BoardState (wp, bp) wt d) =
        printBoard pp d d 
        where ap = [Point (x, y) | y <- [0..d], x <- [0..d]]
              pp = [if (elem p wp || elem p bp)
                        then (if elem p wp then "W"
                                           else "B")
                        else "_" | p <- ap]

-- Given a list of Strings from Show BoardState, recursively iterate through
-- the list and append newlines after N elements on a line. This makes
-- printing the board to the screen more user-friendly, as each line contains
-- the pieces on that line.
printBoard ps n d
    | null (tail ps) = head ps
    | n == 0         = head ps ++ "\n" ++ printBoard (tail ps) d d
    | otherwise      = head ps ++ printBoard (tail ps) (n - 1) d

-- Store the utility of a BoardState in terms of the AI. We use this for
-- minimax. Printing is mostly for debugging.
data Utility = Utility Int deriving (Show, Eq)

-- Generate a NxN board to start the game with.
genBoard n 
    | n <= 2    = error "Board size is too small."
    | otherwise = BoardState (white, black) True (n - 1)
        where xs    = [0..n - 1]
              white = [Point (x, n - 1) | x <- xs] 
              black = [Point (x, 0) | x <- xs]

-- Play hexapawn with given board. If the game is over, print the winner.
-- Otherwise, get either the user's move or the AIs move.
hexapawn :: BoardState -> String
hexapawn b@(BoardState ps wt d)
    | boardUtility b == 999   = "Black wins."
    | boardUtility b == -999  = "White wins."
    | wt == True              = hexapawn (playerMove b)
    | otherwise               = hexapawn (minimax b)

-- Start a game of hexapawn on a NxN board. Disallow boards smaller than
-- 3x3 and larger than 6x6 for now.
play n 
    | n < 3     = error "Board size too small, minimum is 3x3."
    | n > 5     = error "Board size is too large, maximum is 5x5."
    | otherwise = hexapawn (genBoard n)

-- Return the utility of the board for black. This can be used to determine 
-- whether the game is over, since if the result is BWIN or WWIN, the game has
-- been won by one of the players. Otherwise, it returns the utility of the board
-- for black, the AI, used in minimax.
boardUtility (BoardState (wp, bp) wt d)
    | null wp                             = 999
    | null bp                             = -999
    | not (null [p | p <- bp, py p == d]) = 999
    | not (null [p | p <- wp, py p == 0]) = -999
    | null (legalMoves wp bp 'W' d)       = 999
    | null (legalMoves bp wp 'B' d)       = -999
    | otherwise                           = 0


-- Return a list of all legal moves that can be made by the given player on
-- the given BoardState. ap is a list of all Points on the given board and
-- am is all the points not occupied.
legalMoves pp op pl d = [p | p <- concat pm, p /= NullPoint]
    where v  = if pl == 'W' then (-1) else (1)
          pm = map (\(Point (x, y)) -> 
                        (if ((elem (Point (x, y+v)) op)
                            || y+v < 0 || y+v > d) then NullPoint else (Point (x, y+v))) :
                        (if ((elem (Point (x-1, y+v)) op)
                            && y+v >= 0 && y+v <= d) then (Point (x-1, y+v)) else NullPoint) :
                        (if ((elem (Point (x+1, y+v)) op)
                            && y+v >= 0 && y+v <= d) then (Point (x+1, y+v)) else NullPoint) : []) pp

-- Get the player's input for a move. I'm not doing a lot of checking on what
-- the user inputs, that's not the point here. Return a Move datatype.
getInput :: BoardState -> IO Move
getInput b@(BoardState (wp, bp) wt d) = do 
    -- Get x,y coordinate of piece to move and make a Point out of it.
    putStr "Enter starting x coordinate: "
    sx <- getLine 
    let xc = read sx::Int
    putStr "Enter starting y coordinate: "
    sy <- getLine
    let yc = read sy::Int
    let sp = Point (xc, yc)

    -- Get x,y coordinate of square to move to and make Point out of it.
    putStr "Enter ending x coordinate: "
    ex <- getLine
    let xc = read ex::Int
    putStr "Enter ending y coordinate: "
    ey <- getLine
    let yc = read ey::Int
    let ep = Point (xc, yc)
    return (Move (sp, ep))

-- Get the player's Move and create a new BoardState with it. There is no error
-- checking on what the user inputs, that's not the point here.
playerMove :: BoardState -> BoardState
playerMove b@(BoardState (wp, bp) wt d) = applyMove b um 'W'
        where um = liftIO (getInput b)

-- Apply the given move to the given BoardState.
applyMove :: BoardState -> Move -> Char -> BoardState
applyMove (BoardState (wp, bp) wt d) (Move (s, e)) p
    | elem e op && p == 'W' = BoardState (e:(deleteMove wp s), deleteMove bp e) nwt d
    | elem e op && p == 'B' = BoardState (deleteMove wp e, e:(deleteMove bp s)) nwt d
    | p == 'W'              = BoardState (e:(deleteMove wp s), bp) nwt d
    | otherwise             = BoardState (wp, e:(deleteMove bp s)) nwt d
        where nwt = if wt then False else True
              op  = if p == 'W' then bp else wp

-- Return a list of Moves not including the given Move. If the Move isn't in the
-- list, just return the list.
deleteMove :: [Point] -> Point -> [Point]
deleteMove ms m
    | null ms      = []
    | m == head ms = deleteMove (tail ms) m
    | otherwise    = (head ms) : deleteMove (tail ms) m

-- Minimax.
minimax :: BoardState -> BoardState
minimax (BoardState (wp, bp) wt d) = BoardState (wp, bp) wt d










