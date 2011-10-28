-- Learning Haskell's I/O is not trivial, and it's not the point of this
-- assignment, so use a little "evil" for the greater good.
import System.IO.Unsafe

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
        (printBoard pp d d) ++ "\n" 
        where ap = [Point (x, y) | y <- reverse [0..d], x <- [0..d]]
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
test :: BoardState -> String
test b@(BoardState ps wt d)
    | boardUtility b == 999   = "Black wins."
    | boardUtility b == -999  = "White wins."
    | wt == True              = hexapawn (playerMove b (if wt then 'W' else 'B'))
    | otherwise               = hexapawn (playerMove b (if wt then 'W' else 'B'))
    
hexapawn :: BoardState -> String
hexapawn b@(BoardState ps wt d)
    | boardUtility b == 999   = "Black wins."
    | boardUtility b == -999  = "White wins."
    | wt == True              = hexapawn (playerMove b 'W')
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
    | null (map (legalMoves bp 'W' d) wp) = 999
    | null (map (legalMoves wp 'B' d) bp) = -999
    | otherwise                           = 0


-- Return a list of all legal moves that can be made by the given player on
-- the given BoardState. ap is a list of all Points on the given board and
-- am is all the points not occupied.
legalMoves op pl d (Point (x,y)) = [p | p <- pm, p /= NullPoint]
    where v  = if pl == 'W' then (-1) else (1)
          pm = (if ((elem (Point (x, y+v)) op)
                   || y+v < 0 || y+v > d) 
                       then NullPoint else (Point (x, y+v))) :
               (if ((elem (Point (x-1, y+v)) op)
                   && y+v >= 0 && y+v <= d) 
                       then (Point (x-1, y+v)) else NullPoint) :
               (if ((elem (Point (x+1, y+v)) op)
                   && y+v >= 0 && y+v <= d) 
                       then (Point (x+1, y+v)) else NullPoint) : []

-- Get the player's input for a move. I'm not doing a lot of checking on what
-- the user inputs, that's not the point here. Return a Move datatype.
getInput b@(BoardState (wp, bp) wt d) p = do
    -- Print the board to the screen to show available moves.
    putStr (show b)

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
    if (not (elem sp pp))
       ||  (not (elem ep (legalMoves op p d sp))) then getInput b p
    else return (Move (sp, ep))
        where op = if p == 'W' then bp else wp
              pp = if p == 'W' then wp else bp

-- Get the player's Move and create a new BoardState with it.
playerMove b p = applyMove b p um
        where um = unsafePerformIO (getInput b p)

-- Apply the given move to the given BoardState.
applyMove (BoardState (wp, bp) wt d) p (Move (s, e))
    | elem e bp && p == 'W' = BoardState (e:(deleteMove wp s), deleteMove bp e) nwt d
    | elem e wp && p == 'B' = BoardState (deleteMove wp e, e:(deleteMove bp s)) nwt d
    | p == 'W'              = BoardState (e:(deleteMove wp s), bp) nwt d
    | otherwise             = BoardState (wp, e:(deleteMove bp s)) nwt d
        where nwt = not wt

-- Return a list of Moves not including the given Move. If the Move isn't in the
-- list, just return the list.
deleteMove :: [Point] -> Point -> [Point]
deleteMove ms m
    | null ms      = []
    | m == head ms = deleteMove (tail ms) m
    | otherwise    = (head ms) : deleteMove (tail ms) m

-- Minimax.
minimax :: BoardState -> BoardState
minimax b@(BoardState (wp, bp) wt d) = b









