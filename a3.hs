-- Learning Haskell's I/O is not trivial, and it's not the point of this assignment, so use a 
-- little "evil" for the greater good.
import System.IO.Unsafe

-- Point is an x-y coordinate on the board.
data Point = Point (Int, Int) | NullPoint deriving (Eq, Show)

-- Helper functions to access the x and y coordinates of a Point.
px (Point (x, _)) = x
py (Point (_, y)) = y

-- Move datatype stores x-y coordinate of starting and ending positions.
data Move = Move (Point, Point) deriving (Show, Eq)

-- BoardState stores a list of white's pieces as points and a list of black's pieces as a tuple, a 
-- boolean flag indicating whether or not it is white's turn to move, and an Int for the dimension 
-- of the board minus 1, which is useful for printing the board.
data BoardState = BoardState ( [Point], [Point] ) Bool Int

-- Print the board to the screen using a helper function. ap is a list of all possible Points on the 
-- board. pp creates a list of Strings where each String is either "W", "B" or "_" based on whether 
-- the point contains a white pawn, black pawn or is empty, respectively.
instance Show BoardState where
    show (BoardState (whitePieces, blackPieces) whitesTurn dimension) =
        (printBoard playerPieces dimension dimension) ++ "\n" 

        where allPoints    = [Point (x, y) | y <- reverse [0..dimension], x <- [0..dimension]]
              playerPieces = [if (elem point whitePieces || elem point blackPieces)
                                then (if elem point whitePieces then "W"
                                                                else "B")
                                else "_" | point <- allPoints]

-- Given a list of Strings from Show BoardState, recursively iterate through the list and append 
-- newlines after N elements on a line. This makes printing the board to the screen more user 
-- friendly, as each line contains the pieces on that line.
printBoard points currentVal dimension 
    | null (tail points) = head points 
    | currentVal == 0    = head points ++ "\n" ++ printBoard (tail points) dimension dimension
    | otherwise          = head points ++ printBoard (tail points) (currentVal - 1) dimension

-- Store the utility of a BoardState in terms of the AI. We use this for search. Printing is mostly 
-- for debugging.
data Utility = Utility Int deriving (Show, Eq)

-- Generate a NxN board to start the game with.
genBoard n 
    | n <= 2    = error "Board size is too small."
    | otherwise = BoardState (whitePieces, blackPieces) True (n - 1)
        where legalRange  = [0..n - 1]
              whitePieces = [Point (x, n - 1) | x <- legalRange] 
              blackPieces = [Point (x, 0) | x <- legalRange]

-- Play hexapawn with given board. If the game is over, print the winner. Otherwise, get either the 
-- user's move or the AIs move.
test :: BoardState -> String
test board@(BoardState _ whitesTurn _)
    | boardUtility board == 999   = "Black wins."
    | boardUtility board == -999  = "White wins."
    | whitesTurn == True          = test (playerMove board player)
    | otherwise                   = test (playerMove board player)
        where player = if whitesTurn then 'W' else 'B'

-- Play a game of hexapawn. The BoardState passed in should be the initial BoardState, created by
-- the play function. This will continue to get either the user's move or the AI's move and switch
-- players until the game is over.
hexapawn :: BoardState -> String
hexapawn board@(BoardState _ whitesTurn dimension)
    | boardUtility board == 999   = "Black wins."
    | boardUtility board == -999  = "White wins."
    | whitesTurn == True          = hexapawn (playerMove board 'W')
    | otherwise                   = hexapawn (aiMove board depth alpha beta)
        where   depth = dimension
                alpha = -999
                beta  = 999

-- Start a game of hexapawn on a NxN board. Disallow boards smaller than 3x3 and larger than 6x6 for 
-- now.
play n 
    | n < 3     = error "Board size too small, minimum is 3x3."
    | otherwise = test (genBoard n)

-- Return the utility of the board for black. This can be used to determine whether the game is over, 
-- since if the result is BWIN or WWIN, the game has been won by one of the players. Otherwise, it 
-- returns the utility of the board for black, the AI, used in search.
boardUtility (BoardState (whitePieces, blackPieces) whitesTurn dimension)
    | null whitePieces                                                 = 999
    | null blackPieces                                                 = -999
    | not (null [point | point <- blackPieces, py point == dimension]) = 999
    | not (null [point | point <- whitePieces, py point == 0])         = -999
    | null (map (legalMoves blackPieces 'W' dimension) whitePieces)    = 999
    | null (map (legalMoves whitePieces 'B' dimension) whitePieces)    = -999
    | otherwise                                                        = 0

-- Return a list of legal Points that the Point canmove to. This function assumes the Point contains a
-- piece belonging to the player. There are three possible Points that can be moved to:
--  1) Forward one space: The same x-coordinate, y-coordinate plus verticalOffset.
--  2) Forward one space, one space right: x-coordinate plus one, y-coordinate plus verticalOffset. 
--  3) Forward one space, one space left: x-coordinate minus one, y-coordinate plus verticalOffset.
-- The verticalOffset is -1 for white (since white can only move down) and 1 for black.
legalMoves opponentPieces player dimension (Point (x,y)) = 
    [point | point <- possibleMoves, point /= NullPoint]
        where   verticalOffset = if player == 'W' then (-1) else (1)
                possibleMoves  = (if ((elem (Point (x, y + verticalOffset)) opponentPieces)
                                    || y + verticalOffset < 0 || y + verticalOffset > dimension) 
                                        then NullPoint else (Point (x, y + verticalOffset))) :
                                (if ((elem (Point (x - 1, y + verticalOffset)) opponentPieces)
                                    && y + verticalOffset >= 0 && y + verticalOffset <= dimension) 
                                        then (Point (x - 1, y + verticalOffset)) else NullPoint) :
                                (if ((elem (Point (x + 1, y + verticalOffset)) opponentPieces)
                                    && y + verticalOffset >= 0 && y + verticalOffset <= dimension) 
                                        then (Point (x + 1, y + verticalOffset)) else NullPoint) : []

-- Ask the user for their move in canonical form, i.e. (0,0) is the bottom right of the board and
-- values increase left and up. This function checks the user's move is legal on the given board, and
-- repeatedly asks for a Move if the input is illegal.
getInput board@(BoardState (whitePieces, blackPieces) whitesTurn dimension) player = do
    -- Print the board to the screen to show available moves. Remind the user to use canonical form.
    putStr ("* Use canonical form, where (0,0) is bottom\nleft and values increase right and up!\n")
    putStr ("Here's the board:\n" ++ (show board))

    -- Get x,y coordinate of piece to move and make a Point out of it.
    putStr "Enter starting x coordinate: "
    x <- getLine
    let sourceX = read x::Int
    putStr "Enter starting y coordinate: "
    y <- getLine
    let sourceY = read y::Int
    let sourcePoint = Point (sourceX, sourceY)

    -- Get x,y coordinate of square to move to and make Point out of it.
    putStr "Enter ending x coordinate: "
    x <- getLine
    let targetX = read x::Int
    putStr "Enter ending y coordinate: "
    y <- getLine
    let targetY = read y::Int
    let targetPoint = Point (targetX, targetY)

    -- If the move is illegal, recurse. Otherwise return the Move.
    if (not (elem sourcePoint playersPieces)) 
        ||  (not (elem targetPoint (legalMoves opponentsPieces player dimension sourcePoint))) 
            then getInput board player
            else return (Move (sourcePoint, targetPoint))
                where   opponentsPieces = if player == 'W' then blackPieces else whitePieces
                        playersPieces   = if player == 'W' then whitePieces else blackPieces 

-- Take the user's move and create a new BoardState. This uses the "evil" unsafePerformIO function.
-- I choose to spend more time on the AI and less on learning Haskell's IO.
playerMove board player = applyMove board player usersMove
        where usersMove = unsafePerformIO (getInput board player)

-- Apply the the Move to the BoardState and return the new BoardState. There are four cases to check:
--  1) White captures a black pawn: Remove white pawn from source and move into target the black pawn.
--  2) Black captures a white pawn: The converse of (1).
--  3) White moves a pawn straight forward: Remove white pawn from source and move into target.
--  4) Black moves a pawn straight forward: The converse of (3).
-- This function assumes the Move is legal on the BoardState, it does not perform error checking.
applyMove (BoardState (whitePieces, blackPieces) whitesTurn dimension) player (Move (source, target))
    | player == 'W' && elem target blackPieces 
        = BoardState (target : (deleteMove whitePieces source), 
            (deleteMove blackPieces source)) notWhitesTurn dimension
    | player == 'B' && elem target whitePieces
        = BoardState ((deleteMove whitePieces target), 
            target : (deleteMove blackPieces source)) notWhitesTurn dimension
    | player == 'W' 
        = BoardState ((target : (deleteMove whitePieces source)),
            blackPieces) notWhitesTurn dimension
    | otherwise 
        = BoardState (whitePieces, (target : (deleteMove blackPieces source))) 
            notWhitesTurn dimension 
        
        where notWhitesTurn = not whitesTurn 

-- Take a list of points (the whitePieces or blackPieces from a BoardState) and a Point and return the
-- list of points excluding the given Point. This is used by applyMove, since white/blackPieces change
-- on each iteration of the game.
deleteMove :: [Point] -> Point -> [Point]
deleteMove allPoints oldPoint
    | null allPoints                = []
    | oldPoint == head allPoints    = deleteMove (tail allPoints) oldPoint
    | otherwise                     = (head allPoints) : deleteMove (tail allPoints) oldPoint

-- Get the AI's best move on the BoardState. I have implemented negascout, and it can be tweaked from
-- hexapawn, the calling function, to search a certain depth. By default, it searches the full depth of
-- the game tree. The initial alpha and beta values are -999 and 999, respectively.
aiMove board@(BoardState (whitePieces, blackPieces) whitesTurn dimension) depth alpha beta = board 

