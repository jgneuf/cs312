-- A point on the board, x-y coordinate.
data Point = Point (Int, Int)

-- A move from one point on the board to another.
data Move  = Move Point Point

-- Store board representation and flag for white's turn.
data BoardState = BoardState [[Char]] Bool 

-- Stores utility of a BoardState as -INF, INF or UTL.
data Utility = WWIN | BWIN | UTL

-- Returns utility of a BoardState
boardUtility :: BoardState -> Utility
boardUtility bs = WWIN

-- Generate the initial board of NxN spaces.
genBoard :: Int -> BoardState
genBoard n = BoardState [whites] ++ [take n blanks ] ++ [blacks] True
    where whites = replicate n 'W'
          blacks = replicate n 'B'
          blanks = genBlanks n

-- blanks = blanks
genBlanks n = [(replicate n ' ')] ++ genBlanks n
