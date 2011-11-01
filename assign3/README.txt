Student Name: Jonathan Neufeld
Student ID: 30671093
CS ID: p9d8

My goal was to put together a modified minimax AI using negascout. I did most of my adversarial search
research through wikipedia and chess programming pages, since I've done a little chess programming
before. This did not turn out the way I had hoped. The hexapawn engine, i.e. checking legal moves and
generating boards, took some time to put together and I have a feeling there are still bugs. I find 
testing/debugging much harder in Haskell since you can't print out breakpoint strings in the middle of
a function. Anyway, I'll try to describe what I've done.

The game is started with "play n", where n is the size of the board you want. The AI will search up to
that depth. The hexapawn function takes a BoardState and checks if the game is over by evaluating the
utility score of the board -- if it's >= abs(1000) then the game is over, since I've defined that to be
the score of the end of the game. Otherwise we check whose turn it is, a boolean flag in the BoardState.
If it's the player, capture the player's move through playerMove and call hexapawn on that new BoardState.
Otherwise get the AI's move through aiMove.

From aiMove I look at each black piece and generate possible (legal) moves they can make. I then apply
these moves new boards (possibleBoards). This is essentially the first ply of nodes in the game tree,
so I call negascout on these nodes and select the maximum (black always maximizes) using bestBoard.
Negascout should, from this point, generate move for white. Since negascout is called recursively I added
a where clause to handle not just these white cases, but subsequent calls to negascout; negascout is told
which player its "scouting" so it can generate BoardStates for that player using possibleBoards. Each
possible move is then applied to a BoardState, and these boards are handed off to shortCircuitFold.

shortCircuitFold is supposed to similate "break" or "return" in C, since that's what negascout should do.
It does this by trying a smaller window, an attempt to prove that no better utility can be found in the
subtree. If it can then we "break" from the fold and return the best move right away without more searching.
That was the idea anyway, but I messed it up. I had hoped to take advantage of this pruning in two ways.
The first was to sort moves by what looked best, i.e. highest utility, before going through their subtrees.
I think this would make pruning much more effective since the lowest utilities appear last and have a
greater chance of being pruned. My second goal was to use this extra time to my advantage, and track how
many subtrees I pruned off. If I pruned off 20-30% (just a guess), then I could search an extra ply or two
on the subtrees that I didn't prune.

In the end I didn't have time to do that and my code doesn't actually do what I want it to even in the
simple cases; my AI is dumb. That's not to say it doesn't win games, and sometimes it seems to make some
pretty smart moves, but more often that I'd like it's just wretchedly stupid.

