:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

init(
    [[3], [2], [3], [1,3], []],	% RowsClues
       
    [[1], [], [1,2], [4], [4]], 	% ColsClues
   
   [["X", _ , _ , _ , _ ], 		
    ["X", _ ,"X", _ , _ ],
    ["X", _ , _ , _ , _ ],		% Grid
    ["#","#","#", _ , _ ],
    [ "#" , "#" ,"#","#","#"]
   ]
).
   
   
   