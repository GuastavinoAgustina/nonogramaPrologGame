:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

 init(
    [[3], [4], [4], [4], [5]],	% RowsClues
    
    [[2,1], [5], [5], [5], [2]], 	% ColsClues
    
    [[_ , _ , _ , _ , _ ], 		
     [_ , _ , _ , _ , _ ],
     [_ , _ , _ , _ , _ ],		% Grid
     [_ , _ , _ , _ , _ ],
     [_ , _ , _ , _ , _ ]
    ]
    ).
   
   
   