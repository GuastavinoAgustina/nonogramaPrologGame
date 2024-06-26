:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

 init(
   [[3], [1,1,1], [5], [2,2], [6], [3,3], [2,2,1], [2,1,2], [2,1,2], [4,4]],	% RowsClues
    
   [[1], [1,1], [3,5], [1,8], [3,2,1], [1,1,3], [1,4,1], [3,3], [6], [1]],  	% ColsClues
    
   [[_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ], 		
   [_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
   ["#" , _ , _ , _ , _ , _ , _ , _ , _ , _ ],		% Grid
   [_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
   [_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
   [_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
   [_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
   [_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
   [_ , _ , _ , _ , _ , _ , _ , _ , _ , _ ],
   [_, "#" , "#" , "#" , "#" , _ , "#" , "#" , "#" , "#" ]
    ]
    ).
   
   
   