:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).


matchean([],[],1).
matchean(["X"|XS],[],RowSat):-matchean(XS,[],RowSat).
matchean(["_"|XS],[],RowSat):-matchean(XS,[],RowSat).
matchean(["X"|XS],[],0).
matchean([],[Y|Ys],0).
matchean(["X"|Xs],Ys,RowSat):-matchean(Xs,Ys,RowSat).
matchean(["_"|Xs],Ys,RowSat):-matchean(Xs,Ys,RowSat).
matchean(["#"|Xs],[Y|Ys],RowSat):- 
    Yaux is Y-1,
	matchAux(Xs,Yaux,Zs,SatAux),
	SatAux = 1,
	matchean(Zs,Ys,RowSat).
matchean(["#"|Xs],[Y|Ys],0).

matchAux([],0,[],1).
matchAux([],Y,[],0).
matchAux(["_"|Xs],0,Xs,1).
matchAux(["_"|Xs],Y,Xs,0).
matchAux(["X"|Xs],0,Xs,1).
matchAux(["X"|Xs],Y,Xs,0).
matchAux(["#"|Xs],0,Xs,0).
matchAux(["#"|Xs],Y,Zs,SatAux):- 
    Yaux is Y-1,
    matchAux(Xs,Yaux,Zs,SatAux).

check_row_sat([X|Xs],0,[Y|Ys],RowSat):- matchean(X,Y,RowSat).
check_row_sat([X|Xs],RowN,[Y|Ys],RowSat):-
	RowN > 0,
	RowNs is RowN -1,
	check_row_sat(Xs,RowNs,Ys,RowSat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%

put(Content,[RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)),
	check_row_sat(NewGrid,RowN,Rows_Clues,RowSat).