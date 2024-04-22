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
matchean([],_Ys,0).
matchean([Var|_XS],[],0):-Var == "#".
matchean([_S|XS],[],RowSat):-matchean(XS,[],RowSat).
matchean([Var|Xs],[Y|Ys],RowSat):- 
    Var == "#",
    Yaux is Y-1,
	matchAux(Xs,Yaux,Zs,SatAux),
	verif_resul(SatAux,Zs,Ys,RowSat).
matchean([_S|Xs],Ys,RowSat):-matchean(Xs,Ys,RowSat).

verif_resul(1,Zs,Ys,RowSat):-matchean(Zs,Ys,RowSat).
verif_resul(0,_Zs,_Ys,0).

matchAux([],0,[],1).
matchAux([],_Y,[],0).
matchAux([Var|Xs],0,Xs,0):-Var == "#".
matchAux([_S|Xs],0,Xs,1).
matchAux([Var|Xs],Y,Zs,SatAux):- 
    Var == "#",
    Yaux is Y-1,
    matchAux(Xs,Yaux,Zs,SatAux).
matchAux([_S|Xs],_Y,Xs,0).

obtenerFila([X|_Xs],0,[Y|_Ys],X,Y).
obtenerFila([_X|Xs],RowN,[_Y|Ys],Ws,Zs):-
	RowN > 0,
	RowNs is RowN -1,
	obtenerFila(Xs,RowNs,Ys,Ws,Zs).

obtenerElemento([X|_Xs],0,X).
obtenerElemento([_X|Xs],N,Ws):-
    N > 0,
	Ns is N -1,
	obtenerElemento(Xs,Ns,Ws).

obtenerColumna([Xs],ColN,Ys,[Ws],Zs):- 
    obtenerElemento(Xs,ColN,Ws),
    obtenerElemento(Ys,ColN,Zs).
obtenerColumna([X|Xs],ColN,Ys,[W|Ws],Zs):-
    obtenerElemento(X,ColN,W),
    obtenerColumna(Xs,ColN,Ys,Ws,Zs).

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

	obtenerElemento(RowsClues,RowN,MyRowClues),
	matchean(NewRow,MyRowClues,RowSat),
	obtenerColumna(NewGrid,ColN,ColsClues,MyCol,MyColClues),
	matchean(MyCol,MyColClues,ColSat).