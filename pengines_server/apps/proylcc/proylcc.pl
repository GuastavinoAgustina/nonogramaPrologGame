:- module(proylcc,
	[  
		put/8,
		tableroInicial/6,
		checkGanador/3
	]
	).
	
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODAVIA NO LO USAMOS EN NINGUN LADO.
% obtenerFila(+Grid, +RowN, RowsClues, -MyRow, -MyRowClues).
%
% MyCol es la ColN columna de Grid. 
% MyColClues es el elemento en la posición ColN de ColsClues.

obtenerFila([X|_Xs],0,[Y|_Ys],X,Y).
obtenerFila([_X|Xs],RowN,[_Y|Ys],Ws,Zs):-
	RowN > 0,
	RowNs is RowN -1,
	obtenerFila(Xs,RowNs,Ys,Ws,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerElemento(+Xs, +Index,-XsI).
%
% XsI es el elemento en la posición Index de Xs. 

obtenerElemento([X|_Xs],0,X).
obtenerElemento([_X|Xs],N,Ws):-
    N > 0,
	Ns is N -1,
	obtenerElemento(Xs,Ns,Ws).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerColumna(+Grid, +ColN, +ColsClues, -MyCol, -MyColClues).
%
% MyCol es la ColN columna de Grid. 
% MyColClues es el elemento en la posición ColN de ColsClues.

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
	
	replace(Row, RowN, NewRow, Grid, NewGrid),

	
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)),

	obtenerElemento(RowsClues,RowN,MyRowClues),
	matchean(NewRow,MyRowClues,RowSat),
	obtenerColumna(NewGrid,ColN,ColsClues,MyCol,MyColClues),
	matchean(MyCol,MyColClues,ColSat).

verificarRowsSat([Xs],[Ys],[Z]):- matchean(Xs,Ys,Z).
verificarRowsSat([X|Xs],[Y|Ys],[Z|Zs]):- matchean(X,Y,Z), verificarRowsSat(Xs,Ys,Zs).

verificarColsSat(Xs,ColX,0,Ys,[Z]):- 
	obtenerColumna(Xs,ColX,Ys,W,M), 
	matchean(W,M,Z).
verificarColsSat(Xs,ColX,ColY,Ys,[Z|Zs]):- 
	obtenerColumna(Xs,ColX,Ys,W,M), 
	matchean(W,M,Z),
	ColYS is ColY - 1,
    ColXS is ColX + 1,
	verificarColsSat(Xs,ColXS,ColYS,Ys,Zs).

tableroInicial(RowsClues, ColsClues, Grid, CantCol, RowsCluesSat,ColsCluesSat):-
    ColY is CantCol - 1,
	verificarColsSat(Grid,0, ColY, ColsClues, ColsCluesSat),
    verificarRowsSat(Grid, RowsClues, RowsCluesSat).

sonUnos([Xs],Xs).
sonUnos([X|Xs],G):-
	X = 1,
	sonUnos(Xs,G).
sonUnos([_X|_Xs],0).


hayGanador(GR,GC,1):-
	GR = 1,
	GC = 1.
hayGanador(_GR,_GC,0).

checkGanador(RowsCluesSat,ColsCluesSat,Ganador):-
	sonUnos(RowsCluesSat,GR), 
	sonUnos(ColsCluesSat,GC),
	hayGanador(GR,GC,Ganador).
