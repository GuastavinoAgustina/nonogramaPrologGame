:- module(proylcc,
	[  
		put/8,
		tableroInicial/5,
		checkGanador/3,
        resolverNonograma/4
	]
	).
	
:-use_module(library(lists)).
:- use_module(library(clpfd)).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% matchean(+Xs,+Ys,-LineSat).
%
% Xs es la lista de elementos de una linea.
% Xy es la lista de pistas correspondientes a una linea.

matchean([],[],1).
matchean([],_Ys,0).
matchean([Var|_XS],[],0):-Var == "#".
matchean([_S|XS],[],LineSat):-matchean(XS,[],LineSat).
matchean([Var|Xs],[Y|Ys],LineSat):- 
    Var == "#",
    Yaux is Y-1,
	matchAux(Xs,Yaux,Zs,SatAux),
	verif_resul(SatAux,Zs,Ys,LineSat).
matchean([_S|Xs],Ys,LineSat):-matchean(Xs,Ys,LineSat). 	

verif_resul(1,Zs,Ys,LineSat):-matchean(Zs,Ys,LineSat).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% verificarLineSat(+Xs,+Ys,-Z).
%
% Xs es la grilla del tablero
% Ys es la lista de pistas correspondientes a las lineas.

verificarLineSat([Xs],[Ys],[Z]):- matchean(Xs,Ys,Z).
verificarLineSat([X|Xs],[Y|Ys],[Z|Zs]):- matchean(X,Y,Z), verificarLineSat(Xs,Ys,Zs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% tableroInicial(+RowsClues, +ColsClues, +Grid, +CantCol, -RowsCluesSat,-ColsCluesSat).
%
tableroInicial(RowsClues, ColsClues, Grid, RowsCluesSat,ColsCluesSat):-
    transpose(Grid, TransposeGrid),
	verificarLineSat(TransposeGrid, ColsClues, ColsCluesSat),
    verificarLineSat(Grid, RowsClues, RowsCluesSat).

sonUnos([Xs],Xs).
sonUnos([X|Xs],G):-
	X = 1,
	sonUnos(Xs,G).
sonUnos([_X|_Xs],0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% hayGanador(+GR,+GC,-Ganador).
%
% GR es 1 y todas las pistas de filas del tablero fueron satifechas.
% GC es 1 y todas las pistas de columnas del tablero fueron satifechas.

hayGanador(GR,GC,1):-
	GR = 1,
	GC = 1.
hayGanador(_GR,_GC,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkGanador(+RowsCluesSat,+ColsCluesSat,-Ganador).
%
% RowsCluesSat es la lista de estado de las pistas de las filas. 
% ColsCluesSat es la lista de estado de las pistas de las columnas. 

checkGanador(RowsCluesSat,ColsCluesSat,Ganador):-
	sonUnos(RowsCluesSat,GR), 
	sonUnos(ColsCluesSat,GC),
	hayGanador(GR,GC,Ganador).



%generarFilasSatisfactorias(X,Y,Z)

genFilaSat([],[],[]):-!.
genFilaSat([],_Ys,_Zs):-!,fail.
genFilaSat([_X|Xs],[],["X"|Zs]):- genFilaSat(Xs,[],Zs),!.

genFilaSat([X|Xs],[Y|Ys],Zs):- X=="#", !,  
    pintar([X|Xs],Y,Vs,Ws,Sat), 
    Sat==1,
    genFilaSat(Ws,Ys,Us),
    concat(Vs,Us,Zs).
genFilaSat([X|Xs],Ys,["X"|Zs]):- X == "X", !, genFilaSat(Xs,Ys,Zs).
genFilaSat(Xs,[Y|Ys],Zs):-pintar(Xs,Y,Vs,Ws,Sat),
    Sat==1,
    genFilaSat(Ws,Ys,Us),
    concat(Vs,Us,Zs).
genFilaSat([_X|Xs],Ys,["X"|Zs]):-genFilaSat(Xs,Ys,Zs).

pintar([],0,[],[],1):-!.
pintar([],_Y,_Xs,_Ys,_Sat):-fail.
pintar([Var|Xs],0,["#"],Xs,0):- Var == "#", !,fail.
pintar([_X|Xs],0,["X"],Xs,1):-!.
pintar([_X|Xs],Y,["#"|Ws],Zs,Sat):- YAux is Y-1, 
    pintar(Xs,YAux,Ws,Zs,Sat).

concat([],Ys,Ys).
concat([X|Xs],Ys,[X|Zs]):-concat(Xs,Ys,Zs).

generarCombinacionesFilas([],[],[]).
generarCombinacionesFilas([X|Xs],[Y|Ys],[Z|Zs]):-findall(Fila,genFilaSat(X,Y,Fila),Z),
    generarCombinacionesFilas(Xs,Ys,Zs).

generarGrilla([],[]).
generarGrilla([X|Xs],[K|Ys]):-
    X = [K|_Ks],
    generarGrilla(Xs,Ys).
generarGrilla([X|Xs],Ys):-
    X = [_K|Ks],
    generarGrilla([Ks|Xs],Ys).

generarGrillaCorrecta(Xs,ColsClues,Zs):-
    findall(Ys,generarGrilla(Xs,Ys),PosiblesGrillas),
    encontrarCorrecta(PosiblesGrillas,ColsClues,Zs).
    
encontrarCorrecta([X|_Xs],ColsClues,X):-
    transpose(X,XTranspuesta),
    verificarLineSat(XTranspuesta, ColsClues, ColsCluesSat),
    sonUnos(ColsCluesSat,Sat),
	Sat == 1.
encontrarCorrecta([_X|Xs],ColsClues,Zs):-
    encontrarCorrecta(Xs,ColsClues,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% resolverNonograma(+RowsClues, +ColsClues, -Solucion)
%
% Genera la solución del nonograma para las pistas dadas.
%
resolverNonograma(InitGrid, RowsClues, ColsClues, Solucion) :-
    generarCombinacionesFilas(InitGrid,RowsClues,Combinatoria),
    generarGrillaCorrecta(Combinatoria,ColsClues,Solucion).
