:- module(proylcc,
	[  
		put/8,
		tableroInicial/5,
		checkGanador/3,
        resolverNonograma/4,
        resolverPista/9
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
% matchean(+Xs, +Ys, -LineSat).
%
% Xs es la lista de elementos de una linea.
% Xy es la lista de pistas correspondientes a la linea Xs.

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
% obtenerElemento(+Xs, +Index, -XsI).
%
% XsI es el elemento en la posición Index de Xs. 
%
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
    transpose(NewGrid,NewGridTranspose),
    obtenerElemento(NewGridTranspose,ColN,MyCol),
    obtenerElemento(ColsClues,ColN,MyColClues),
	matchean(MyCol,MyColClues,ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% verificarLineSat(+Grid, +LineClues, -GridLineSat).
%
%
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
%
checkGanador(RowsCluesSat, ColsCluesSat, Ganador):-
	sonUnos(RowsCluesSat, GR), 
	sonUnos(ColsCluesSat, GC),
	hayGanador(GR, GC, Ganador).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% genLineaSat(+Linea,+LineaClues,-LineaSatisfactoria).
%
% Genera una linea satisfactoria para una linea inicial y las pistas correspondientes a dicha linea.
%
genLineaSat([],[],[]):-!.
genLineaSat([],_Ys,_Zs):-!,fail.
genLineaSat([X|_Xs],[],_Zs):- X=="#", !, fail. 
genLineaSat([_X|Xs],[],["X"|Zs]):- genLineaSat(Xs,[],Zs),!.
genLineaSat([X|Xs],[Y|Ys],Zs):- X=="#", !,  
    pintar([X|Xs],Y,Vs,Ws,Sat), 
    Sat==1,
    genLineaSat(Ws,Ys,Us),
    concat(Vs,Us,Zs).
genLineaSat([X|Xs],Ys,["X"|Zs]):- X == "X", !, genLineaSat(Xs,Ys,Zs).
genLineaSat(Xs,[Y|Ys],Zs):-pintar(Xs,Y,Vs,Ws,Sat),
    Sat==1,
    genLineaSat(Ws,Ys,Us),
    concat(Vs,Us,Zs).
genLineaSat([_X|Xs],Ys,["X"|Zs]):-genLineaSat(Xs,Ys,Zs).

pintar([],0,[],[],1):-!.
pintar([],_Y,_Xs,_Ys,_Sat):-fail.
pintar([Var|Xs],0,["#"],Xs,0):- Var == "#", !,fail.
pintar([_X|Xs],0,["X"],Xs,1).
pintar([Var|_Xs],_Y,_,_,0):- Var == "X",!,fail.
pintar([_X|Xs],Y,["#"|Ws],Zs,Sat):- YAux is Y-1, 
    pintar(Xs,YAux,Ws,Zs,Sat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% concat(+ListaA,+ListaB,-ListaAB).
%
% Genera una ListaAB que es el resultado de concatenar la ListaA con la ListaB.
%
concat([],Ys,Ys).
concat([X|Xs],Ys,[X|Zs]):-concat(Xs,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% coincidencias(+LineasSatisfactorias, -LineaCoincidente).
%
% Genera una linea que contiene las coincidencias de todas las posibles lineas satisfactorias.
%
coincidencias([],[]).
coincidencias([Z],Z).
coincidencias([Z|Zs],U):-
    Zs = [Y|Ys],
    coincidenciasAux(Z,Y,W),
    coincidencias(Ys,V),
    coincidenciasAux(W,V,U).

coincidenciasAux(Zs,[],Zs).
coincidenciasAux([Z|Zs],[Y|Ys],[Y|P]):-
    Z == Y,
	coincidenciasAux(Zs,Ys,P).
coincidenciasAux([_Z|Zs],[_Y|Ys],[_|P]):-
    coincidenciasAux(Zs,Ys,P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% verificarCompletud(+Linea, -LineaCompleta).
%
% LineaCompleta es 1 si Linea solo contiene "X"s Y "#"s, no contiene "_"s.
% LineaCompleta es 0, si Linea contiene algún "_".
%
verificarCompletitud([],1).
verificarCompletitud([X|_Xs],0):-
    X\=="#", X\=="X".
verificarCompletitud([_X|Xs],Sat):-
    verificarCompletitud(Xs,Sat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% intentarCompletar(+Grid, +LineClues, +LineCluesSat, -NewLineCluesSat, -NewGrid).
%
% Para cada linea de Grid que no esté completa, intenta complementarla a partir de su marcado y sus pistas.
%
intentarCompletar([],[],[],[],[]).
intentarCompletar([X|Xs],[Y|Ys],[W|Ws],[Sat|Vs],[LineaCoincidente|Zs]):-
    W==0,
    findall(Linea, genLineaSat(X,Y,Linea), Z),
    coincidencias(Z, LineaCoincidente),
    verificarCompletitud(LineaCoincidente, Sat),
    intentarCompletar(Xs, Ys, Ws, Vs, Zs).
intentarCompletar([X|Xs],[_Y|Ys],[W|Ws],[W|Vs],[X|Zs]):-
    intentarCompletar(Xs, Ys, Ws, Vs, Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% resolverNonogramaAux(-InitGrid, -RowsClues, -ColsClues, -RowsComplete, -ColsComplete, +Solution).
%
%
resolverNonogramaAux(InitGrid, _RowsClues, _ColsClues, RowsComplete, ColsComplete, InitGrid):-
    checkGanador(RowsComplete, ColsComplete, Ganador),
    Ganador == 1.
resolverNonogramaAux(InitGrid, RowsClues, ColsClues, RowsComplete, ColsComplete, Solution):-
    intentarCompletar(InitGrid, RowsClues, RowsComplete, NewRowsComplete, NewGrid), %intenta completar filas
    transpose(NewGrid, GridTranspose),
    intentarCompletar(GridTranspose, ColsClues, ColsComplete, NewColssComplete, NewNewGrid), %intenta completar columnas
    transpose(NewNewGrid, Grid),
    resolverNonogramaAux(Grid, RowsClues, ColsClues, NewRowsComplete, NewColssComplete, Solution).

generarCeros([],[]).
generarCeros([_X|Xs],[0|Ys]):-
    generarCeros(Xs,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% resolverNonograma(+InitGrid, +RowsClues, +ColsClues, -Solution)
%
% Genera la solución del nonograma para la grilla inicial y las pistas dadas.
%
resolverNonograma(InitGrid,RowsClues,ColsClues,Solution):-
    InitGrid = [X|_Xs],
    generarCeros(X, RowsComplete), 
    transpose(InitGrid, InitGridTranpose),
    generarCeros(InitGridTranpose, ColsComplete),
    resolverNonogramaAux(InitGrid, RowsClues, ColsClues, RowsComplete, ColsComplete, Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% resolverPista(+Grid, +GridSolution, +RowN, +ColN, +RowsClues, +ColsClues, -NewGrid, -RowSat, -ColSat)
%
resolverPista(Grid, GridSolution, RowN, ColN, RowsClues, ColsClues, NewGrid, RowSat, ColSat):-
    obtenerElemento(Grid, RowN, MyRow),
    obtenerElemento(MyRow, ColN, Content),
    obtenerElemento(GridSolution, RowN, FilaResuelta),
    obtenerElemento(FilaResuelta, ColN, ClueAnswer),
    (Content == ClueAnswer, 
    NewGrid = Grid,
    matchean(MyRow, RowsClues, RowSat),
    transpose(Grid, GridTranspose),
    obtenerElemento(GridTranspose, ColN, MyCol),
    obtenerElemento(ColsClues, ColN, MyColClues),
	matchean(MyCol, MyColClues, ColSat)
    ;
    put(ClueAnswer, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat)).