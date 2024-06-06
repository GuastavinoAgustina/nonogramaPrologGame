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
    transpose(NewGrid,NewGridTranspose),
    obtenerElemento(NewGridTranspose,ColN,MyCol),
    obtenerElemento(ColsClues,ColN,MyColClues),
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

concat([],Ys,Ys).
concat([X|Xs],Ys,[X|Zs]):-concat(Xs,Ys,Zs).

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

verificarCompletitud([],1).
verificarCompletitud([X|_Xs],0):-
    X\=="#", X\=="X".
	
verificarCompletitud([_X|Xs],Sat):-
    verificarCompletitud(Xs,Sat).

generarCombinaciones([],[],[],[],[]).
generarCombinaciones([X|Xs],[Y|Ys],[W|Ws],[Sat|Vs],[LineaCoincidente|Zs]):-
    W==0,
    findall(Linea,genLineaSat(X,Y,Linea),Z),
    coincidencias(Z,LineaCoincidente),
    verificarCompletitud(LineaCoincidente,Sat),
    generarCombinaciones(Xs,Ys,Ws,Vs,Zs).
generarCombinaciones([X|Xs],[_Y|Ys],[W|Ws],[W|Vs],[X|Zs]):-
    generarCombinaciones(Xs,Ys,Ws,Vs,Zs).

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

generarCombinacionesRec(InitGrid, _RowsClues, _ColsClues, RowsCluesSat, ColsCluesSat, InitGrid):-
    checkGanador(RowsCluesSat,ColsCluesSat,Ganador),
    Ganador == 1.
generarCombinacionesRec(InitGrid, RowsClues, ColsClues, RowsCluesSat, ColsCluesSat, Solucion):-
    generarCombinaciones(InitGrid,RowsClues,RowsCluesSat, NewRowsCluesSat, NuevaGrilla),
    transpose(NuevaGrilla, GrillaTranspuesta),
    generarCombinaciones(GrillaTranspuesta,ColsClues,ColsCluesSat, NewColsCluesSat, NuevaNuevaGrilla),
    transpose(NuevaNuevaGrilla, Grilla),
    generarCombinacionesRec(Grilla, RowsClues, ColsClues, NewRowsCluesSat, NewColsCluesSat, Solucion).

generarCeros([],[]).
generarCeros([_X|Xs],[0|Ys]):-
    generarCeros(Xs,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% resolverNonograma(+RowsClues, +ColsClues, -Solucion)
%
% Genera la solución del nonograma para las pistas dadas.
%
resolverNonograma(InitGrid, RowsClues, ColsClues, Solucion) :-
    InitGrid = [X|_Xs],
    generarCeros(X,RowsCluesSat),
    transpose(InitGrid,InitGridTranpose),
    generarCeros(InitGridTranpose,ColsCluesSat),
    generarCombinacionesRec(InitGrid, RowsClues, ColsClues, RowsCluesSat, ColsCluesSat, Solucion).

resolverPista(Grilla,GrillaResol,FilaN,ColN,RowsClues, ColsClues,ResGrid, RowSat, ColSat):-
    obtenerElemento(Grilla,FilaN,Fila),
    obtenerElemento(Fila,ColN,MiContenido),
    obtenerElemento(GrillaResol,FilaN,FilaResuelta),
    obtenerElemento(FilaResuelta,ColN,MiPista),
    (MiContenido == MiPista, 
    ResGrid = Grilla,
    matchean(Fila,RowsClues,RowSat),
    transpose(Grilla,GridTranspose),
    obtenerElemento(GridTranspose,ColN,MyCol),
    obtenerElemento(ColsClues,ColN,MyColClues),
	matchean(MyCol,MyColClues,ColSat)
    ;
    put(MiPista,[FilaN,ColN],RowsClues, ColsClues,Grilla,ResGrid,RowSat,ColSat)).
