/*
*   Marc Llobera Villalonga
*   Pere Joan Vives Morey
*/

%Funcions_Auxiliars
borrar(_,[],[]).
borrar(X,[X|L],L).
borrar(X,[Y|L1],[Y|L2]):-borrar(X,L1,L2).

%Imprimir_Estructura_de_Dades
escriuNonograma([]).
escriuNonograma([X|L1]):-write(X),nl,escriuNonograma(L1).

%Pintar_Pantalla
mostraNonograma(Nono,Files,Columnes,IncFiles, IncColumnes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar para imprimir espacios en blanco
imprimir_espacios(0).
imprimir_espacios(N) :-
    write(' '),
    N1 is N - 1,
    imprimir_espacios(N1).

% Predicado auxiliar para imprimir una fila de la lista
imprimir_fila([]).
imprimir_fila([X|Xs]) :-
    write(X),
    write(' | '),
    imprimir_fila(Xs).

% Predicado principal para imprimir la lista de listas
imprimir_lista(Lista, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas) :-
    imprimir_filas(Lista, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas, 1).

% Predicado auxiliar para imprimir las filas de la lista
imprimir_filas([], _, _, _, _, _).
imprimir_filas([Fila|Filas], NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas, FilaActual) :-
    imprimir_fila(Fila),
    nl,
    FilaActual < NumFilas,
    imprimir_espacios(SeparacionFilas),
    FilaActual1 is FilaActual + 1,
    imprimir_filas(Filas, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas, FilaActual1).
imprimir_filas([_], NumColumnas, NumFilas, _, SeparacionFilas, FilaActual) :-
    FilaActual =:= NumFilas.

imprimir_lista(Lista, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Construir_Nonograma_Aleatori
ferNonograma(Colors, Files, Columnes,Nono).

%Descriure_nonograma
descriuNonograma(Nono,Descripció).

treuPistes([],[]).
treuPistes([X|L1],[Y|L2]):-extreu(X,Y),treuPistes(L1,L2).
extreu([],[]).
extreu([X|L1],[[seguits,X,1]|L2]):-vegades(X,[X|L1],1),!,extreu(L1,L2).   %seguits_color_1
extreu([X|L1],[[seguits,X,N]|L2]):-vegades(X,[X|L1],N),seguits(N,X,[X|L1]),!,borrar(X,[X|L],L3),extreu(L3,L2).   %seguits_color_N
extreu([X,L1],[[no_seguits,X,N]|L2]):-vegades(X,[X|L1],N),!,borrar(X,[X|L1],L3),extreu(L3,L1).   %no_seguits_color_N

%Pintar_Pistes
mostraPistesHoritzontals(DescripcióHoritzontal, F,C,FInc,CInc). 
mostraPistesVerticals(DescripcióVertical, F,C, FInc,CInc).