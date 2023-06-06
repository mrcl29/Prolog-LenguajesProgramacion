/*
*   Marc Llobera Villalonga
*   Pere Joan Vives Morey
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cls:-write('\e[2J'), gotoXY(0,0).

gotoXY(F,C) :- write('\e['),write(C),write(";"),write(F),write("H").

color(negre) :- write("\e[1;90m"). 
color(vermell) :- write("\e[1;91m"). 
color(verd) :- write("\e[1;92m"). 
color(groc) :- write("\e[1;93m"). 
color(blau) :- write("\e[1;94m"). 
color(lila) :- write("\e[1;95m"). 
color(cel) :- write("\e[1;96m").

%Esborrar un element llista
borrar(_,[],[]).
borrar(X,[X|L],L1):-borrar(X,L,L1).
borrar(X,[Y|L1],[Y|L2]):-borrar(X,L1,L2).

% Predicado auxiliar para obtener los primeros elementos de las listas
primers_elements([], []).
primers_elements([[PrimerElemento|_]|Resto], [PrimerElemento|ListaResto]) :-
    primers_elements(Resto, ListaResto).

% Predicado auxiliar para eliminar los primeros elementos de las listas
eliminar_primers([], []).
eliminar_primers([[_|Resto]|RestoFilas], [Resto|ListaResto]) :-
    eliminar_primers(RestoFilas, ListaResto).

% Predicado principal para construir la nueva lista de listas y mostrarla como matriz
trasposta([P1|R], L) :-
    length(P1, Longitud),
    traspostaAux([P1|R], L, Longitud),limpiar(L),!.

% Caso recursivo: construir la nueva lista de listas
traspostaAux([], _, _).
traspostaAux(_, _, 0).
traspostaAux(ListaOriginal, [PrimerosElementos|RestoNuevaLista], Longitud) :-
    Longitud > 0,
    primers_elements(ListaOriginal, PrimerosElementos),
    eliminar_primers(ListaOriginal, RestoElementos),
    LongitudNueva is Longitud - 1,
    traspostaAux(RestoElementos, RestoNuevaLista, LongitudNueva).

limpiar([]).
limpiar([Fila|Resto]) :-
    limpiar_aux(Fila),
    limpiar(Resto).
limpiar_aux([]).
limpiar_aux([_|Resto]) :-
    limpiar_aux(Resto).

% Conta les aparicions de un element dins una llista
vegades(_, [], 0).
vegades(Elemento, [Elemento|Resto], N) :-
    vegades(Elemento, Resto, N1),
    !,N is N1 + 1.
vegades(Elemento, [_|Resto], N) :-
    vegades(Elemento, Resto, N).

seguits(_, 0, _).
seguits(X, N, [X|Resto]) :-
    !,seguits_aux(X, [X|Resto], N).
seguits(X, N, [_|Resto]) :-
    seguits(X, N, Resto).

seguits_aux(_, [], 0).
seguits_aux(Elemento, [Elemento|Resto], N) :-
    seguits_aux(Elemento, Resto, N1),
    !,N is N1 + 1.


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
%imprimir_lista(Lista, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas) :-
    %imprimir_filas(Lista, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas, 1).
%imprimir_lista(Lista, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas).

% Predicado auxiliar para imprimir las filas de la lista
imprimir_filas([], _, _, _, _, _).
imprimir_filas([Fila|Filas], NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas, FilaActual) :-
    imprimir_fila(Fila),
    nl,
    FilaActual < NumFilas,
    imprimir_espacios(SeparacionFilas),
    FilaActual1 is FilaActual + 1,
    imprimir_filas(Filas, NumColumnas, NumFilas, SeparacionColumnas, SeparacionFilas, FilaActual1).
imprimir_filas([_], _, NumFilas, _, _, FilaActual) :-
    FilaActual =:= NumFilas.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. Escriure per la pantalla, les files d’un nonograma %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escriuNonograma([]).
escriuNonograma([X|L1]):-write(X),nl,escriuNonograma(L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2. Pintar  a  la  pantalla  un  nonograma,  donades  les  seves  files, %%%
%%% columnes i la separació entre cada fila i columna %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pintaFila([X],_,_,_) :- color(X), write('x').
pintaFila([X|L],Files,Columnes, IncColumnes) :- color(X), write('x'), 
                C is Columnes+IncColumnes, gotoXY(Files,C), pintaFila(L,Files,C,IncColumnes).


mostraNonogramaAux([],_,_,_,_).
mostraNonogramaAux([X|L],Files,Columnes,IncFiles, IncColumnes) :- gotoXY(Files, Columnes),
                                F is Files+IncFiles,  pintaFila(X,Files,Columnes,IncColumnes), 
                                mostraNonogramaAux(L,F,Columnes,IncFiles,IncColumnes).

mostraNonograma(Nono,Files,Columnes,IncFiles, IncColumnes) :- mostraNonogramaAux(Nono,Files,Columnes,IncFiles,IncColumnes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. Construir un nonograma aleatori a partir de una llista de colors, %%%
%%% un número de files i un número de columnes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

afegirFila(Colors,Longitud1,1,[Z]):- Longitud2 is Longitud1+1, random(1,Longitud2,Rnd), nth1(Rnd,Colors,Z).
afegirFila(Colors,Longitud1,Columna,[X|Z]):- Longitud2 is Longitud1+1, random(1,Longitud2,Rnd), nth1(Rnd,Colors,X), C is Columna-1,
        afegirFila(Colors,Longitud1,C,Z).

ferNonogramaAux(Colors,Longitud,1,Columnes,[Z]) :- afegirFila(Colors,Longitud,Columnes,Z).
ferNonogramaAux(Colors,Longitud,Files,Columnes,[X|Z]) :- afegirFila(Colors,Longitud,Columnes,X), F is Files-1, 
        ferNonogramaAux(Colors,Longitud,F,Columnes,Z).

ferNonograma([],_,_,[]).
ferNonograma(Colors,Files,Columnes,Nono) :- length(Colors,Longitud), ferNonogramaAux(Colors,Longitud,Files,Columnes,Nono),cls, mostraNonograma(Nono,3,5,1,3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4. Extreure, a partir d’un nonograma, la descripció dels colors de %%%%%%%%%%%%
%%% les seves files i columnes. Ha de tornar una llista on la primera component %%%
%%% sigui la descripció de les files i la segona la descripció de les columnes %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

descriuNonograma(Nono, [DescripcioHoritzontal, DescripcioVertical]):-
treuPistes(Nono, DescripcioHoritzontal),
trasposta(Nono, NonoAux),
treuPistes(NonoAux, DescripcioVertical).

treuPistes([],[]).
treuPistes([X|L1],[Y|L2]):-extreu(X,Y),treuPistes(L1,L2).
extreu([],[]).
extreu([X|L1],[[seguits,X,1]|L2]):-vegades(X,[X|L1],1),!,extreu(L1,L2).   %seguits_color_1
extreu([X|L1],[[seguits,X,N]|L2]):-vegades(X,[X|L1],N),seguits(X,N,[X|L1]),!,borrar(X,[X|L1],L3),extreu(L3,L2).   %seguits_color_N
extreu([X|L1],[[no_seguits,X,N]|L2]):-vegades(X,[X|L1],N),!,borrar(X,[X|L1],L3),extreu(L3,L2).   %no_seguits_color_N

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. Pintar les pistes d’una descripció donada %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mostraPistesHoritzontals(DescripcioHoritzontal, F,C,FInc,CInc). 
mostraPistesVerticals(DescripcioVertical, F,C, FInc,CInc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 6. Resoldre un nonograma a partir de la descripció de les pistes per %%%
%%% les files i columnes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%