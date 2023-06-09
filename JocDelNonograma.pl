/*
*   Marc Llobera Villalonga
*   Pere Joan Vives Morey
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FUNCIONS AUXILIARS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

cls:-write('\e[2J'), gotoXY(0,0).

gotoXY(F,C) :- write('\e['),write(C),write(";"),write(F),write("H").

color(negre) :- write("\e[1;90m"). 
color(vermell) :- write("\e[1;91m"). 
color(verd) :- write("\e[1;92m"). 
color(groc) :- write("\e[1;93m"). 
color(blau) :- write("\e[1;94m"). 
color(lila) :- write("\e[1;95m"). 
color(cel) :- write("\e[1;96m").

/*
* Esborrar un element d'una llista
*/
borrar(_,[],[]).
borrar(X,[X|L],L1):-borrar(X,L,L1).
borrar(X,[Y|L1],[Y|L2]):-borrar(X,L1,L2).
/*****************************************************************************************************/

/*
* Fer la trasposta d'una matriu
*/
trasposta([P1|R], L) :-
    length(P1, Longitud),
    traspostaAux([P1|R], L, Longitud),!.

% Cas recursiu
traspostaAux([], [], _).
traspostaAux(_, [], 0).
traspostaAux(ListaOriginal, [PrimerosElementos|RestoNuevaLista], Longitud) :-
    Longitud > 0,
    primers_elements(ListaOriginal, PrimerosElementos),
    eliminar_primers(ListaOriginal, RestoElementos),
    LongitudNueva is Longitud - 1,
    traspostaAux(RestoElementos, RestoNuevaLista, LongitudNueva).

% Predicat auxiliar per obtenir els primers elements de les llistes
primers_elements([], []).
primers_elements([[PrimerElemento|_]|Resto], [PrimerElemento|ListaResto]) :-
    primers_elements(Resto, ListaResto).

% Predicat auxiliar per eliminar els primers elements de les llistes
eliminar_primers([], []).
eliminar_primers([[_|Resto]|RestoFilas], [Resto|ListaResto]) :-
    eliminar_primers(RestoFilas, ListaResto).
/*****************************************************************************************************/

/*
* Conta les aparicions de un element dins una llista
*/
vegades(_, [], 0).
vegades(Elemento, [Elemento|Resto], N) :-
    vegades(Elemento, Resto, N1),
    !,N is N1 + 1.
vegades(Elemento, [_|Resto], N) :-
    vegades(Elemento, Resto, N).
/*****************************************************************************************************/

/*
* Permutar una llista
*/
permutacio([],[]).
permutacio([X|Y],Z) :- permutacio(Y,L), inserir(X,L,Z).

inserir(E,L,Y) :- afegir([E],L,Y).
inserir(E,[X|Y],[X|Z]):-inserir(E,Y,Z).

afegir([],L,L).
afegir([X|L1],L2,[X|L3]) :- afegir(L1,L2,L3).
/*****************************************************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. Escriure per la pantalla, les files d’un nonograma %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

escriuNonograma([]).
escriuNonograma([X|L1]):-
    write(X),
    nl,
    escriuNonograma(L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2. Pintar  a  la  pantalla  un  nonograma,  donades  les  seves  files, %%%
%%% columnes i la separació entre cada fila i columna %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mostraNonograma(Nono,Files,Columnes,IncFiles, IncColumnes) :- 
    mostraNonogramaAux(Nono,Files,Columnes,IncFiles,IncColumnes).

mostraNonogramaAux([],_,_,_,_).
mostraNonogramaAux([X|L],Files,Columnes,IncFiles, IncColumnes) :- 
    gotoXY(Files, Columnes),
    F is Files+IncFiles,  
    pintaFila(X,Files,Columnes,IncColumnes), 
    mostraNonogramaAux(L,F,Columnes,IncFiles,IncColumnes).

pintaFila([X],_,_,_) :- 
    color(X), 
    write('x').
pintaFila([X|L],Files,Columnes, IncColumnes) :- 
    color(X), 
    write('x'), 
    C is Columnes+IncColumnes, gotoXY(Files,C), pintaFila(L,Files,C,IncColumnes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. Construir un nonograma aleatori a partir de una llista de colors, %%%
%%% un número de files i un número de columnes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ferNonograma([],_,_,[]).
ferNonograma(Colors,Files,Columnes,Nono) :- 
    length(Colors,Longitud), 
    ferNonogramaAux(Colors,Longitud,Files,Columnes,Nono).

ferNonogramaAux(Colors,Longitud,1,Columnes,[Z]) :- 
    afegirFila(Colors,Longitud,Columnes,Z).
ferNonogramaAux(Colors,Longitud,Files,Columnes,[X|Z]) :- 
    Files>1, 
    afegirFila(Colors,Longitud,Columnes,X), 
    F is Files-1, 
    ferNonogramaAux(Colors,Longitud,F,Columnes,Z).

afegirFila(Colors,Longitud1,1,[Z]):- 
    Longitud2 is Longitud1+1, 
    random(1,Longitud2,Rnd), 
    nth1(Rnd,Colors,Z).
afegirFila(Colors,Longitud1,Columna,[X|Z]):- 
    Columna>1,
    Longitud2 is Longitud1+1, 
    random(1,Longitud2,Rnd), 
    nth1(Rnd,Colors,X),
    C is Columna-1,
    afegirFila(Colors,Longitud1,C,Z).

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
treuPistes([X|L1],[Y|L2]):-
    extreu(X,Y),!,
    treuPistes(L1,L2).

extreu([],[]).
extreu([p|L1],L2) :- 
    extreu(L1,L2). % Botam les 'p' que hem afegit al metode borrarNou
extreu([X|L1],[[seguits,X,1]|L2]):- 
    vegades(X,[X|L1],1),!,
    extreu(L1,L2).   % seguits_color_1
extreu([X|L1],[[seguits,X,N]|L2]):- 
    vegades(X,[X|L1],N),
    seguits(X,N,[X|L1]),!,
    borrar(X,[X|L1],L3),
    extreu(L3,L2).   % seguits_color_N
extreu([X|L1],[[no_seguits,X,N]|L2]):- 
    vegades(X,[X|L1],N),!,
    borraNou(X,[X|L1],L3),
    extreu(L3,L2).   % no_seguits_color_N

% Comprova que el color estigui seguit
seguits(_,0,_) :- !.
seguits(Color,N,[Color|Z]) :- N1 is N-1, seguits(Color,N1,Z).

% Idea: borram els X primers i la resta ho canviam per una 'p', aquesta p la botarem al metode extreu.
borraNou(_,[],[]).
borraNou(X,L,Z1) :- 
    borrarPrimers(X,L,Z), 
    canviaResta(X,Z,Z1).

borrarPrimers(_,[],[]).
borrarPrimers(X,[Y|L],[Y|L]) :- 
    X \= Y, !.
borrarPrimers(X,[X|L],Y) :- 
    borrarPrimers(X,L,Y).

canviaResta(_,[],[]).
canviaResta(X,[Y|L],[Y|L1]) :- X \= Y,!, canviaResta(X,L,L1).
canviaResta(X,[X|L],[p|L1]) :- canviaResta(X,L,L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. Pintar les pistes d’una descripció donada %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mostraPistesHoritzontals([],_,_,_,_).
mostraPistesHoritzontals([X|L], F,C,FInc,CInc) :- 
    pintaFilaPistes(X,F,C,CInc),
    F1 is F+FInc, 
    mostraPistesHoritzontals(L,F1,C,FInc,CInc).

pintaFilaPistes([],_,_,_).
pintaFilaPistes([X|L],F,C,CInc) :- 
    treureText(X,[Color,_,Numero,_]),!, 
    gotoXY(F,C), 
    color(Color), 
    write('<'), 
    write(Numero), 
    write('>'), 
    C1 is C+CInc+2, 
    pintaFilaPistes(L,F,C1,CInc).
pintaFilaPistes([X|L],F,C,CInc) :- 
    treureText(X,[Color,Numero]), 
    gotoXY(F,C), color(Color), 
    write(Numero), 
    C1 is C+CInc, 
    pintaFilaPistes(L,F,C1,CInc).

mostraPistesVerticals([],_,_,_,_).
    mostraPistesVerticals([X|L],F,C,FInc,CInc) :- 
    pintaColumnaPistes(X,F,C,FInc), C1 is C+CInc,
    mostraPistesVerticals(L,F,C1,FInc,CInc).

pintaColumnaPistes([],_,_,_).
pintaColumnaPistes([X|L],F,C,FInc) :- 
    treureText(X,[Color,_,Numero,_]),!,
    C2 is C-1, 
    gotoXY(F,C2),
    color(Color),
    write('<'),
    write(Numero),
    write('>'), 
    F1 is F+ FInc, 
    pintaColumnaPistes(L,F1,C,FInc).
pintaColumnaPistes([X|L],F,C,FInc) :- 
    treureText(X,[Color,Numero]), 
    gotoXY(F,C), 
    color(Color),
    write(Numero), 
    F1 is F+1, 
    pintaColumnaPistes(L,F1,C,FInc).

treureText([],[]).
treureText([seguits,Y,1],[Y,1]).
treureText([seguits,Y,N],[Y,<,N,>]) :- 
    N > 1.
treureText([no_seguits,Y,N],[Y,N]).

%%% Mostrar Pistes en conjunt %%%
mostrarPistes([Phoritzontals,Pverticals],Fnono,Cnono):-
    Caux is Cnono*3+2,
    mostraPistesVerticals(Pverticals,1,Caux,1,3),
    Faux is Fnono+2,
    mostraPistesHoritzontals(Phoritzontals,Faux,1,1,3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 6. Resoldre un nonograma a partir de la descripció de les pistes per %%%
%%% les files i columnes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolNonograma([],[]).
resolNonograma([PistesHoritzontals,PistesVerticals], NonoHoritzontal):-  
    nonogramaPistes(PistesHoritzontals,NonoHoritzontal),
    nonogramaPistes(PistesVerticals,NonoVertical),
    verificacio(NonoHoritzontal,NonoVertical).

nonogramaPistes([],[]).
nonogramaPistes([X|L],[Y|Z3]) :- 
    generaFila(X,Z), 
    permutacio(Z,Y), 
    append([Y],[],Y1), 
    treuPistes(Y1,Y2),
    Y2 = [X], 
    nonogramaPistes(L,Z3).

generaFila([],[]).
generaFila([[_,Color,Numero]|L],Z) :- 
    afegeixColors(Color,Numero,Y), 
    generaFila(L,Z1), 
    append(Y,Z1,Z).

afegeixColors(Color,1,[Color]) :- !.
afegeixColors(Color,N,[Color|Y]) :- N1 is N-1,afegeixColors(Color,N1,Y).

verificacio(X,Y):-
    trasposta(Y,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Donat una llista de colors i un tamany de matriu crea un Nonograma aleatori, treu les pistes, mostra les pistes, resol el Nonograma i mostra el Nonograma resolt %%%
execucio(LlistaColors,F,C):-
    cls,
    ferNonograma(LlistaColors,F,C,Nono),
    descriuNonograma(Nono,Pistes),
    mostrarPistes(Pistes,F,C),
    resolNonograma(Pistes,NonoResolt),
    Faux is F+2, Caux is C*3+2,
    mostraNonograma(NonoResolt,Faux,Caux,1,3).