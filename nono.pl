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

/* Escriu les llistes a cada línia de la matriu pasada per paràmetre*/
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

/* Funció recursiva que mou el pinzell a la posició proposada i pinta una llista del Nonograma*/
mostraNonogramaAux([],_,_,_,_).
mostraNonogramaAux([X|L],Files,Columnes,IncFiles, IncColumnes) :- 
    gotoXY(Files, Columnes),
    F is Files+IncFiles,  % Botam una línia avall la distància proposada
    pintaFila(X,Files,Columnes,IncColumnes), 
    mostraNonogramaAux(L,F,Columnes,IncFiles,IncColumnes).

/* Funció que pinta una fila sencera*/
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

/* Funció recursiva que crea una fila aleatòria d'un Nonograma*/
ferNonogramaAux(Colors,Longitud,1,Columnes,[Z]) :- 
    afegirFila(Colors,Longitud,Columnes,Z).
ferNonogramaAux(Colors,Longitud,Files,Columnes,[X|Z]) :- 
    Files>1, 
    afegirFila(Colors,Longitud,Columnes,X), 
    F is Files-1, 
    ferNonogramaAux(Colors,Longitud,F,Columnes,Z).

/*Funcions que afegeixen a Z una fila de colors aleatòris dins la llista de Colors*/
/*Per agafar aleatòriament aquests colors, necessitarem la longitud de la llista de colors i la llista de colors*/
/*Empleam un ràndom per treure un número aleatòri entre la primera i darerra posició de la llista de colors*/
/*Agafam el color que es troba a aquesta posició i l'afegim a la fila*/

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

/* Per treure les llistes de pistes del Nonograma primer param la llista normal a
treuPistes per treure les pistes horitzontals i després treim també les pistes 
horitzontals de la matriu trasposta que correspondran amb les pistes verticals de 
la matriu original*/
descriuNonograma(Nono, [DescripcioHoritzontal, DescripcioVertical]):-
    treuPistes(Nono, DescripcioHoritzontal),
    trasposta(Nono, NonoAux),
    treuPistes(NonoAux, DescripcioVertical).

/* Funció recursiva per treure les pistes d'un nonograma, 
el paràmetre que pasem a 'extreu' serà una llista que correspon a una fila de la matriu*/
treuPistes([],[]).
treuPistes([X|L1],[Y|L2]):-
    extreu(X,Y),!,
    treuPistes(L1,L2).

/* Donat una llista de colors per cada color diferent de la llista treu les seves característiques dins la llista*/
extreu([],[]).
/* Aquí botam un color que ja hem procesat*/
extreu([p|L1],L2) :- 
    extreu(L1,L2). % Botam les 'p' que hem afegit al metode borrarNou
/* Aquí comprovam si el color de la llista es únic en aquesta*/
extreu([X|L1],[[seguits,X,1]|L2]):- 
    vegades(X,[X|L1],1),!,
    extreu(L1,L2).   % seguits_color_1
/* Aquí comprovam si el color de la llista que ja sabem que no és únic surt varis pics i aquests son seguits*/
extreu([X|L1],[[seguits,X,N]|L2]):- 
    vegades(X,[X|L1],N),
    seguits(X,N,[X|L1]),!,
    borrar(X,[X|L1],L3),
    extreu(L3,L2).   % seguits_color_N
/* Aquí ja sabem que el color de la llista surt varis pics i no estan tots seguits, 
els colors iguals restants de la llista seràn reemplaçats*/
extreu([X|L1],[[no_seguits,X,N]|L2]):- 
    vegades(X,[X|L1],N),!,
    borraNou(X,[X|L1],L3),
    extreu(L3,L2).   % no_seguits_color_N

/* Comprova que el color estigui seguit*/
seguits(_,0,_) :- !.
seguits(Color,N,[Color|Z]) :- N1 is N-1, seguits(Color,N1,Z).

/*En el cas de que els colors es trobin seguits (seguits,vermell,2) no tenim problema en borrar-los.
Ara bé, si no es troben seguits (no_seguits,vermell,2) i tenim un cas com [vermell,lila,vermell,lila],
tenim un problema, i és que si borram els vermells així com ho hem fet amb els seguits ens quedarà una
llista així [lila,lila]. Com podem veure, antes els colors lila eren no seguits, i ara es troben seguits.

Per evitar això, si hem de borrar no seguits, exemple: [vermell,vermell,lila,vermell,lila]
, el primer que farem es borrar els que sí estàn seguits i a la resta els sustituirem per una 'p'.
Després de fer això, la llista queda d'aquesta manera: [lila,p,lila]. El que farem amb les 'p' a la funció principal
és simplement passar d'elles*/

/*borrarNou agafa per paràmetre una lletra i una llista on hi trobam aquesta lletra de manera no seguida*/
borraNou(_,[],[]).
borraNou(X,L,Z1) :- 
    borrarPrimers(X,L,Z), 
    canviaResta(X,Z,Z1).

/*Agafa per paràmetre una lletra i una llista i borra totes aquestes lletres que es troben seguides a l'inici*/
borrarPrimers(_,[],[]).
borrarPrimers(X,[Y|L],[Y|L]) :- 
    X \= Y, !.
borrarPrimers(X,[X|L],Y) :- 
    borrarPrimers(X,L,Y).

/*Agafa per paràmetre una lletra i una llista que, en el nostre cas, ja no començarà per aquesta lletra
Sustituïm aquesta lletra dins la llista per 'p's*/
canviaResta(_,[],[]).
canviaResta(X,[Y|L],[Y|L1]) :- X \= Y,!, canviaResta(X,L,L1).
canviaResta(X,[X|L],[p|L1]) :- canviaResta(X,L,L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. Pintar les pistes d’una descripció donada %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*Funció que rep una llista de pistes i les pinta a partir d'una Fila F, una columna C i amb els seus increments
respectius*/
mostraPistesHoritzontals([],_,_,_,_).
mostraPistesHoritzontals([X|L], F,C,FInc,CInc) :- 
    pintaFilaPistes(X,F,C,CInc),
    F1 is F+FInc, 
    mostraPistesHoritzontals(L,F1,C,FInc,CInc).

/*Funció que pinta una fila de pistes*/
/*Per paràmetre rebrà, per exemple: ([[seguides,verd,2],[no_seguides,lila,2],[seguides,vermella,1]],3,1,1)*
el que farem primer es treure el text que surt d'aquestes pistes mitjançant la funció treureText.
treureText ens tornarà una llista, que podrà tenir 2 formes: [Color,<,Numero,>] o [Color,Numero].
El que feim es distingir aquestes 2 formes, si es tracta de la primera, definirem el color, després pintarem
<, seguidament el número i per acabar un altre >
Si es tracta de la segona, simplement definirem el color i pintarem el número*/

pintaFilaPistes([],_,_,_).
/*cas [Color,<,Numero,>]*/
/*Veim que en aquest cas incrementam la columna en +2, això és per els dos caràcters extres pintats <>*/
pintaFilaPistes([X|L],F,C,CInc) :- 
    treureText(X,[Color,_,Numero,_]),!, 
    gotoXY(F,C), 
    color(Color), 
    write('<'), 
    write(Numero), 
    write('>'), 
    C1 is C+CInc+2, 
    pintaFilaPistes(L,F,C1,CInc).
/*cas [Color,Numero]*/
pintaFilaPistes([X|L],F,C,CInc) :- 
    treureText(X,[Color,Numero]), 
    gotoXY(F,C), color(Color), 
    write(Numero), 
    C1 is C+CInc, 
    pintaFilaPistes(L,F,C1,CInc).

/*Feim el mateix que a mostraPistesHoritzontals*/
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

/*Funció qué donada una pista ens torna una llista de la forma: [Color,<,Numero,>] o [Color,Numero] */
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

/*Funció qué donades unes pistes, farà els següent:
1) Fer un nonograma a partir de les pistes Horitzontals
2) Fer un nonograma a partir de les pistes Verticals
3) Comprovar que la trasposta d'un és igual a l'altre*/
resolNonograma([],[]).
resolNonograma([PistesHoritzontals,PistesVerticals], NonoHoritzontal):-  
    nonogramaPistes(PistesHoritzontals,NonoHoritzontal),
    nonogramaPistes(PistesVerticals,NonoVertical),
    verificacio(NonoHoritzontal,NonoVertical).

/*Funció que fa:
1) agafa una fila d'una llista, per exemple [[seguits,verd,1],[no_seguits,vermell,2]] i la converteix
en una altre llista: [verd,vermell,vermell]

2) Permuta [verd,vermell,vermell] i treu les pistes d'aquesta permutació. Si les pistes d'aquesta permutació
coincideixen amb les pistes de la fila, llavors afegirem aquesta permutació a Z3*/

nonogramaPistes([],[]).
nonogramaPistes([X|L],[Y|Z3]) :- 
    generaFila(X,Z), 
    permutacio(Z,Y), 
    append([Y],[],Y1), 
    treuPistes(Y1,Y2),
    Y2 = [X], 
    nonogramaPistes(L,Z3).

/*Funció que, donada una llista de pistes, la converteix a una llista de colors*/
generaFila([],[]).
generaFila([[_,Color,Numero]|L],Z) :- 
    afegeixColors(Color,Numero,Y), 
    generaFila(L,Z1), 
    append(Y,Z1,Z).

/*Aquesta funció rep per paràmetre un color, el seu número i retorna una llista amb aquest nombre de colors*/
afegeixColors(Color,1,[Color]) :- !.
afegeixColors(Color,N,[Color|Y]) :- N1 is N-1,afegeixColors(Color,N1,Y).

/*Verifica que X es transposta de Y*/
verificacio(X,Y):-
    trasposta(Y,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Donat una llista de colors i un tamany de matriu crea un Nonograma aleatori, treu les pistes, mostra les pistes, resol el Nonograma i mostra el Nonograma resolt*/
execucio(LlistaColors,F,C):-
    cls,
    ferNonograma(LlistaColors,F,C,Nono),
    descriuNonograma(Nono,Pistes),
    mostrarPistes(Pistes,F,C),
    resolNonograma(Pistes,NonoResolt),
    Faux is F+2, Caux is C*3+2,
    mostraNonograma(NonoResolt,Faux,Caux,1,3).