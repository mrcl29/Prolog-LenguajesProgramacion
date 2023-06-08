%Juntar dues llistes
afegir([],L,L).
afegir([X|L1],L2,[X|L3]):-afegir(L1,L2,L3).

%Trobar darrer element
darrer([X],Y).
darrer([X|L],Y):-darrer(L,Y).

%Esborrar un element llista
esborrar(_,[],[]).
esborrar(X,[X|L],L).
esborrar(X,[Y|L1],[Y|L2]):-esborrar(X,L1,L2).

%Sumar elements llista
sumar([],0).
sumar([X|L],N):-sumar(L,N1), N is N1+X.

%Maxim llista
maxim([X],X).
maxim([X,Y|L],Z):-X>Y, maxim([X|L],Z).
maxim([_|L],Z):-maxim(L,Z).

%Inserir element a llista
inserir(E,L,[E|L]).
inserir(E,[X|Y],[X|Z]):-inserir(E,Y,Z).

% Verificar si un elemento está en una lista
pertany(Elem, [Elem|_]).
pertany(Elem, [_|Resto]) :-
    pertany(Elem, Resto).

% Permutar una lista
permutacio([], []).
permutacio(Lista, [Elem|Resto]) :-
    pertany(Elem, Lista),
    select(Elem, Lista, RestoPermutado),
    permutacio(RestoPermutado, Resto).

%Aplanar_una_llista
aplanar([],[]).
aplanar([X|L],L2):-is_list(X),aplanar(X,L3),aplanar(L1,L4),append(L3,L4,L2).
aplanar([X|L1],[X|L2]):-aplanar(L1,L2).

%Agafar_n_element_de_llista
agafar(1,[X|L],X).
agafar(N,[X|L],Y):-N1 is N-1,agafar(N1,L,Y).

%Rotar_element_llista_cap_a_la_dreta
rotardreta([L1],[Y|L2]):-darrer(L1,Y),rdc(L1,L2).
rdc([X],[]).
rdc([X|L1],[X|L2]):-rdc(L1,L2).

%Rotar_element_llista_cap_a_la_dreta
rotardreta([X|L1],L2):-append(L1,[X],L2).

%Digit
digit(X):- between(1,9,X).

%Aritmograma
solucionA(A,B,C,D,E,F,G,H):-
    digit(A),digit(B),digit(C),
    7 is A*B-C,
    digit(D),digit(E),
    2 is D*4/E,
    digit(F),digit(G),digit(H),
    7 is F-G+H,
    7 is A+D+F,
    4 is B-4+G,
    7 is C-E+H.

solA:-
    digit(A),digit(B),digit(C),
    7 is A*B-C,
    digit(D),digit(E),
    2 is D*4/E,
    digit(F),digit(G),digit(H),
    7 is F-G+H,
    7 is A+D+F,
    4 is B-4+G,
    7 is C-E+H,
    write([A,B,C,D,E,F,G,H]),
    nl,fail.

%Exemple Jugam a bolles
bolles(X,Y):- 
              between(1,30,X),
              between(1,30,Y),
              X1 is X+1,
              X1 is Y-1,
              X2 is X-1,
              Y2 is Y+1,
              Y2 is X2*2.

%Exemple Rebaixes
compra(P,C,M):-
                MaxPantalons is 500/25,
                MaxCamisetes is 500/5,
                MaxMocadors is 500*4,
                between(5,MaxPantalons,P),
                between(10,MaxCamisetes,C),
                between(1,MaxMocadors,M), M<500,
                T is ceiling(P*25+C*5+M//4), T=500,
                R is T-P*25-C*5-M*0.25,
                write("Sobren: "),write(R),write(" euros").

%Exemple Ciclistes
c1([a,_,_]).
c1([c,_,_]).
c2([a,_,_]).
c2([X,_,_]):- X\=a, X=b.
c3([X,_,a]):- X/=c.
c3([a,_,_]).
c3([_,a,_]).
c4([_,a,_]).
c4([_,b,_]).
solucioCiclistes(L):- permutacio([a,b,c],L),
        c1(L),c2(L),c3(L),c4(L).

%Exemple Mutants
animal([c,o,c,o,d,r,i,l]).
animal([t,o,r,t,u,g,a]).
animal([l,l,o,p]).
animal([g,o,r,r,i,o]).
animal([o,n,s,o]).
animal([g,a,l,l,i,n,a]).
animal([s,o,m,e,r,a]).

mutant(X):- animal(A), animal(B),
            append(CapA,CuaA, A),
            CapA\=[], CuaA\=[],
            append(CapB,CuaB, B),
            CapA=CuaB,
            append(CapA,B,X).

%Exemple vaques i vedelles
nVaques(X):- between(1,5,X).
vaques(L):-
    nVaques(B1), nVaques(B2), nVaques(B3), 10 is B1+B2+B3,
    nVaques(N1), nVaques(N2), nVaques(N3), 10 is N1+N2+N3,
    nVaques(M1), nVaques(M2), nVaques(M3), 10 is M1+M2+M3,
    V1 is B1+N1+M1, V2 is B2+N2+M2, V1=V2,
    V3 is B3+N3+M3, V2=V3,
    F1 is B1*3+N1*2+M1,
    F2 is B2*3+N2*2+M2, F1=:=F2,
    F3 is B3*3+N3*2+M3, F2=:=F3,
    L=[[B1,N1,M1],[B2,N2,M2],[B3,N3,M3]].

% Predicado auxiliar para obtener los primeros elementos de las listas
primeros_elementos([], []).
primeros_elementos([[PrimerElemento|_]|Resto], [PrimerElemento|ListaResto]) :-
    primeros_elementos(Resto, ListaResto).

% Predicado auxiliar para eliminar los primeros elementos de las listas
eliminar_primeros([], []).
eliminar_primeros([[_|Resto]|RestoFilas], [Resto|ListaResto]) :-
    eliminar_primeros(RestoFilas, ListaResto).

% Predicado principal para construir la nueva lista de listas y mostrarla como matriz
reorganizar_listas([P1|R], L) :-
    length(P1, Longitud),
    reorganizar_listas_aux([P1|R], L, Longitud),
    imprimir_matriz(L),nl,!.

% Caso recursivo: construir la nueva lista de listas
reorganizar_listas_aux([], [], _).
reorganizar_listas_aux(_, _, 0).
reorganizar_listas_aux(ListaOriginal, NuevaLista, Longitud) :-
    Longitud > 0,
    primeros_elementos(ListaOriginal, PrimerosElementos),
    eliminar_primeros(ListaOriginal, RestoElementos),
    LongitudNueva is Longitud - 1,
    reorganizar_listas_aux(RestoElementos, RestoNuevaLista, LongitudNueva),
    NuevaLista = [PrimerosElementos|RestoNuevaLista].

% Predicado para imprimir una matriz
imprimir_matriz([]).
imprimir_matriz([Fila|Resto]) :-
    imprimir_fila(Fila),
    imprimir_matriz(Resto).

imprimir_fila([]) :-
    nl. % Salto de línea al final de la fila.
imprimir_fila([Elemento|Resto]) :-
    write(Elemento),
    write(' '), % Separador entre elementos.
    imprimir_fila(Resto).

genera(X,Y,A):-
    atom_chars(X,L1),
    atom_chars(Y,L2),
    length(L1,LX),
    length(L2,LY),
    LX = LY,
    genera_aux(L1,L2,L),
    atom_chars(A,L).

genera_aux([],[],[]).
genera_aux([X|R1],[Y|R2],[X,Y|L3]):-genera_aux(R1,R2,L3).

numeros_entre(X, Y, Lista) :-
    X =< Y,
    generar_lista(X, Y, Lista).

generar_lista(X, Y, []) :-
    X > Y.
generar_lista(X, Y, [X|Lista]) :-
    X =< Y,
    X1 is X + 1,
    generar_lista(X1, Y, Lista).
