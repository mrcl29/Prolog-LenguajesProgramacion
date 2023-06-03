

%funcions auxiliars
cls:-write('\e[2J'), gotoXY(0,0).

gotoXY(F,C) :- write('\e['),write(C),write(";"),write(F),write("H").

color(negre) :- write("\e[1;90m"). 
color(vermell) :- write("\e[1;91m"). 
color(verd) :- write("\e[1;92m"). 
color(groc) :- write("\e[1;93m"). 
color(blau) :- write("\e[1;94m"). 
color(lila) :- write("\e[1;95m"). 
color(cel) :- write("\e[1;96m").

%funconsprincipals
escriuNonograma([]).
escriuNonograma([X|L1]):-write(X),nl,escriuNonograma(L1).

pintaFila([X],_,_,_) :- color(X), write('x').
pintaFila([X|L],Files,Columnes, IncColumnes) :- color(X), write('x'), 
                C1 is Columnes+IncColumnes, gotoXY(Files,C1), pintaFila(L,Files,C1,IncColumnes).


mostraNonograma(L,Files,Columnes,IncFiles, IncColumnes) :- cls, mostraNonograma1(L,Files,Columnes,IncFiles,IncColumnes).

mostraNonograma1([],_,_,_,_).
mostraNonograma1([X|L],Files,Columnes,IncFiles, IncColumnes) :- gotoXY(Files, Columnes),
                                F1 is Files+IncFiles,  pintaFila(X,Files,Columnes,IncColumnes), 
                                mostraNonograma1(L,F1,Columnes,IncFiles,IncColumnes).

%auxiliars

afegirFila(L,N1,_,1,[Z]):- N2 is N1+1, random(1,N2,N3), nth1(N3,L,Z).
afegirFila(L,N1,_,Columna,[X|Z]):- N2 is N1+1, random(1,N2,N3), nth1(N3,L,X), C is Columna-1,
        afegirFila(L,N1,_,C,Z).

ferNonograma1(L,N1,1,Columnes,[Z]) :- afegirFila(L,N1,_,Columnes,Z).
ferNonograma1(L,N1,Files,Columnes,[X|Z]) :- afegirFila(L,N1,_,Columnes,X), F is Files-1, 
        ferNonograma1(L,N1,F,Columnes,Z).

ferNonograma([],_,_,[]).
ferNonograma(L,Files,Columnes,_) :- length(L,N1), ferNonograma1(L,N1,Files,Columnes,N), mostraNonograma(N,3,5,1,3).
