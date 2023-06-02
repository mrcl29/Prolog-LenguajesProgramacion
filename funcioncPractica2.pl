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
