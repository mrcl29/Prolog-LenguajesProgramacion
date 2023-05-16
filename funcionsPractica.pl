/*
*   Marc Llobera Villalonga
*   Pere Joan Vives Morey
*/

Nono=[[verd,lila,vermell,vermell], 
 [blau,verd,blau,blau], 
 [lila,blau,verd,verd],  
 [verd,blau,vermell,verd]].

%Funcions_Auxiliars
borrar(_,[],[]).
borrar(X,[X|L],L).
borrar(X,[Y|L1],[Y|L2]):-borrar(X,L1,L2).

%Imprimir_Estructura_de_Dades
escriuNonograma(Nono).
escriuNonograma([]):-write([]).
escriuNonograma([X|L1]):-write([X]),escriuNonograma([L1])

%Pintar_Pantalla
mostraNonograma(Nono,Files,Columnes,IncFiles, IncColumnes).

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