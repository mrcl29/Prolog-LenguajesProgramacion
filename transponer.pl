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
