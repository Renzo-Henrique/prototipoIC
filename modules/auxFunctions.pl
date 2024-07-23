:- module(auxFunctions,   [  processar/4, stringMesmoPrincipioAtivo/1, stringAfterChar/3,
                        elementosInternos/4]).

% Define a regra para percorrerLista a lista de números 1 e a lista de números 2
processar(ListaNumeros1, ListaNumeros2, Resultado, Predicado) :-
    percorrerLista(ListaNumeros1, ListaNumeros2, Resultado, Predicado).

% Regra para percorrerLista a lista de números 1
percorrerLista([], _, [], _).
percorrerLista([Numero1|Resto1], ListaNumeros2, Resultado, Predicado) :-
    percorrerLista2(Numero1, ListaNumeros2, ResultadoTemp, Predicado),
    percorrerLista(Resto1, ListaNumeros2, RestoResultado, Predicado),
    append(ResultadoTemp, RestoResultado, Resultado).

% Regra para percorrerLista a lista de números 2
percorrerLista2(_, [], [], _).
percorrerLista2(Numero1, [Numero2|Resto2], [Resultado|RestoResultado], Predicado) :-
    call(Predicado, Numero1, Numero2, Resultado),
    percorrerLista2(Numero1, Resto2, RestoResultado, Predicado).

stringMesmoPrincipioAtivo(Str):-
    Str = "Possuem mesmo principio ativo".

% Predicado principal: string_after_char(+String, +Char, -Substring)
stringAfterChar(String, Char, Substring) :-
    string_chars(String, Chars),
    afterChar(Chars, Char, SubstringChars),
    string_chars(Substring, SubstringChars).

% Predicado auxiliar: afterChar(+Chars, +Char, -Rest)
afterChar([Char|Rest], Char, Rest).
afterChar([_|Tail], Char, Rest) :-
    afterChar(Tail, Char, Rest).

/**
 * elementosInternos(+Lista, -X, -Y, -Z)
 *
 * Predicado que busca elementos internos de sublistas em uma lista.
 *
 * @param Lista A lista a ser percorrida em busca de sublistas contendo os elementos X, Y e Z.
 * @param X Variável que será unificada com o primeiro elemento das sublistas encontradas.
 * @param Y Variável que será unificada com o segundo elemento das sublistas encontradas.
 * @param Z Variável que será unificada com o terceiro elemento das sublistas encontradas.
 *
 * Este predicado é não determinístico e faz uso de backtracking.
 * @note Se uma sublista contendo os elementos X, Y e Z for encontrada, o predicado unifica as variáveis X, Y e Z e retorna true. Caso contrário, o predicado faz backtracking para encontrar outras sublistas que correspondam aos critérios.
 */
elementosInternos([H|T], X, Y, Z) :-
    H = [X, Y, Z] ;
    elementosInternos(T, X, Y, Z).
