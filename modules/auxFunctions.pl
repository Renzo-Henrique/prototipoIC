:- module(auxFunctions,   [  processar/4, stringMesmoPrincipioAtivo/1]).

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