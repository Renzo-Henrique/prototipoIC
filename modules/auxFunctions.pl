:- module(auxFunctions,   [  processar/4, stringMesmoPrincipioAtivo/1, stringAfterChar/3,
                        elementosInternos/4, incluirFiltroQuery/3, indiceOcorrencia/3,
                        ultimaOcorrenciaRecursiva/3, removerUltimaOcorrencia/3,
                        padraofinalStringQuery/1, finalStringQuery/2, incluirFiltro/2,
                        incluirCondicaoString/3]).

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

% Predicate to include a condicao to filter in a query
% Condicao must be able match the argument Query
% example: ' CONTAINS(?interactionIDs, "DB00005")' for Query in queryInteraction
% example: ' CONTAINS(?drugIdentifier, "DB00001")' for all the queries made.
incluirFiltroQuery(QueryString, QueryResultado, Condicao):-
    removerUltimaOcorrencia(QueryString, '}', Removido),
    incluirFiltro(Condicao, FiltroString),
    padraofinalStringQuery(FinalStr),
    string_concat(Removido, FiltroString, R1),
    string_concat(R1, FinalStr, QueryResultado), !.

/**
* indiceOcorrencia(+Lista, +Elemento, -Indice).
*
* Obtem o índice que o elemento se encontra na lista
*
* @param Lista, lista a ser verificada
* @param Elemento, elemento a ser encontrado na lista
* @param Indice, índice de onde o elemento se encontra na lista
*
* @note Este predicado é recursivo.
* @note Elemento na cabeça da lista é caso base, retornando 0, caso não encontre ele procura no restante da lista.
*/
indiceOcorrencia([Elemento|_], Elemento, 0).

indiceOcorrencia([_|Resto], Elemento, Indice) :-
    indiceOcorrencia(Resto, Elemento, IndiceAnterior),
    Indice is IndiceAnterior + 1.

/**
* ultimaOcorrenciaRecursiva(+String, +Caractere, -Posicao).
*
* Obtem o índice da última ocorrência do elemento na string
*
* @param Lista, lista a ser verificada
* @param Elemento, elemento a ser encontrado na lista
* @param Indice, índice de onde o elemento se encontra na lista
*
* @note Este predicado é recursivo.
* @note Elemento na cabeça da lista é caso base, retornando 0, caso não encontre ele procura no restante da lista.
*/
ultimaOcorrenciaRecursiva(String, Caractere, Posicao) :-
    string_chars(String, Chars), % Converte string em lista de caracteres
    findall(Indice, indiceOcorrencia(Chars, Caractere, Indice), Posicaos), % Encontra todas as ocorrencias do caractere
    max_list(Posicaos, Posicao), % Obtem maior índice
    Posicao >= 0.


/**
* removerUltimaOcorrencia(+String, +Caractere, -Resultado)
*
* Predicado que obtem a substring até última ocorrência de um caractere em uma string de entrada.
*
* @param String A string de entrada da qual deseja-se remover a última ocorrência do caractere.
* @param Caractere O caractere a ser removido da string.
* @param Resultado Variável que será unificada com a string resultante após a remoção da última ocorrência do caractere.
*
* @note Este predicado é determinístico.
*/
removerUltimaOcorrencia(String, Caractere, Resultado) :-
    ultimaOcorrenciaRecursiva(String, Caractere, UltimaOcorrencia),
    sub_string(String, 0, UltimaOcorrencia, _, Resultado). % Extrai a substring antes do último caractere
        

/**
* removerPrimeiraOcorrencia(+String, +Caractere, -Resultado)
*
* Predicado que remove a primeira ocorrência de um caractere em uma string de entrada.
*
* @param String A string de entrada da qual deseja-se remover a primeira ocorrência do caractere.
* @param Caractere O caractere a ser removido da string.
* @param Resultado Variável que será unificada com a string resultante após a remoção da primeira ocorrência do caractere.
*
* @note Este predicado é determinístico.
*/
removerPrimeiraOcorrencia(String, Caractere, Resultado) :-
    sub_atom(String, Antes, _, _, Caractere), % Acha o índice da primeira ocorrencia
    sub_atom(String, 0, Antes, _, Resultado), !. % Obtem a substring até a ocorrencia do caractere


/**
* padraofinalStringQuery(-Str)
*
* Predicado que obtem uma String no padrão final esperado para uma consulta SPARQL.
*
* @param Str A string a ser retornada
*
*/
padraofinalStringQuery(Str):-
    Str = "} ORDER BY ?drugIdentifier".

/**
* finalStringQuery(-Str, +Var)
*
* Predicado que obtem uma String no padrão final esperado para uma consulta SPARQL, utiliza Var para definir a variável de consulta.
*
* @param Str A string a ser retornada
* @param Var A string contendo a variável de consulta a ser utilizada
* 
*/
finalStringQuery(Str, Var):-
    string_concat("} ORDER BY ", Var, Str).

/**
* incluirFiltroQuery(+Condicao, -Filtro)
*
* Predicado que obtem o Filtro após inserir uma condicional
*
* @param Condicao uma condição a ser inserida dentro do FILTER da query
* @param Filtro É o resultado a ser obtido
* 
*/
incluirFiltro(Condicao, Filtro) :-
    string_concat(" FILTER(", Condicao, Temp),
    string_concat(Temp, ")\n", Filtro).

/**
* incluirCondicaoString(+Chave, +Valor, -Condicao)
*
* Predicado que obtem a condicional de conter substring
*
* @param Chave Variável de consulta a ser analisada
* @param Valor Valor que a variável de consulta deve ter
* @param Condicao, condicional de conter substring referente a chave e valor
* 
*/
incluirCondicaoString(Chave, Valor, Condicao):-
    string_concat(" CONTAINS(", Chave, Temp1),
    string_concat(Temp1, ", \"", Temp2),
    string_concat(Temp2, Valor, Temp3),
    string_concat(Temp3, "\") ", Condicao).



/** <examples>

?- incluirCondicaoString("?interactionIDs", 'DB00026', Condicao).
*/