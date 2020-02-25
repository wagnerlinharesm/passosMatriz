% Imprime a matriz
print_matrix([]).
print_matrix([H|T]) :- write(H), nl, print_matrix(T).

% 0 = linha, Y = coluna
replace( [L|Ls] , 0 , Y , [R|Ls] ) :- % Procurando a lista desejada,
  replace_column(L,Y,R)                 % - Substitui a coluna desejada
  .                                       %
replace( [L|Ls] , X , Y  , [L|Rs] ) :- % Se ainda não estiver na linha desejada
  X > 0 ,                                 % -e o delsocamento de linha é positivo,
  X1 is X-1 ,                             % - o deslocamento de linha é dimunído
  replace( Ls , X1 , Y  , Rs )         % - e a recorre para baixo
  .                                       %

replace_column( [R|Cs] , 0 , [Z|Cs] ) :-  % Ao encontrar o offset especificado, a substituição é feita.
 R > -1,
 Z is R-1
 .  
replace_column( [C|Cs] , Y  , [C|Rs] ) :- % Se não,
  Y > 0 ,	                               % - Assume que o deslocamento de linha é positivo,
  Y1 is Y-1 ,                                % - decrementa a lista
  replace_column( Cs , Y1  , Rs )         % - e recorre para baixo.
  .                                          %


%Confere se a posição é uma posição válida para a matriz
%X é a linha
%Y é a coluna
%M é a matriz
isValidPosition(X,Y,[M|Ms]) :- length([M|Ms],Lmax),
                         X < Lmax,
                         X >= 0,
                         length(M,Cmax),
                         Y < Cmax,
                         Y >= 0. 

%Retorna o elementado na (x,y) da matriz.
elemMatrix(M,X,Y,N) :- nth0(X,M,L) , nth0(Y,L,N).

 /****************************************************** VERIFICAR ESTADO FINAL DA MATRIZ ********************************************************/
%Percorre a lista L verificando se os valores são menores que 0, exceto para o índice informado

less_zero_except(L, J) :-
    forall((nth0(I, L, V), dif(I, J)), V < 0), nth0(J,L,N), N == 0.


%Verifica se os elementos da Lista são > 0.
less_zero(L) :-
    maplist(>(0), L).

%Verifica a linha dado os parâmetros
verify_row(Row, I, I, ColJ) :-
    less_zero_except(Row, ColJ).
verify_row(Row, I, J, _) :-
    dif(I, J),
    less_zero(Row).
%Verifica se a matriz chegou no valor final
verifyMatrix(RowI, ColJ, Matrix) :-
    forall(nth0(I, Matrix, Row), verify_row(Row, I, RowI, ColJ)).

/***************************************************************************************************************************************************/

verificaVizinhos(X,Y,M) :- verificaVizinhoDireita(X,Y,M), verificaVizinhoBaixo(X,Y,Z), verificaVizinhoEsquerda(X,Y,Z) , verificaVizinhoCima(X,Y,Z),fail, !.

verificaVizinhoDireita(X,Y,M) :- K is Y+1, elemMatrix(M,X,K,N), N is -1.

verificaVizinhoBaixo(X,Y,Z) :- K is X+1, elemMatrix(M,K,Y,N), N is -1.

verificaVizinhoEsquerda(X,Y,Z) :- K is Y-1, elemMatrix(M,X,K,N), N is -1.

verificaVizinhoCima(X,Y,Z) :- K is X-1, elemMatrix(M,K,Y,N), N is -1.



move(X,Y,M,C) :- verificaVizinhos(X,Y,M).

move(X,Y,M,"fim") :- verifyMatrix(X,Y,M),!.
% Mover para direita
move(X,Y,M,["direita"|C]) :-  elemMatrix(M,X,Y,N), N >= 0, R is X, K is Y+1, replace(M,X,Y,MatrizNova),   move(R,K,MatrizNova,C).

% Mover para baixo x+1 y
move(X,Y,M,["baixo"|C]) :-  elemMatrix(M,X,Y,N), N >= 0, R is X+1, K is Y,  replace(M,X,Y,MatrizNova),   move(R,K,MatrizNova,C).

% Mover para esquerda x y-1
move(X,Y,M,["esquerda"|C]) :-  elemMatrix(M,X,Y,N), N >= 0,  R is X, K is Y-1,  replace(M,X,Y,MatrizNova),   move(R,K,MatrizNova,C).

% Mover para cima x-1 y
move(X,Y,M,["cima"|C]) :-  elemMatrix(M,X,Y,N), N >= 0, R is X-1, K is Y,   replace(M,X,Y,MatrizNova),   move(R,K,MatrizNova,C).


game(M, X, Y) :- findall(C,move(X,Y,M,C), Caminhos), salvar(Caminhos). 	

salvar([]).
salvar([C|Cs]):-  nl, write("Caminho"), nl ,write(C), open('caminhos.txt',append,F), write(F,C), write(F,"\n") , close(F), salvar(Cs).
