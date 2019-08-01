%%%%%%%%%%%%%%% Question 1 %%%%%%%%%%%%%%%
makelist(0, _, []).                 % base case
makelist(N, X, [X | Xs]) :-
  NM1 is N - 1,
  makelist(NM1, X, Xs).             % recursive case


%%%%%%%%%%%%%%% Question 2 %%%%%%%%%%%%%%%
second_min(Lst, X) :-
  sort(Lst, [_, SecMin | _]),
  X is SecMin.


%%%%%%%%%%%%%%% Question 3 %%%%%%%%%%%%%%%
mynumlist(Lo, Hi, []) :- Lo > Hi.   % base case
mynumlist(Lo, Hi, [X | Xs]) :-
  X is Lo,
  NextLo is Lo + 1,
  mynumlist(NextLo, Hi, Xs).        % recurcive case


%%%%%%%%%%%%%%% Question 4 %%%%%%%%%%%%%%%
all_diff([]).                       % base case
all_diff([X | Xs]) :-
  \+ member(X, Xs),
  all_diff(Xs).                     % recursive case


%%%%%%%%%%%%%%% Question 5 %%%%%%%%%%%%%%%
negpos([], [], []).                 % base case
negpos([X | Xs], A, [X | Bs]) :-
    X >= 0,
    negpos(Xs, A, Bs).              % recursive case 1
negpos([X | Xs], [X | As], B) :-
    X < 0,
    negpos(Xs, As, B).              % recursive case 2


%%%%%%%%%%%%%%% Question 6 %%%%%%%%%%%%%%%
magic([X1,X2,X3,X4,X5,X6,X7,X8,X9], [A,B,C,D,E,F,G,H,I]) :-
  permutation([X1,X2,X3,X4,X5,X6,X7,X8,X9], [A,B,C,D,E,F,G,H,I]),
  Row1 is A + B + C,                % sum rows
  Row2 is D + E + F,
  Row3 is G + H + I,
  Col1 is A + D + G,                % sum cols
  Col2 is B + E + H,
  Col3 is C + F + I,

  Row1 == Row2,                     % make sure all sums are equal
  Row2 == Row3,                    
  Row3 == Col1,                     
  Col1 == Col2,     
  Col2 == Col3.


% references:
% https://www.swi-prolog.org/pldoc/man?section=lists
% http://www.sfu.ca/~tjd/383summer2019/prolog_intro.html
% http://www.sfu.ca/~tjd/383summer2019/prolog_combinatorial.html
