:- use_module(library(clpfd)).
%Hints given in the description
%at(3, 5, c).
%at(5, 0, w).
%at(9, 6, c).
%Test Cases 2 Battleship Ex 1 Hints
%at(0,0,w).
%Test Cases 2 Battleship Ex 2 Hints
%at(2,0,c).
%at(2,1,r).
%Test Cases 2 Battleship Ex 3 Hints
at(5,0,w).
at(3,5,c).
at(9,6,c).
%mycheck/1
%mycheck(L)
%Holds if L represents a grid/part of a grid
%such that if L Contains a left part of a vessel (l)
%it is followed by the right part (r).
mycheck([]).
mycheck([l]).
mycheck([l,r|T]) :- mycheck(T).
mycheck([H|T]):-
    H \= l,
    H \= r,
    mycheck(T).
%list_to_llists/3
%list_to_llists(L,W,LLists)
%Holds if LLists is a list of lists containing the elements of L.
%Each list in LLists has a length equal to W.
%If the the length of L isn't divisible by w the extra elements are ignored.
list_to_llists(L,W,LLists) :-
    length(L,X),
    M is X mod W,
    Y is X - M,
    list_to_llists_helper(L,W,W,Y,LLists).
%list_to_llists_helper/5
%list_to_llists(L,W,C,RemLength,LLists)
%A helper predicate for list_to_llists/3
%C is a counter that marks the start of a new list/the end of a list
%RemLength is the remaining length of the L
%RemLength prevents backtracking by stopping the predicate when it reaches zero
list_to_llists_helper(_,_,_,0,[]).
list_to_llists_helper([H|T1],W,1,RemLength,[[H]|T3]) :-
    RemLength1 is RemLength - 1,
    list_to_llists_helper(T1,W,W,RemLength1,T3).
list_to_llists_helper([H|T1],W,C,RemLength,[[H|T2]|T3]) :-
    RemLength1 is RemLength - 1,
    C1 is C - 1,
    list_to_llists_helper(T1,W,C1,RemLength1,[T2|T3]).
%getZeroth/2
%getZeroth(L,H)
%Holds if H is the 1st element of the L
getZeroth([H|_],H).
%rest/2
%rest(L,R)
%Holds if R contains all the elements of L except the 1st one
rest([],[]).
rest([_|T],T).
%subList/4
%subList(I1,I2,L,Sub)
%Holds if Sub is a sublist of L containing elements from indices I1 through I2
subList(I1,I2,_,[]) :- I1 > I2.
subList(I1,I2,L,Sub) :-  I2 >= I1,
                         D is I2 - I1,
                         subListHelp(I1,D,L,Sub).

%subListHelp/4
%subListHelp(I1,D,L,Sub)
%A helper predicate for subList/4
%Takes the first index and difference between the indices D
subListHelp(0,0,[H|_],[H]).
subListHelp(_,_,[],[]).
%subListHelp(I,I,[H|_],[H]).
subListHelp(I1,I2,[_|T],Sub) :-
                     I1 > 0,
                     I3 is I1 - 1,
                     subListHelp(I3,I2,T,Sub).
subListHelp(0,I2,[H|T1],[H|T2]) :-
   I2 > 0,
   I3 is I2 - 1,
   subListHelp(0,I3,T1,T2).
%collect_hints/1
%collect_hints(H)
%Holds if H is a list of the provided hints.
collect_hints(H) :-
    setof(at(A,B,C),at(A,B,C),H).

%ensure_hints/4
%ensure_hints(L,Hints,W,H)
%Holds if L is a list representing a grid with width W and height H
%such that the hints in Hints are satisfied.
ensure_hints(L,Hints,W,H) :-
        length(L,Length),
        Length is W*H,
        list_to_llists(L,W,LL),
        ensure_hints_helper(LL,Hints).
%ensure_hints_helper/2
%ensure_hints_helper(LL,Hints)
%A helper predicate for ensure_hints/4
%Holds if LL is a list of lists representing a grid
%such that the hints in Hints are satisfied.
ensure_hints_helper(_,[]).
ensure_hints_helper(LL,[at(X,Y,Type)|Tail]) :-
        nth0(Y,LL,R),
        nth0(X,R,Type),
        ensure_hints_helper(LL,Tail).
%random_assignment/1
%random_assignment(L)
%Holds if L is a grid filled with any of the possible values
%Possible values: w, c, l, r.
random_assignment([]).
random_assignment([H|T]) :-
                    member(H,[w,c,l,r]),
                    random_assignment(T).
%check_rows/4
%check_rows(L,W,H,Totals)
%holds if L represents a grid/partial grid with a width W and height H
%such that the count of vessels in each row corresponds to the counts given in Totals.
check_rows(L,W,H,Totals) :-
                    X is W*H,
                    length(L,X),
                    list_to_llists(L,W,LL),
                    rows_helper(LL,Totals).
check_rows(L,W,H,Totals) :-
                    X is W*H,
                    length(L,Y),
                    Y < X,
                    list_to_llists(L,W,LL),
                    rows_helper(LL,Totals),
                    I1 is Y - (Y mod W) -1,
                    I2 is Y - 1,
                    subList(I1,I2,L,LastRow),
                    Z is Y // W,
                    nth0(Z,Totals,TLR),
                    count_vessels(LastRow,LRC),
                    LRC =< TLR.
%rows_helper/2
%rows_helper(LL,Totals)
%Helper predicate for check_rows/4
%Holds if LL is a list of lists representing a grid
%such that the count of vessels in each row corresponds to the counts given in Totals.
rows_helper([],_).
rows_helper([A|B],[X|Y]) :-
                        count_vessels(A,X),
                        rows_helper(B,Y).
%check_columns/4
%check_columns(L,W,H,Totals)
%holds if L represents a grid/partial grid with a width W and height H
%such that the count of vessels in each column corresponds to the counts given in Totals.
%Uses rows_helper/2 for full grids and columns_helper/3 for partial grids.
check_columns(L,W,_,[X|_]) :-
                length(L,Length),
                Length < W,
                count_vessels(L,LC),
                LC =< X.
check_columns(L,W,H,Totals) :-
                length(L,X),
                X is W*H,
                            list_to_llists(L,W,LR),
                            transpose(LR,LC),
                            rows_helper(LC,Totals).
check_columns(L,W,H,Totals) :-
                length(L,X),
                X > W,
                Y is W*H,
                X < Y,
                list_to_llists(L,W,LR),
                transpose(LR,LC),
                I1 is X - (X mod W) -1,
                I2 is X - 1,
                subList(I1,I2,L,LastRow),
                columns_helper(LC,Totals,LastRow).
%columns_helper/3
%columns_helper(LC,Totals,LastRow)
%A helper method for check_columns/4 that handles partial grids.
%LC is is list of lists representing the transpose of the original partial grid.
%LastRow represents the last row in the original partial grid.
%Holds if the number of vessels in each row (column of the original)
%including the corresponding element in LastRow is equal to the count given in Totals.
columns_helper([],_,_).
columns_helper([A|B],[X|Y],[V|W]) :-
            count_vessels(A,AC),
            count_vessels([V],VC),
            X is AC + VC,
            columns_helper(B,Y,W).
columns_helper([A|B],[X|Y],[]) :-
            count_vessels(A,AC),
            AC =< X,
            columns_helper(B,Y,[]).
%count_vessels/2
%count_vessels(L,R)
%Holds if R is the number of vessels in L
count_vessels(L,R) :- count_vessels(L,0,R).
%count_vessels/3
%count_vessels(L,ACC,R)
%A helper predicate for count_vessels/2 that utilizes and accumulator
count_vessels([],ACC,ACC).
count_vessels([w|T],ACC,R) :- count_vessels(T,ACC,R).
count_vessels([H|T],ACC,R) :-
                           member(H,[c,l,r]),
                           NACC is ACC + 1,
                           count_vessels(T,NACC,R).

%check_destroyer/4
%check_destroyer(L,H,W,TotalDestroyer)
%Holds if L represents a grid with width W and height H
%such that the count of destroyers in L corresponds to TotalDestroyer.
%Handles partial grids by checking that the total count is less than or equal to TotalDestroyer.
check_destroyer([],_,_,_).
check_destroyer(L,H,W,TotalDestroyer) :-
    length(L,X),
    X is H*W,
    list_to_llists(L,W,LL),
    count_destroyers(LL,0,Count),
    TotalDestroyer = Count.
check_destroyer(L,H,W,TotalDestroyer) :-
    length(L,X),
    Y = H*W,
    X < Y,
    list_to_llists(L,W,LL),
    count_destroyers(LL,0,Count),
    TotalDestroyer =< Count.
check_destroyer(L,_,W,TotalDestroyer) :-
    length(L,X),
    X < W,
    count_destroyers_row(L,0,Count),
    TotalDestroyer =< Count.
%count_destroyers/3
%count_destroyers(L,ACC,TD)
%A helper predicate for check_destroyer/4
%Holds if the total count of destroyers in L corresponds to TD.
count_destroyers([],ACC,ACC).
count_destroyers([H|T],ACC,TD) :-
    count_destroyers_row(H,0,TDR),
    NACC is ACC + TDR,
    count_destroyers(T,NACC,TD).
%count_destroyers_row/3
%count_destroyers_row(L,ACC,TDR)
%A helper predicate for count_destroyers/3
%Holds if the total count of destroyers in L corresponds to TDR.
count_destroyers_row([],ACC,ACC).
count_destroyers_row([l,r|T],ACC,TDR) :-
    NACC is ACC + 1,
    count_destroyers_row(T,NACC,TDR).
count_destroyers_row([l],ACC,TDR) :-
    TDR is ACC + 1.
count_destroyers_row([l,Y|T],ACC,TDR) :-
    Y  \= r,
    NACC is ACC + 1,
    count_destroyers_row(T,NACC,TDR).
count_destroyers_row([r|T],ACC,TDR) :-
    NACC is ACC + 1,
    count_destroyers_row(T,NACC,TDR).
count_destroyers_row([H|T],ACC,TDR) :-
    H \= l,
    H \= r,
    count_destroyers_row(T,ACC,TDR).
%check_submarines/4
%check_submarines(L,H,W,TotalSub)
%Holds if L represents a grid with width W and height H
%such that the count of submarines in L corresponds to TotalSub in case of full grid.
%Handles partial grids by checking that the total count is less than or equal to TotalSub.
check_submarines(L,H,W,TotalSub) :-
    length(L,X),
    X is W*H,
    count_submarines(L,0,TotalSub).
check_submarines(L,H,W,TotalSub) :-
    length(L,X),
    Y is W*H,
    X < Y,
    count_submarines(L,0,SubCount),
    SubCount =< TotalSub.
%count_submarines/3
%count_submarines(L,ACC,TS)
%A helper predicate for check_submarines/4
%Holds if the count of submarines in L corresponds to TS.
count_submarines([],ACC,ACC).
count_submarines([c|T],ACC,TS) :-
                                 NACC is ACC + 1,
                                 count_submarines(T,NACC,TS).
count_submarines([H|T],ACC,TS) :-
                               H \= c,
                               count_submarines(T,ACC,TS).

%battleship/7
%battleship(L,W,H,TotalSub,TotalDes,TotalRows,TotalColumns)
%holds if L represents a battleship grid with a width W and height H
%such that the count of submarines in L corresponds to TotalSub
%and the count of destroyers is equal to TotalDes.
%The sum of each row of the grid is represented in TotalRows.
%The sum of each column of the grid is represented in TotalColumns.
battleship(L,W,H,TotalSub,TotalDes,TotalRows,TotalColumns) :-
    X is W*H,
    length(L,X),
    collect_hints(Hints),
    ensure_hints(L,Hints,W,H),
    check_rows(L,W,H,TotalRows),
    check_columns(L,W,H,TotalColumns),
    mycheck(L),
    check_destroyer(L,W,H,TotalDes),
    check_submarines(L,W,H,TotalSub),
    random_assignment(L).
%From the MET website.
pretty_print([],_,_).
pretty_print([Head|Tail],W,H):-
                        %print(pretty_print_h(W,[Head|Tail],L1)),
                        pretty_print_h([Head|Tail],W,L1),
                        pretty_print(L1,W,H).
pretty_print_h(Rest,0,Rest):-nl.
pretty_print_h([],N,[]):-N>0,nl.
pretty_print_h([H|T],W,Rest):-
                            W>0,
                            print(H),print(' '),
                            W1 is W -1 ,
                            pretty_print_h(T,W1,Rest).