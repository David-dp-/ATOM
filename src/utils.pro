%ATOM/src/utils.pro

:- dynamic hideAppNoticesFlag/0.
:- dynamic predicateAppearsInAntecedentsOf/2.
:- dynamic predicateAppearsInConsequentsOf/2.
:- dynamic errorsEncounteredList/1.
:- dynamic verboseFlag/1.

:- dynamic premise/2.
:- dynamic rule/2.
:- dynamic conclusion/2.
:- dynamic movedToEndOfIndexForPred/2.
:- dynamic newAssertionsList/1.

%DOC
% If Condition evaluates to true, print a newline followed by every expression in the "write list"
%
condWriteL(Condition, WriteList) :-
	(call(Condition)
	 -> writeL(WriteList)
	 ;  true ).

%DOC
% If Condition evaluates to true, then call printf with the given format and arg list
%
condPrintf(Condition, Format, ArgList) :-
	(call(Condition)
	 -> printf(Format, ArgList)
	 ;  true ).
	
%DOC
% Print a newline followed by every expression in the "write list"
%
writeL([Write|WriteList]) :-
	nl,write(Write),writeL1(WriteList).

writeL1([]).

writeL1([Write|WriteList]) :-
	write(Write),writeL1(WriteList).

writeLL([]).

writeLL([WriteList|WriteListOfLists]) :-
	writeL(WriteList),writeLL(WriteListOfLists).

errorL(ErrorL) :-
	(hideAppNoticesFlag ; writeL(ErrorL),nl),
	errorsEncounteredList(EncounteredErrsL),
	retract(errorsEncounteredList(_)),
	assert(errorsEncounteredList([ErrorL|EncounteredErrsL])).
	
resetErrors :-
	%writeL([' Resetting error list']),
	(retract(errorsEncounteredList(_)) ; true),
	assert(errorsEncounteredList([])).

%DOC
% Used at the end of tests to minimize interference between tests.
%
resetFacts :-
	%writeL([' Resetting premises, conclusions, rules, and all indexing of rule antes and conseqs']),
	retractall(premise(_,_)),
	retractall(conclusion(_,_)),
	retractall(rule(_,_)),
	retractall(predicateAppearsInAntecedentsOf(_,_)),
	retractall(predicateAppearsInConsequentsOf(_,_)).

%DOC
%Verify that every expression in the "list of test conjunctions" evaluates to true
% Treat each test conjunction separately, so no bindings from one interfere with any others
% Print a summary starting with the provided "test label", then indicating any tests that failed (or if all succeed, just say all succeeded)
%ListOfTestConjunctions has form [(p1,p2),(p3,p4),...] where (p1,p2) is called a conjunction.
%
test(TestLabel,ListOfTestConjunctions) :-
	(hideAppNoticesFlag ; assert(hideAppNoticesFlag)),
	writeL(['Running test set <',TestLabel,'> ...']),
	!,
	(( testL(ListOfTestConjunctions,1),
	   retract(hideAppNoticesFlag),
	   write('  All tests succeeded.'),nl ) ;
	 ( retract(hideAppNoticesFlag),
	   nl, %be sure to finish with newline if any test fails, so 'no' is on its own line
	   !,fail )).
	   
	testL([], _TestCountExpr).

testL([TestConj|TestConjL], TestCountExpr) :-
	copy_term(TestConj,TestConj2), %Make sure each TestConj uses a fresh/empty binds list
	TestCount is TestCountExpr,
	(resetFacts ; true),
	(resetErrors ; true),
	%writeL([' Testing: ',TestConj2,' count: ',TestCount]), %DEBUG!!!!!!!!!
	testLL(TestConj2,Outcome,FailMsg),
	!,
	(Outcome ->
	 !,testL(TestConjL,(1+ TestCount)) ; %whether call(TestConj2) succeeds or fails, we need to test the other conj's; this line is for the success case 
	 (writeL(['  Failed test',TestCount,FailMsg]),
	  %Print all errors revealed by call(TestConj2)
	  errorsEncounteredList(EncounteredErrsL),
	  writeLL(EncounteredErrsL),
	  !,testL(TestConjL,(1+ TestCount)), %whether call(TestConj2) succeeds or fails, we need to test the other conj's; this line is for the failure case
	  !,fail )).
  
testLL([],fail,': Empty test').

testLL([TestConj|TestConjL], Outcome, FailMsg) :-
	testLL1([TestConj|TestConjL], 1, Outcome, FailMsg).

testLL1([], _, true, _).

testLL1([TestConj|TestConjL], TestCountExpr, Outcome, FailMsg) :-
	TestCount is TestCountExpr,
	%writeL(['  Step',TestCount,': ',TestConj]), %DEBUG!!!!!!!!!
	(call(TestConj) ->
	 (!,testLL1(TestConjL,(1+ TestCount),Outcome,FailMsg)) ;
	 (Outcome = fail,
	  term_string(TestConj,TestConjString),
	  term_string(TestCount,TestCountString),
	  concat_string([', step',TestCountString,': ',TestConjString], FailMsg) )),
	(ground(Outcome) ; Outcome = true).

%Beyond this point, and in all files that import utils.pro, there should be a test set for every
% new, exported predicate. Tests not only help to highlight editing errors; they also help document
% what a predicate can do (and be useful for).


%DOC
%Determine if 1st arg can be unified with any members of list in 2nd arg
% If such a unification can be done, binds 3rd parameter to the matched member of the 2nd arg,
%  and binds 4th parameter to a list of all remaining members of 2nd arg.
%This is useful for implementing listsUnifiableDisregardingOrder/4, but could also be useful
% as an alternative to member/2 (because it should avoid adding any bindings)
% and is_member/2 (because it tests membership with unification instead of ==).
%
unifiableWithMemberOf(X,[X|T],X,T) :-
	ground(X).

unifiableWithMemberOf(X,[Y|T],Y,T) :-
	not(not(X = Y)),!.

unifiableWithMemberOf(X,[Y|T],TMember,[Y|TLeftovers]) :-
	!,unifiableWithMemberOf(X,T,TMember,TLeftovers),!.
  
%DOC
%Implements the set operation also known as "difference"; that is, all members of the 1st arg
% that can't be found in the 2nd arg are put in the 3rd parameter. And all members of the 2nd arg
% that can't be found in the 1st arg are put in the 4th parameter.
%
listsUnifiableDisregardingOrder([], [], [], []).

listsUnifiableDisregardingOrder([H1|T1], L2, L1Leftovers, L2Leftovers) :-
	unifiableWithMemberOf(H1,L2,_L2Member,L2Remainder),
	listsUnifiableDisregardingOrder(T1,L2Remainder,L1Leftovers, L2Leftovers), !.

listsUnifiableDisregardingOrder([H1|T1], L2, [H1|L1Leftovers], L2Leftovers) :-
	!,listsUnifiableDisregardingOrder(T1,L2,L1Leftovers, L2Leftovers), !.

listsUnifiableDisregardingOrder([], L2, [], L2).


%DOC
% A handy way of testing that Predicate is true when applied to the combo of Propo1 and each member of the list
%
trueForAll(_,_,[]).
trueForAll(Predicate,Propo1,[Propo2|Propo2List]) :-
	Test =.. [Predicate,Propo1,Propo2],
	call(Test),
	trueForAll(Predicate,Propo1,Propo2List).


%DOC
% A handy way of testing that two propositions aren't directly opposed to each other
%
noConflict(Propo1,Propo2) :-
	not conflict(Propo1,Propo2).

conflict(Propo,neg(Propo)).
conflict(neg(Propo),Propo).


%DOC
%This code is courtesy of Chip Eastham from
% http://forum.amzi.com/viewtopic.php?p=3484&sid=f15b5fe04f6343a8adfb7d8af23886ac#3484
%
pairwiseSum(N1,N2,Sum) :- 
    ground(N1), 
    ground(N2), 
    A is N1, 
    B is N2, 
    (var(Sum) -> C = Sum;  C is Sum), 
    !, 
    C is A + B. 
pairwiseSum(N1,N2,Sum) :- 
    ground(N1), 
    ground(Sum), 
    A is N1, 
    (var(N2) -> B = N2; B is N2), 
    C is Sum, 
    !, 
    B is C - A. 
pairwiseSum(N1,N2,Sum) :- 
    ground(N2), 
    ground(Sum), 
    (var(N1) -> A = N1; A is N1), 
    B is N2, 
    C is Sum, 
    !, 
    A is C - B.
