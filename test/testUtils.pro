%:- import utils.

%Eyeball verification1... test('test/2 shows which test conditions fail',[true,false,true,false]).
%Eyeball verification2... test('test/2 has a compact way of saying all test conditions passed',[true,true]).


test_testLL1_4 :-
write('Running test set <test_testLL1/4> ...'),
testLL1([5 is 2 + 3,3 is 1-4],1,Outcome,FailMsg),
Outcome == fail,
FailMsg == ", step2: 3 is 1 - 4",
writeL(['  All tests succeeded.']).


% Tests for unifiableWithMemberOf/4; ! and == are used to ensure that first result is the expected one
test_unifiableWithMemberOf_4 :-
test('unifiableWithMemberOf/4',
      [[%Test: Ensure arg1 item can be found when at head of list in arg 2
        unifiableWithMemberOf(1,[1,2],M,R),
        M==1, R==[2] ],
		 [%Test: Ensure arg1 item can be found when in arg2 list *but not at head*
		  unifiableWithMemberOf(2,[1,2],M,R),
		  M==2, R==[1] ],
		 [%Test: Ensure when arg1 item isn't in arg2 list that query *fails*
		  not unifiableWithMemberOf(3,[1,2],M,R) ],
		 [%Test: Ensure arg1 item can be found in arg2 list when arg1 is matchable to head of arg2
		  unifiableWithMemberOf(X,[1,2],M,R),
		  M==1, R==[2] ],
		 [%Test: Ensure arg1 item can be found in arg2 list when head of arg2 is matchable to arg1
		  unifiableWithMemberOf(1,[X,2],M,R),
		  M==X, R==[2] ],
		 [%Test: Ensure arg1 item can be found in arg2 list when head of arg2 is matchable to arg1 *and* arg2 has an empty tail
		  unifiableWithMemberOf(1,[X],M,R),
		  M==X, R==[] ],
		 [%Test: Ensure arg1 item can be found in arg2 list when *non*-head of arg2 is matchable to arg1
		  unifiableWithMemberOf(2,[1,X],M,R),
		  M==X, R==[1] ]
		]).
			   
% Tests for listsUnifiableDisregardingOrder/4
test_listsUnifiableDisregardingOrder_4 :-
test('listsUnifiableDisregardingOrder/4',
      [[%Test: Ensure that when arg1 and arg2 are same (regardless of order), remainder lists are empty
        listsUnifiableDisregardingOrder([1],[1],R1,R2),
        R1==[], R2==[] ],
       [%Test: Ensure that when arg1 and arg2 are *matchable* (regardless of order), remainder lists are empty
        listsUnifiableDisregardingOrder([_X,_Y],[_Z,3],R1,R2),
        R1==[], R2==[] ],
       [%Test: Ensure that when arg1 has non-matchable extras that exactly those extras appear in arg1's remainder list and arg2's remainder list is empty
        listsUnifiableDisregardingOrder([1,2],[2],R1,R2),
        R1==[1], R2==[] ],
       [%Test: Ensure that when *arg2* has non-matchable extras that exactly those extras appear in arg2's remainder list and arg1's remainder list is empty
        listsUnifiableDisregardingOrder([2],[2,3],R1,R2),
        R1==[], R2==[3] ],
       [%Test: Ensure that when arg1 and arg2 both have non-matchable extras that exactly those extras appear in the corresponding remainder lists
        listsUnifiableDisregardingOrder([1,2],[2,3],R1,R2),
        R1==[1], R2==[3] ],
       [%Test: Ensure that when arg1 and arg2 have no overlap, that their remainder lists are identical to the corresponding original lists
        listsUnifiableDisregardingOrder([1],[3],R1,R2),
        R1==[1], R2==[3] ]
      ]).
      
% Tests for test_noConflict/2
test_noConflict_2 :-
test('noConflict/2',
      [[%NegTest: A propo that negates another should be seen as a conflict
        not noConflict(foo(mary),neg(foo(mary))) ],
       [%NegTest: Reversing the order of propos in a conflict should not make a difference
        not noConflict(neg(foo(mary)),foo(mary)) ],
       [%PosTest: Different predications about the same thing don't conflict
        noConflict(foo(mary),goo(mary)) ]
      ]).
      
% Tests for test_trueForAll/3
test_trueForAll_3 :-
test('trueForAll/3',
      [[%NegTest: Ensure that conflicts are found
        not trueForAll(noConflict,foo(mary),[goo(mary),foo(mary),neg(foo(mary))]) ],
       [%PosTest
         trueForAll(noConflict,neg(foo(mary)),[goo(mary),neg(foo(mary))]) ]
      ]).