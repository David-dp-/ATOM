% Tests for test_isMotivatedAbout/3
test_isMotivatedAbout_3 :-
test('isMotivatedAbout/3',
      [[%PosTest
        addRules_isMotivatedAbout,
        verifyAssertions('1. Liking indicates motivation',
								 fireFwd(p1,likeThat(fred,eating(fred,iceCream),1)),
							    [premise(p1, likeThat(fred, eating(fred, iceCream), 1)),
							     conclusion(isMotivatedAbout(fred, eating(fred, iceCream), 1), [isMotivatedAbout_viaLiking_rule, isMotivatedAbout(fred, eating(fred, iceCream), 1), [[1, [p1, likeThat(fred, eating(fred, iceCream), 1), []]]]])
							    ])],
		 [%PosTest
        addRules_isMotivatedAbout,
        verifyAssertions('2. Obligation indicates motivation',
								 fireFwd(p2,obligedToEnsure(barney,completes(bamBam,homework),1)),
							    [premise(p2, obligedToEnsure(barney, completes(bamBam, homework), 1)),
							     conclusion(isMotivatedAbout(barney, completes(bamBam, homework), 1), [isMotivatedAbout_viaObligation_rule, isMotivatedAbout(barney, completes(bamBam, homework), 1), [[1, [p2, obligedToEnsure(barney, completes(bamBam, homework), 1), []]]]])
							    ])]
      ]).

      
% Tests for test_noConflictingIntent/3
test_noConflictingIntent_3 :-
test('noConflictingIntent/3',
      [[%PosTest: When there're no other intents, there can be no conflict with the candidate intent
        addRules_noConflictingIntent,
        query(noConflictingIntent(fred,eating(fred,iceCream),1), allowBackchaining, Trace1),
        Trace1 = [noConflictingIntent_viaExistingIntentDoesntConflict_rule, noConflictingIntent(fred, eating(fred, iceCream), 1), [[1, [compute, [ground(eating(fred, iceCream)), findall(H25914, query(intendWhenFor(fred, do(fred, _H25929), _H25924, H25914, 1), dontAllowBackchaining, _H25920), '[]'), ground('[]'), trueForAll(noConflict, eating(fred, iceCream), '[]')], '[]']]]]
        ],
       [%PosTest: When the only existing intent has a matching goal, there can be no conflict with the candidate intent
        addRules_noConflictingIntent,
        fireFwd(p1,intendWhenFor(fred,do(fred,feed(fred,iceCream)),holding(fred,iceCream),eating(fred,iceCream),1)),
        query(noConflictingIntent(fred,eating(fred,iceCream),1), allowBackchaining, Trace2),
        Trace2 = [noConflictingIntent_viaExistingIntentDoesntConflict_rule, noConflictingIntent(fred, eating(fred, iceCream), 1), [[1, [compute, [ground(eating(fred, iceCream)), findall(H36026, query(intendWhenFor(fred, do(fred, _H36041), _H36036, H36026, 1), dontAllowBackchaining, _H36032), [eating(fred, iceCream)]), ground([eating(fred, iceCream)]), trueForAll(noConflict, eating(fred, iceCream), [eating(fred, iceCream)])], '[]']]]]
        ],
       [%PosTest: When the only existing intents have different goals or times, there can be no conflict with the candidate intent
        addRules_noConflictingIntent,
        fireFwd(p1,intendWhenFor(fred,do(fred,feed(goat,carrot)),holding(fred,carrot),eating(goat,carrot),1)),
        fireFwd(p2,intendWhenFor(fred,do(fred,feed(fred,iceCream)),holding(fred,iceCream),neg(eating(fred,iceCream)),2)),
        query(noConflictingIntent(fred,eating(fred,iceCream),1), allowBackchaining, Trace3),
        Trace3 = [noConflictingIntent_viaExistingIntentDoesntConflict_rule, noConflictingIntent(fred, eating(fred, iceCream), 1), [[1, [compute, [ground(eating(fred, iceCream)), findall(H46358, query(intendWhenFor(fred, do(fred, _H46373), _H46368, H46358, 1), dontAllowBackchaining, _H46364), [eating(goat, carrot)]), ground([eating(goat, carrot)]), trueForAll(noConflict, eating(fred, iceCream), [eating(goat, carrot)])], '[]']]]]
        ],
       [%NegTest: When there's an existing intent for the negation of the candidate goal, then the candidate intent would conflict
        addRules_noConflictingIntent,
        fireFwd(p1,intendWhenFor(fred,do(fred,feed(fred,iceCream)),holding(fred,iceCream),neg(eating(fred,iceCream)),1)),
        not query(noConflictingIntent(fred,eating(fred,iceCream),1), allowBackchaining, _)
        ],
       [%NegTest: When there's an existing intent for the negation of the candidate goal, then the candidate intent would conflict
        addRules_noConflictingIntent,
        fireFwd(p1,intendWhenFor(fred,do(fred,feed(fred,iceCream)),holding(fred,iceCream),eating(fred,iceCream),1)),
        not query(noConflictingIntent(fred,neg(eating(fred,iceCream)),1), allowBackchaining, _)
        ]
		]).
		
/**/
% Tests for triggerIntentRule
test_triggerIntentRule :-
test('triggerIntentRule',
     [[%PosTest: Ensure that when we like a situation that is not imminent, but where we believe we would eventually
       %          be able to bring it about, and where there are no conflicting intents, leads to having an
       %          intent to bring the situation about.
       addRules_triggerIntent,
       fireFwd(fredLikesIceCream,                    likeThat(fred, eating(fred,iceCream), 1)),
       fireFwd(fredIsntLuckyAboutGettingIceCream,    believe(fred, not incipiently(eating(fred,iceCream)), 1)),
       assert(premise(fredThinksFeedingCausesEating, believe(fred, causeWhenEnabledBy([1:do(Q,feed(Q,F))],eating(Q,F),[2:holding(Q,F)]), 1))), %Cant use fireFwd because this isnt ground
       %(assert(traceReasoner) ; true), %DEBUG!!!!!!!!
       fireFwd(fredThinksHeWillHoldIceCream,         believe(fred, eventually(holding(fred,iceCream)), 1)),
       %(retract(traceReasoner) ; true), %DEBUG!!!!!!!!
       conclusion(intendWhenFor(fred,do(fred,feed(fred,iceCream)),holding(fred,iceCream),eating(fred,iceCream),2), Trace),
       Trace = [triggerIntentRule, intendWhenFor(fred, do(fred, feed(fred, iceCream)), holding(fred, iceCream), eating(fred, iceCream), 2), [[4, [fredThinksHeWillHoldIceCream, believe(fred, eventually(holding(fred, iceCream)), 1), []]], [1, [conclusion, isMotivatedAbout(fred, eating(fred, iceCream), 1), [isMotivatedAbout_viaLiking_rule, isMotivatedAbout(fred, eating(fred, iceCream), 1), [[1, [fredLikesIceCream, likeThat(fred, eating(fred, iceCream), 1), []]]]]]], [2, [fredIsntLuckyAboutGettingIceCream, believe(fred, not incipiently(eating(fred, iceCream)), 1), []]], [3, [fredThinksFeedingCausesEating, believe(fred, causeWhenEnabledBy([1:do(fred, feed(fred, iceCream))], eating(fred, iceCream), [2:holding(fred, iceCream)]), 1), []]], [5, [compute, (var(fred) ; fred = fred), []]], [6, [compute, query(noConflictingIntent(fred, eating(fred, iceCream), 1), allowBackchaining, [noConflictingIntent_viaExistingIntentDoesntConflict_rule, noConflictingIntent(fred, eating(fred, iceCream), 1), [[1, [compute, [ground(eating(fred, iceCream)), findall(H134995, query(intendWhenFor(fred, do(fred, _H135010), _H135005, H134995, 1), dontAllowBackchaining, _H135001), []), ground([]), trueForAll(noConflict, eating(fred, iceCream), [])], []]]]]), []]], [7, [compute, pairwiseSum(1, 1, 2), []]]]]
       ]
     ]).
/**/