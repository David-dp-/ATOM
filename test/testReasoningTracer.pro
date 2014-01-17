%ATOM/test/testReasoningTracer.pro

test_flattenAntecedentTree_4 :-
test('flattenAntecedentTree/4',
     [[%PosTest
       flattenAntecedentTree([	:(1,[[p1_1_1,p1_1_2],p1_2]),
								:(2,p2),
								:(3,[p3_1,p3_2,[p3_3_1,p3_3_2]]) ],
							 1,
							 _FlattenedAnteTree,
							 [] )
       ]
     ]).

	 	  
test_updateIndex_5 :-
test('updateIndex/5',
     [[%PosTest for antecedents when only one rule with that ante seen so far
       updateIndex(predicateAppearsInAntecedentsOf(isGoat,Rules1), predicateAppearsInAntecedentsOf, isGoat, rule(goatRule1,<=(playful(X),[1:isGoat(X)])), Rules1),
       predicateAppearsInAntecedentsOf(isGoat,[rule(goatRule1, <=(playful(H576), [1:isGoat(H576)]))])
       ],
      [%PosTest for antecedents when several rules with that ante seen so far, incl some with multiple antes
       updateIndex(predicateAppearsInAntecedentsOf(sleeping,Rules1), predicateAppearsInAntecedentsOf, sleeping, rule(rule1,<=(vertical(X),[1:sleeping(X),2:isCow(X)])), Rules1),
       updateIndex(predicateAppearsInAntecedentsOf(sleeping,Rules2), predicateAppearsInAntecedentsOf, sleeping, rule(rule2,<=([horizontal(X),snores(X)],[1:isDog(X),2:sleeping(X)])), Rules2),
       updateIndex(predicateAppearsInAntecedentsOf(isMan,Rules3), predicateAppearsInAntecedentsOf, isMan, rule(rule3,<=(snores(X),[1:isMan(X)])), Rules3),
       predicateAppearsInAntecedentsOf(sleeping, [rule(rule2, <=([horizontal(H578), snores(H578)], [1:isDog(H578), 2:sleeping(H578)])), rule(rule1, <=(vertical(H606), [1:sleeping(H606), 2:isCow(H606)]))])
       ],
      [%PosTest for consequents when only one rule with that conseq seen so far
       updateIndex(predicateAppearsInConsequentsOf(playful,Rules1), predicateAppearsInConsequentsOf, playful, rule(goatRule1,<=(playful(X),[1:isGoat(X)])), Rules1),
       predicateAppearsInConsequentsOf(playful, [rule(goatRule1, <=(playful(H537), [1:isGoat(H537)]))])
       ],
      [%PosTest for consequents when several rules with that conseq seen so far, incl some with multiple consqs
       updateIndex(predicateAppearsInConsequentsOf(vertical,Rules1), predicateAppearsInConsequentsOf, vertical, rule(rule1,<=(vertical(X),[1:sleeping(X),2:isCow(X)])), Rules1),
       updateIndex(predicateAppearsInConsequentsOf(snores,Rules2), predicateAppearsInConsequentsOf, snores, rule(rule2,<=([horizontal(X),snores(X)],[1:isDog(X),2:sleeping(X)])), Rules2),
       updateIndex(predicateAppearsInConsequentsOf(snores,Rules3), predicateAppearsInConsequentsOf, snores, rule(rule3,<=(snores(X),[1:isMan(X)])), Rules3),
       predicateAppearsInConsequentsOf(snores, [rule(rule3, <=(snores(H576), [1:isMan(H576)])), rule(rule2, <=([horizontal(H595), snores(H595)], [1:isDog(H595), 2:sleeping(H595)]))])
       ],
      [%PosTest for triggers & enablers when several rules with that predicate have been seen so far,
       % incl some with multiple triggers & enablers
       R1 = rule(rule1,causeWhenEnabledBy([1:waterAtLevel(B,L)],[bucketOverflows(B),waterSpills],[2:height(B,H),3:compute(H < L)])),
       R2 = rule(rule2,causeWhenEnabledBy([1:stepOn(A,B)],[seeFromHeight(A,H)],[2:height(B,H1),3:height(A,H2),4:compute(H is H1 + H2)])),
       R3 = rule(rule3,causeWhenEnabledBy([1:pour(A,C)],[waterSpills],[2:isCup(C),3:notEmpty(C)])),
       R4 = rule(rule4,causeWhenEnabledBy([1:stepOn(A,B)],[bitten(A)],[2:isSnake(B)])),
       updateIndex(predicateAppearsInAntecedentsOf(height,Rules1), predicateAppearsInAntecedentsOf, height, R1, Rules1),
       updateIndex(predicateAppearsInConsequentsOf(waterSpills,Rules2), predicateAppearsInConsequentsOf, waterSpills, R1, Rules2),
       updateIndex(predicateAppearsInAntecedentsOf(height,Rules3), predicateAppearsInAntecedentsOf, height, R2, Rules3),
       updateIndex(predicateAppearsInAntecedentsOf(stepOn,Rules4), predicateAppearsInAntecedentsOf, stepOn, R2, Rules4),
       updateIndex(predicateAppearsInConsequentsOf(waterSpills,Rules5), predicateAppearsInConsequentsOf, waterSpills, R3, Rules5),
       updateIndex(predicateAppearsInAntecedentsOf(stepOn,Rules6), predicateAppearsInAntecedentsOf, stepOn, R4, Rules6),
       predicateAppearsInAntecedentsOf(height, [rule(rule2, causeWhenEnabledBy([1:stepOn(H177, H178)], [seeFromHeight(H177, H183)], [2:height(H178, H191), 3:height(H177, H199), 4:compute(H183 is H191 + H199)])), rule(rule1, causeWhenEnabledBy([1:waterAtLevel(H228, H229)], [bucketOverflows(H228), waterSpills], [2:height(H228, H243), 3:compute(H243 < H229)]))]),
       predicateAppearsInAntecedentsOf(stepOn, [rule(rule4, causeWhenEnabledBy([1:stepOn(H177, H178)], [bitten(H177)], [2:isSnake(H178)])), rule(rule2, causeWhenEnabledBy([1:stepOn(H205, H206)], [seeFromHeight(H205, H211)], [2:height(H206, H219), 3:height(H205, H227), 4:compute(H211 is H219 + H227)]))]),
       predicateAppearsInConsequentsOf(waterSpills, [rule(rule3, causeWhenEnabledBy([1:pour(H177, H178)], [waterSpills], [2:isCup(H178), 3:notEmpty(H178)])), rule(rule1, causeWhenEnabledBy([1:waterAtLevel(H210, H211)], [bucketOverflows(H210), waterSpills], [2:height(H210, H225), 3:compute(H225 < H211)]))])
       ]
     ]).
	 	  
test_updateIndices_1 :-
test('updateIndices/1',
     [[%PosTest for <= using a single rule
       updateIndices(rule(rule1, <=([hasHooves(X),isWhite(X)], [1:isGoat(X), 2:livesInMtns(X)]))),
       predicateAppearsInAntecedentsOf(isGoat,     [rule(rule1, <=([hasHooves(H6009), isWhite(H6009)], [1:isGoat(H6009), 2:livesInMtns(H6009)]))]),
       predicateAppearsInAntecedentsOf(livesInMtns,[rule(rule1, <=([hasHooves(H6009), isWhite(H6009)], [1:isGoat(H6009), 2:livesInMtns(H6009)]))]),
       predicateAppearsInConsequentsOf(hasHooves,  [rule(rule1, <=([hasHooves(H6009), isWhite(H6009)], [1:isGoat(H6009), 2:livesInMtns(H6009)]))]),
       predicateAppearsInConsequentsOf(isWhite,    [rule(rule1, <=([hasHooves(H6009), isWhite(H6009)], [1:isGoat(H6009), 2:livesInMtns(H6009)]))])
       ],
      [%PosTest for causeWhenEnabledBy using several rules
       updateIndices(rule(rule1,causeWhenEnabledBy([1:waterAtLevel(B,L)],[bucketOverflows(B),waterSpills],[2:height(B,H),3:compute(H < L)]))),
       updateIndices(rule(rule2,causeWhenEnabledBy([1:stepOn(A,B)],[seeFromHeight(A,H)],[1:height(B,H1),2:height(A,H2),3:compute(H is H1 + H2)]))),
       updateIndices(rule(rule3,causeWhenEnabledBy([1:pour(A,C)],[waterSpills],[2:isCup(C),3:notEmpty(C)]))),
       updateIndices(rule(rule4,causeWhenEnabledBy([1:stepOn(A,B)],[bitten(A)],[2:isSnake(B)]))),
		 predicateAppearsInConsequentsOf(bucketOverflows, [rule(rule1, causeWhenEnabledBy([1:waterAtLevel(H175, H176)], [bucketOverflows(H175), waterSpills], [2:height(H175, H190), 3:compute(H190 < H176)]))]),
		 predicateAppearsInConsequentsOf(waterSpills, [rule(rule3, causeWhenEnabledBy([1:pour(H175, H176)], [waterSpills], [2:isCup(H176), 3:notEmpty(H176)])), rule(rule1, causeWhenEnabledBy([1:waterAtLevel(H208, H209)], [bucketOverflows(H208), waterSpills], [2:height(H208, H223), 3:compute(H223 < H209)]))]),
		 predicateAppearsInAntecedentsOf(stepOn, [rule(rule4, causeWhenEnabledBy([1:stepOn(H177, H178)], [bitten(H177)], [2:isSnake(H178)])), rule(rule2, causeWhenEnabledBy([1:stepOn(H205, H206)], [seeFromHeight(H205, H211)], [1:height(H206, H219), 2:height(H205, H227), 3:compute(H211 is H219 + H227)]))]),
		 predicateAppearsInAntecedentsOf(height, [rule(rule2, causeWhenEnabledBy([1:stepOn(H177, H178)], [seeFromHeight(H177, H183)], [1:height(H178, H191), 2:height(H177, H199), 3:compute(H183 is H191 + H199)])), rule(rule1, causeWhenEnabledBy([1:waterAtLevel(H228, H229)], [bucketOverflows(H228), waterSpills], [2:height(H228, H243), 3:compute(H243 < H229)]))])
       ]
     ]).
     
test_query_2 :-
test('query/2',
	  [[%PosTest: Ensure that "procedural attachments" (computations not requiring fact-matching) work
	    query(compute(2 is 1 + 1),allowBackchaining,Trace),
	    Trace = ['compute',2 is 1 + 1,[]]
	    ],
	   [%PosTest: Another test of "procedural attachments"
	    query(compute(3 > 2),allowBackchaining,Trace),
	    Trace = ['compute',3 > 2,[]]
	    ],
	   [%NegTest: Ensure that "procedural attachments" fail when their computation returns false
	    not query(compute(3 is 1 + 1),allowBackchaining,Trace)
	    ],
	   [%Test: Ensure that queries satisfiable by a premise are in fact satisfied
	    assert(premise(p1,blue(sky))),
	    query(blue(X),allowBackchaining,Trace),
	    Trace = [p1,blue(sky),[]]
	    ],
	   [%Test: Ensure that queries satisfiable by a conclusion are in fact satisfied
	    assert(conclusion(haveWings(penguins), [rule1, haveWings(penguins), [[1, [p1, bird(penguins), []]]]])),
	    query(haveWings(X),allowBackchaining,Trace),
	    Trace = ['conclusion',haveWings(penguins),[rule1, haveWings(penguins), [[1, [p1, bird(penguins), []]]]]]
	    ],
	   [%Test: Ensure that queries satisfiable by backchaining through just one rule are in fact satisfied
	    addRule(rule5, <=([haveBeak5(X),haveWings5(X)], [1:bird5(X)])),
	    assert(premise(p5,bird5(penguins))),
	    query(haveWings5(_Y),allowBackchaining,Trace),
	    Trace = [rule5, haveWings5(_H198), [[1, [p5, bird5(penguins), '[]']]]]
	    ]
	  ]).
	  

test_queryFlattenedAnteListForRule_3 :-
test('queryFlattenedAnteListForRule/3',
	  [[%NegTest: Ensure that backchaining (in service of forward-chaining) requiring shuffling of antecedents DOESNT work when the anteList of the candidate rule has no match for the premiseOrConclusion
	    assert(premise(p1,greek(plato))),
	    not queryFlattenedAnteListForRule(premise(p2, philosopher(plato)), [1:unmarried(plato),2:male(plato)], _)
	    ],
	   [%NegTest: Ensure that backchaining (in service of forward-chaining) requiring shuffling of antecedents DOESNT work when support for antes isnt present beyond the premiseOrConclusion itself
	    assert(premise(p1,philosopher(plato))),
	    not queryFlattenedAnteListForRule(premise(p2, male(plato)), [1:unmarried(plato),2:male(plato)], _)
	    ],
	   [%PosTest: Ensure that backchaining (in service of forward-chaining)                                         : POC in 1st pos
	    assert(premise(p1,unmarried(plato))),
	    assert(premise(p2,greek(plato))),
	    queryFlattenedAnteListForRule(premise(p3, male(plato)), [1:male(plato),2:unmarried(plato),3:greek(plato)], AnteTrace),
	    AnteTrace = [[1, [p3, male(plato), '[]']], [2, [p1, unmarried(plato), '[]']], [3, [p2, greek(plato), '[]']]]
	    ],
	   [%PosTest: Ensure that backchaining (in service of forward-chaining) requiring shuffling of antecedents works: POC in middle
	    assert(premise(p1,unmarried(plato))),
	    assert(premise(p2,greek(plato))),
	    queryFlattenedAnteListForRule(premise(p3, male(plato)), [1:unmarried(plato),2:male(plato),3:greek(plato)], AnteTrace),
	    AnteTrace = [[2, [p3, male(plato), '[]']], [1, [p1, unmarried(plato), '[]']], [3, [p2, greek(plato), '[]']]]
	    ],
	   [%PosTest: Ensure that backchaining (in service of forward-chaining) requiring shuffling of antecedents works: POC in last pos
	    assert(premise(p1,unmarried(plato))),
	    assert(premise(p2,greek(plato))),
	    queryFlattenedAnteListForRule(premise(p3, male(plato)), [1:unmarried(plato),2:greek(plato),3:male(plato)], AnteTrace),
	    AnteTrace = [[3, [p3, male(plato), '[]']], [1, [p1, unmarried(plato), '[]']], [2, [p2, greek(plato), '[]']]]
	    ]]).
	  
test_fireFwdCheck_1 :-
test('fireFwdCheck/1',
      [[%Test: We disallow firing forward from premises with vars
        (fireFwd(premise1,mortal(_X)) ; true), %'true' here allows the following condition to be tested even if this call fails
        not premise(premise1,mortal(_Y))
        ],
       [%Test: Ensure we avoid adding same premise under diff name
        assert(premise(premise2,greek(socrates))),
        not fireFwd(premise3,greek(socrates)),
        not premise(premise3,greek(socrates))
        ],
       [%Test: Ensure we avoid adding same premise as a preexisting conclusion
        assert(conclusion(male(socrates),[])),
        not fireFwd(premise4,male(socrates)),
        not premise(premise4,male(socrates))
        ]
      ]).
      

test_fireFwd_2 :-
test('fireFwd/2',
      [[%Test: When the factbase is empty, firing forward a premise should assert at least the premise
        %writeL(['test1']),
        fireFwd(premise5,mortal(plato)),
        premise(premise5,mortal(plato))
        ],
       [%Test: Use verifyAssertions/3 to do the same check, plus also checking that the premise is the ONLY premise present
        %writeL(['test2']),
        verifyAssertions('1. Adding premise to empty kb should assert only itself',
								 fireFwd(p1,notBittenByVampire(socrates)),
							    [premise(p1, notBittenByVampire(socrates))] )
		  ],
       [%Test: Ensure we don't allow a naming conflict with another premise
        %writeL(['test3']),
        assert(premise(premise6,greek(plato))),
        not fireFwd(premise6,bald(plato)),
        not premise(premise6,bald(plato))
        ],
       [%Test: Ensure we don't allow a naming conflict with a rule
        %writeL(['test4']),
        assert(rule(rule1,<=(european(X),[1:greek(X)]))),
        not fireFwd(rule1,bald(plato)), %'rule1' is an unlikely label for a premise, but used here to test for naming conflict
        not premise(rule1,bald(plato))
        ],
       [%Test: Core test of firing forward: There is a rule that can be completely supported by the fireFwd trigger plus other facts
        %writeL(['test5']),
        addRule(rule2,<=(bachelor(X),[1:unmarried(X),2:male(X)])),
        assert(premise(premise7,unmarried(plato))),
        %(assert(traceReasoner) ; true), %DEBUG!!!!!!!!
        fireFwd(premise8,male(plato)),
        %(retract(traceReasoner) ; true), %DEBUG!!!!!!!!
        conclusion(bachelor(plato), [rule2, bachelor(plato), [[2, [premise8, male(plato), []]], [1, [premise7, unmarried(plato), []]]]])
        ],
       [%Test
        %writeL(['test6']),
        addRule(r1,<=(mortal(X),[:(1,human(X)),:(2,notBittenByVampire(X))])),
		  addRule(r2,<=(eligibleVoter(X),[:(1,adult(X)),:(2,human(X))])),
		  addRule(r3,<=(human(X),[:(1,selfAware(X))])),
		  verifyAssertions('2. Adding premise to kb with rules and other necessary premises should trigger all expected conclusions',
								 fireFwd(p2,selfAware(socrates)),
								 [premise(p2, selfAware(socrates)),
								  conclusion(human(socrates), [r3, human(socrates), [[1, [p2, selfAware(socrates),[]]]]]),
								  conclusion(mortal(socrates), [r1, mortal(socrates), [[1, [conclusion, human(socrates), [r3, human(socrates), [[1, [p2, selfAware(socrates), []]]]]]], [2, [p1, notBittenByVampire(socrates), []]]]])
								 ])
		  ],
       [%Test: Ensure "list of consequents" works when there is only one consequent
        %writeL(['test7']),
        addRule(rule3,<=([bachelor3(X)],[1:unmarried3(X),2:male3(X)])),
        assert(premise(premise9,unmarried3(plato))), %preds end in "3" here to avoid collision with similar preds, since we don't allow adding the same premise content more than once
        fireFwd(premise10,male3(plato)),
        conclusion(bachelor3(plato), [rule3, bachelor3(plato), [[2, [premise10, male3(plato), []]], [1, [premise9, unmarried3(plato), []]]]])
        ],
       [%Test: Ensure "list of consequents" works when there are multiple consequents
        %writeL(['test8']),
        addRule(rule4,<=([bachelor4(X),neurotic4(X),hasMoney4(X)],[1:unmarried4(X),2:male4(X)])),
        assert(premise(premise11,unmarried4(plato))),
        fireFwd(premise12,male4(plato)),
        conclusion(bachelor4(plato), [rule4, bachelor4(plato), [[2, [premise12, male4(plato), []]], [1, [premise11, unmarried4(plato), []]]]]),
        conclusion(neurotic4(plato), [rule4, neurotic4(plato), [[2, [premise12, male4(plato), []]], [1, [premise11, unmarried4(plato), []]]]]),
        conclusion(hasMoney4(plato), [rule4, hasMoney4(plato), [[2, [premise12, male4(plato), []]], [1, [premise11, unmarried4(plato), []]]]])
        ],
       [%Test: Ensure that when more than one rule can be activated, then the consequents of all such rules are asserted
        %writeL(['test9']),
        addRule(rule5a,<=(bachelor5a(X),[1:unmarried5(X),2:male5(X)])),
        addRule(rule5b,<=(shaves5b(X),[1:unelectrolycized5(X),2:male5(X)])),
        assert(premise(premise13,unmarried5(plato))),
        assert(premise(premise14,unelectrolycized5(plato))),
        fireFwd(premise15,male5(plato)),
        conclusion(bachelor5a(plato), [rule5a, bachelor5a(plato), [[2, [premise15, male5(plato), []]], [1, [premise13, unmarried5(plato), []]]]]),
        conclusion(shaves5b(plato), [rule5b, shaves5b(plato), [[2, [premise15, male5(plato), []]], [1, [premise14, unelectrolycized5(plato), []]]]])
        ],
       [%PosTest: Ensure that a general premise about a causal relation, along with premises about all the triggers and enablers
        %         required by that relation, lead to the causal outcome indicated by that relation.
        %writeL(['test10 - causal deduction']),
        %(assert(traceReasoner) ; true), %DEBUG!!!!!!!!
        addRule(bucketOverflowRule,causeWhenEnabledBy([1:waterAtLevel(B,L)],[bucketOverflows(B),waterSpills],[2:height(B,H),3:compute(H < L)])),
        assert(premise(height2,height(bucket1,2))),
        fireFwd(bucketLevel2,waterAtLevel(bucket1,3)),
        %(retract(traceReasoner) ; true), %DEBUG!!!!!!!!
        conclusion(bucketOverflows(bucket1), Trace1),
        Trace1 = [bucketOverflowRule, bucketOverflows(bucket1), [[1, [bucketLevel2, waterAtLevel(bucket1, 3), []]], [2, [height2, height(bucket1, 2), []]], [3, [compute, 2 < 3, []]]]],
        conclusion(waterSpills, Trace2),
        Trace2 = [bucketOverflowRule, waterSpills, [[1, [bucketLevel2, waterAtLevel(bucket1, 3), []]], [2, [height2, height(bucket1, 2), []]], [3, [compute, 2 < 3, []]]]]
        ]
      ]).