%TODO: Add intensity to desires, so higher-priority goals can cancel existing lower-priority intents
%TODO: Add intensity to beliefs so outer 'neg' is replaced by inner neg intensity (so opp-sign attitudes found when looking for same-sign)


%The role normally played by a desire in a BDI acct is here played either
% by 1) liking for a situation, or 2) having a duty to bring the situation about
addRules_isMotivatedAbout :-
(addRule(isMotivatedAbout_viaLiking_rule,
         <=(isMotivatedAbout(P,G,T), [1:likeThat(P,G,T)]) ) ;
 true ), %Disjunct w/true ensures that even if this rule is already loaded, calling this defn still succeeds
(addRule(isMotivatedAbout_viaObligation_rule,
         <=(isMotivatedAbout(P,G,T), [1:obligedToEnsure(P,G,T)]) ) ;
 true ).
 

%Before committing to an intention, we need to check whether it would conflict with something we
% are already committed to. This is a handy way of checking for such conflicts.
%TODO define conflict/2.
addRules_noConflictingIntent :-
(addRule(noConflictingIntent_viaNoExistingIntent_rule,
         %Must use 'fail' in query to prevent infinite regress starting from triggerIntentRule
         <=(noConflictingIntent(P,_,T), [1:compute(not query(intendWhenFor(P,do(P,_),_,_,T),dontAllowBackchaining,_QTrace))]) ) ;
 true ),
(addRule(noConflictingIntent_viaExistingIntentIsSame_rule,
         %Must use 'fail' in query to prevent infinite regress starting from triggerIntentRule
         <=(noConflictingIntent(P,G,T), [1:compute([ ground(G), findall(G2,query(intendWhenFor(P,do(P,_),_,G2,T),dontAllowBackchaining,_QTrace),[G]) ])]) ) ;
 true ),
(addRule(noConflictingIntent_viaExistingIntentDoesntConflict_rule,
         %Must use 'fail' in query to prevent infinite regress starting from triggerIntentRule
         <=(noConflictingIntent(P,G,T), [1:compute([ ground(G), findall(G2,query(intendWhenFor(P,do(P,_),_,G2,T),dontAllowBackchaining,_QTrace),GL), ground(GL), trueForAll(noConflict,G,GL) ])]) ) ;
 true ).
        
        
%triggering of intent
addRules_triggerIntent :-
addRules_isMotivatedAbout,
addRules_noConflictingIntent,
addRule(triggerIntentRule,
		  causeWhenEnabledBy([1:isMotivatedAbout(P,G,T),
		                      2:believe(P, not incipiently(G), T),
		                      3:believe(P, causeWhenEnabledBy([:(_,do(Q,A))],G,[:(_,C)]), T),
		                      4:believe(P, eventually(C), T) %C includes ability to do A
		                     ],
		                     [intendWhenFor(P,do(P,A),C,G,T2)
		                     ],
		                     [5:compute((var(Q); Q = P)), %Ensure causal rule in trigger is not specific to someone other than P
		                      6:compute(query(noConflictingIntent(P,G,T),allowBackchaining,_)), %We use compute/query to get around the restriction that fore-chaining doesn't allow direct backchaining
		                      7:compute(pairwiseSum(T,1,T2))
		                     ])).


addRules_cancelIntent_loseMotivation :-
addRule(cancelIntentRule_loseMotivation,
		  causeWhenEnabledBy([1:intendWhenFor(P,doWhen(P,A),C,G,T),
                            2:neg(isMotivatedAbout(P,G,T)) %maybe P realizes G has a bad consequence
                           ],
                      		[neg(intendFor(P,doWhen(P,A),C,G,T2))
                      		],
                   			[3:compute(pairwiseSum(T,1,T2))
                   			])).

%TODO make sure that the reason one thinks G is incipient isn't because one is intending/attempting it!!
addRules_cancelIntent_expectLuck :-
addRule(cancelIntentRule_expectLuck,
		  causeWhenEnabledBy([1:intendWhenFor(P,doWhen(P,A),C,G,T),
                            2:believe(P,incipiently(G),T) %maybe it appears G will happen without P's effort
                           ],
                      		[neg(intendFor(P,doWhen(P,A),C,G,T2))
                      		],
                   			[3:compute(pairwiseSum(T,1,T2))
                   			])).
                           
addRules_cancelIntent_doubtKnowledge :-
addRule(cancelIntentRule_doubtKnowledge,
		  causeWhenEnabledBy([1:intendWhenFor(P,doWhen(P,A),C,G,T),
                            2:neg(believe(P,cause(doWhen(Q,A),C,G),T)) %maybe one discovers one's causal knowledge is faulty
                           ],
                      		[neg(intendFor(P,doWhen(P,A),C,G,T2))
                      		],
                   			[3:compute((var(Q); Q = P)), %Ensure causal rule in trigger is not specific to someone other than P
                   			 4:compute(pairwiseSum(T,1,T2))
                   			])).
                           
addRules_cancelIntent_doubtPossibility :-
addRule(cancelIntentRule_doubtPossibility,
		  causeWhenEnabledBy([1:intendWhenFor(P,doWhen(P,A),C,G,T),
                            2:neg(believe(P,eventually(C),T)) %C includes ability to do A; maybe one discovers one will never have the requisite ability
                           ],
                      		[neg(intendFor(P,doWhen(P,A),C,G,T2))
                      		],
                   			[3:compute(pairwiseSum(T,1,T2))
                   			])).
%note: P could be blamed for "giving up too early" on himself, or about changing his beliefs or likes too readily


/*
%Would be nice if the shared initial antes with triggerIntentRule could be indexed such that when we find whether ante4 is supported, we could continue with triggerIntentRule or continue here
addRules_triggerSubgoal :-
addRule(triggerSubgoalRule,
		  causeWhenEnabledBy([1:isMotivatedAbout(P,G,T),
		                      2:believe(P, not incipiently(G), T),
		                      3:believe(P, causeWhenEnabledBy([:(_,do(Q,A))],G,[:(_,C)]), T),
		                      4:not believe(P, eventually(C), T) %C includes ability to do A
		                     ],
		                     [isMotivatedAbout(P,C,T)
		                     ],
		                     [5:compute((var(Q); Q = P)), %Ensure causal rule in trigger is not specific to someone other than P
		                      6:compute(pairwiseSum(T,1,T2))
		                     ])).
		                     
%triggering of intentional attempt
causeWhenEnabledBy([intendFor(P,doWhen(P,A,C),G,T),
                    believe(P,C,T)],
                   [attempt(P,G,A,C,T2),
                    believe(P,doWhen(P,A,C),T2) ], %when one starts an attempt, one thinks one is succeeding at it
                   [])
:- pairwiseSum(T,1,T2).
       
%triggering of expectation of attempt outcome            
causeWhenEnabledBy([believe(Q,attempt(P,_G1,A,_C1,T),T), %Q is P or an onlooker
                    believe(Q,cause(doWhen(P,A,C),G),T), %G and C might not be what P intended
                    believe(Q,C,T)],
                   [believe(Q,incipiently(hasOutcome(attempt(P,A),G)),T2)],
                   [])
:- pairwiseSum(T,1,T2).

%trigger interest in understanding P's motives if doing A during C doesn't seem in P's self-interest?
                   
%trigger interest in understanding cause of failure
causeWhenEnabledBy([believe(P,incipiently(hasOutcome(attempt(P,A),G)),T2),
                    believe(P,not(G),T)],
                   [neg(believe(P,cause(doWhen(Q,A,C),G),T3)), %prevent same plan from firing while trying to explain its failure
                    like(P,explainAndCorrectFor(neg(cause(doWhen(Q,A,C),G))),T3)],
                   [])
:- pairwiseSum(T2,1,T),
   pairwiseSum(T,1,T3).
*/

/*--------------------------------------
 *
 * Non-intentional causes of action (with one exception)
 *
 *--------------------------------------*/

/*
%TODO find way to state generalization: "if conditions differ from expectations, an attempt to do A may become a doing of B instead (or may be a successful A)"
causeWhenEnabledBy([attempt(P,touchCeiling,raiseArm,physFact(empty(spaceAboveArmBelowCeiling))],
                   [do(P,punchInFace(Q))],
                   [physFact(at(faceOf(Q),spaceAboveArmBelowCeiling))] ).
                   
%TODO provide an account of unintended side-effects
                   
causeWhenEnabledBy([hasPhysicalReflex(P,C,A), %eg, spasm; duck when hearing low noise overhead
                    physFact(C) ],
                   [do(P,A)],
                   [] ).
causeWhenEnabledBy([hasLearnedResponse(P,A,C), %eg, Pavlov's dog
                    believe(P,C) ],
                   [do(P,A)],
                   [] ).
causeWhenEnabledBy([hasAutoExpression(P,E,A,C), %eg, slam table when angry and a table is in reach
                    feelEmotionally(P,E),
                    believe(P,C) ],
                   [do(P,A)],
                   [] ).
causeWhenEnabledBy([hasGoalDirectedHabit(P,G,A,C), %eg, return tennis ball with racket; spider crossing web
                    like(P,G),
                    believe(P,C) ],
                   [do(P,A)],
                   [] ).
%TODO? find way to state generalization: "sometimes physical forces/situations lead us to do something"
causeWhenEnabledBy([physFact(ropesTyingToChair(P))],
                   [do(P,stayInChair)]
                   [] ).
causeWhenEnabledBy([physFact(neg(hasFootOrHandholdOnStairs(P)))],
                   [do(P,fallDownStairs)]
                   [] ).
*/