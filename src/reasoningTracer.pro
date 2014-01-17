%ATOM/src/reasoningTracer.pro

:- import(lists). %Used for member/2 and append/3

:- dynamic( traceReasoner/0 ).

:- dynamic( traceFailedConstraints/0 ).

newAssertionsList([]).

%See http://www.amzi.com/ExpertSystemsInProlog/05forward.htm#implementation
%:- op(230, xfx, <=).
%:- op(32, xfy, :).
%:- op(250, fx, rule).
%rule r1: mortal(X) <= [1:human(X), 2:notBittenByVampire(X)].


%DOC
% Assert the given fact into the db (with given label) if it turns out to be new information.
% Then, deduce as far as possible from the new fact.
%
fireFwd(PremiseLabel,Fact) :-
	condWriteL(traceReasoner,['FF ',PremiseLabel,'; ',Fact]),
	!,(ground(PremiseLabel) ;
	   (errorL(['ERROR: Premise label <',PremiseLabel,'> must be ground']),
	    !,fail )),
	retractall(movedToEndOfIndexForPred(_,_)), %Remove all 'moved' pairs to avoid interference from any previous fireFwd from user
	!,fireFwdPremise(PremiseLabel,Fact).

   
%DOC
% Does the majority of the work for fireFwd/2
%
fireFwdPremise(PremiseLabel,Fact) :-
	condWriteL(traceReasoner,['  FFP ',PremiseLabel,'; ',Fact]),
	!,fireFwdCheck(Fact),
	%Make sure the suggested label isn't already in use
	!,(premise(PremiseLabel,AnyFact) ->
	  	(errorL(['ERROR: <',PremiseLabel,'> is already used for <',premise(PremiseLabel,AnyFact),'>']),
	    !,fail ) ;
	   true ),
	!,(rule(PremiseLabel,AnyRule) ->
	   (errorL(['ERROR: <',PremiseLabel,'> is already used for <',rule(PremiseLabel,AnyRule),'>']),
	    !,fail ) ;
	   true ),
	%We don't attempt any truth maintenance by checking for negations of Fact; assume all facts refer to a timepoint
	A = premise(PremiseLabel,Fact),
	!,makeAndAnnounceAssertion(A),
	%Find any rules that can be used to assert conclusions
	(fireFwdUsingRules(A) ; true), %fireFwdUsingRules should be considered to always succeed, so avoid printing 'No' when done
	nl.

   
%DOC
%Checks whether a candidate fact for the db is fully ground, and doesn't match a term already in the db.
%
fireFwdCheck(Fact) :-
	condWriteL(traceReasoner,['  FFCh ',Fact]),
	!,(ground(Fact) ->
	   true ;
	   (errorL(['ERROR: We dont yet allow firing from universally-quantified facts like <',Fact,'>']),
	    !,fail )),
	!,wouldBeNew(conclusion(Fact,_AnyTrace)),
	!,wouldBeNew(premise(_AnyLabel,Fact)).


wouldBeNew(Fact) :-
	condWriteL(traceReasoner,['   WBN ',Fact]),
	!,not clause(Fact,true). %fire fwd only if new; to see if new, search only facts and avoid RHS's

   
%DOC
%Deduces as far forward as possible from the given premise or conclusion.
%Note that the given premise is assumed to be wrapped with premise() and a given conclusion is assumed to be wrapped with conclusion().
%
fireFwdUsingRules(PremiseOrConclusion) :-
	condWriteL(traceReasoner,['  FFUR ',PremiseOrConclusion]),
	!,unwrapPremiseOrConclusion(PremiseOrConclusion,UnwrappedPremiseOrConclusion),
	%RRF is meant to be backtracked into so that RL/C/AL are assigned accordingly for every rule that is relevant to Trigger
	!,relevantRuleForward(UnwrappedPremiseOrConclusion, RuleLabel, Consequent, AnteList),
	transferBindsAndAllowBacktracking(:(_,UnwrappedPremiseOrConclusion),AnteList), %Make sure all bindings in the PremiseOrConclusion are transfered to Consequent
	wouldBeNew(movedToEndOfIndexForPred(PremiseOrConclusion, RuleLabel)), %prevent firing from same facts
	condWriteL(traceReasoner,['   FFUR1 al:',AnteList,' at:',AnteTrace]),
	queryAnteListForRule(PremiseOrConclusion,AnteList,AnteTrace),
	%We want to allow Consequent to be either a single term or a list of terms
	% Doing so is syntactic sugar, since one could also just specify multiple rules with identical antecedents but diff single-term consequents
	(Consequent = [Conseq|ConseqsL] ->
	 (Trace = [RuleLabel,Conseq,AnteTrace],
	  (fireFwdConclusion(Conseq,Trace,PremiseOrConclusion,RuleLabel) ; true),
	  fireFwdConclusionL(ConseqsL,AnteTrace,PremiseOrConclusion,RuleLabel) ) ;
	 (Trace = [RuleLabel,Consequent,AnteTrace],
	  fireFwdConclusion(Consequent,Trace,PremiseOrConclusion,RuleLabel) )).
 
 
%DOC Strip off the 'premise' or 'conclusion' wrapper
%
unwrapPremiseOrConclusion(premise(_,UPOC), UPOC).
unwrapPremiseOrConclusion(conclusion(UPOC,_), UPOC).


%DOC
relevantRuleForward(Trigger, RuleLabel, Consequent, AnteList) :-
	functor(Trigger,Predicate,_),
	!,predicateAppearsInAntecedentsOf(Predicate, RelevantRuleList),
	!,member(Rule,RelevantRuleList), %Callers of RRF can backtrack to this point to get the next relevant rule
	(Rule = rule(RuleLabel,<=(Consequent,AnteList)) ;
	 (Rule = rule(RuleLabel,causeWhenEnabledBy(Triggers,Consequent,Enablers)),
	  append(Triggers,Enablers,AnteList) )).
  
  
%DOC
relevantRuleBackward(Trigger, RuleLabel, Consequent, AnteList) :-
	functor(Trigger,Predicate,_),
	!,predicateAppearsInConsequentsOf(Predicate, RelevantRuleList),
	!,member(Rule,RelevantRuleList), %Callers of RRF can backtrack to this point to get the next relevant rule
	(Rule = rule(RuleLabel,<=(Consequent,AnteList)) ;
	 (Rule = rule(RuleLabel,causeWhenEnabledBy(Causes,Consequent,Enablers)),
	  append(Causes,Enablers,AnteList) )).
  
 
%This would allow QALFR to succeed even if PremiseOrConclusion didn't appear in the full ante list,
% so we must keep this out of the kb.  
%queryAnteListForRule(_PremiseOrConclusion,[],[]).
   

%DOC
%Checks that all the antecedents are supported by either a premise or conclusion. No backchaining further than that is allowed,
% because it's expected that if that were necessary, trying so now would fail because otherwise the conclusions would be present.
% Some new premise later on might provide the missing conclusion, so we'd revisit this ante list then. Eventually, the rule would
% fire once all its antecedents are supported.
%This check also requires that one of the antecedents be supported by the provided PremiseOrConclusion; without this, checking
% the ante list would have no relevance to the item currently being "fired forward".
%
queryAnteListForRule(PremiseOrConclusion,[:(AnteLabel,Ante)|AnteList],AnteTrace) :-
	condWriteL(traceReasoner,['  QALFR poc:',PremiseOrConclusion,' a:',Ante,' ala:',AnteLabel,' ali:',AnteList]),
	!,flattenAntecedentTree([:(AnteLabel,Ante)|AnteList],1,FlattenedAnteTree,[]),
	condWriteL(traceReasoner,['        fat:',FlattenedAnteTree,' at:',AnteTrace]),
	!,queryFlattenedAnteListForRule(PremiseOrConclusion,FlattenedAnteTree,AnteTrace).
  
  
queryFlattenedAnteListForRule(PremiseOrConclusion, [:(AnteLabel,Ante)|AnteList], [AnteTrace1|AnteTrace2]) :-
	condWriteL(traceReasoner,['  QFALFR poc:',PremiseOrConclusion,' ala:',AnteLabel,' a:',Ante,' ali:',AnteList]),
	!,anteListRemainingAfterPOCSupport(PremiseOrConclusion,[:(AnteLabel,Ante)|AnteList],RemainingAnteList,AnteTrace1),
	!,queryAnteList(RemainingAnteList,fail,AnteTrace2). %'fail' means don't use any backchaining; support must already exist "at next level down"


anteListRemainingAfterPOCSupport(PremiseOrConclusion,[:(AnteLabel,Ante)|AnteList], AnteList, [AnteLabel,[PremiseLabel,Ante,[]]]) :-
	PremiseOrConclusion = premise(PremiseLabel,Ante), %tests if poc says anything about ante
	condWriteL(traceReasoner,['   p:poc <',PremiseLabel,'> satisfies this ante:',Ante]).
  
anteListRemainingAfterPOCSupport(PremiseOrConclusion,[:(AnteLabel,Ante)|AnteList], AnteList, [AnteLabel,['conclusion',Ante,Trace]]) :-
	PremiseOrConclusion = conclusion(Ante,Trace),     %tests if poc says anything about ante
	condWriteL(traceReasoner,['   c:poc <conclusion> satisfies this ante:',Ante]).

%DOC
% Otherwise, postpone this ante and see if poc can be matched to any other ante's in the list
%
anteListRemainingAfterPOCSupport(PremiseOrConclusion,
								 [:(AnteLabel,Ante)|[:(AnteListHeadLabel,AnteListHead)|AnteListTail]],
								 [:(AnteLabel,Ante)|RemainingAnteList],
								 AnteTrace ) :-
	condWriteL(traceReasoner,['   rot:poc:',PremiseOrConclusion,' a:',[:(AnteLabel,Ante)|[:(AnteListHeadLabel,AnteListHead)|AnteListTail]]]),
	anteListRemainingAfterPOCSupport(PremiseOrConclusion,[:(AnteListHeadLabel,AnteListHead)|AnteListTail],RemainingAnteList,AnteTrace).
 
  
%DOC
%Find all leaves in the Ante tree, assign each a label based on AnteLabel, then flatten
% for use as 2nd arg to queryFlattenedAnteListForRule.
%4th arg is a list of what's already been flattened. We need to pass such a 4th arg to each
% subcall (where just one item will be prepended (or none will)), because otherwise we would
% collect a sublist from each subcall, and then concat/embed those sublists in the right way
% to keep the result flat. That seems tricky; instead, this approach takes the risk of using
% too much stack space to pass such sublists.
%
flattenAntecedentTree([],_,FlattenedAlready,FlattenedAlready).

flattenAntecedentTree([:(AnteLabel,Ante)|AnteList],AnteHeadIndex,FlattenedAnteTree,FlattenedAlready) :-
	%writeL(['FAT a:',Ante,' ala:',AnteLabel,' ali:',AnteList,' ahi:',AnteHeadIndex,' fa:',FlattenedAlready]),
	((var(AnteList),
	  FlattenedAnteListTree = AnteList ) ;
	 flattenAntecedentTree(AnteList,1,FlattenedAnteListTree,FlattenedAlready) ),
	!,
	((var(Ante),
	  %writeL([' case1 falt:',FlattenedAnteListTree]),
	  FlattenedAnteTree = [:(AnteLabel,Ante)|FlattenedAnteListTree]
	  %,writeL([' case1 => fat:',FlattenedAnteTree])
	  );
	 (Ante = [AnteHead|AnteTail],
	  %writeL([' case2 ah:',AnteHead,' at:',AnteTail,' falt:',FlattenedAnteListTree]),
	  createSubLabel(AnteLabel,AnteHeadIndex,AnteHeadLabel),
	  flattenAntecedentTree([:(AnteHeadLabel,AnteHead)],1,FlattenedAnteHeadTree,FlattenedAnteListTree),
	  flattenAntecedentTailTree(AnteLabel,2,AnteTail,FlattenedAnteTree,FlattenedAnteHeadTree)
	  %,writeL([' case2 => fat:',FlattenedAnteTree])
	  ) ;
	 (%writeL([' case3 falt:',FlattenedAnteListTree]),
	  FlattenedAnteTree = [:(AnteLabel,Ante)|FlattenedAnteListTree]
	  %,writeL([' case3 => fat:',FlattenedAnteTree])
	  )).


flattenAntecedentTailTree(AnteLabel,TailHeadIndex,AnteTail,FlattenedAnteTree,FlattenedAlready) :-
	%writeL(['FATT at:',AnteTail,' ala:',AnteLabel,' thi:',TailHeadIndex,' fa:',FlattenedAlready]),
	((var(AnteTail),
	  createSubLabel(AnteLabel,TailHeadIndex,TailHeadLabel),
	  append(FlattenedAlready,[:(TailHeadLabel,AnteTail)],FlattenedAnteTree) ) ;
	 (AnteTail = [],
	  FlattenedAnteTree = FlattenedAlready ) ;
	 (AnteTail = [AnteTailHead|AnteTailTail],
	  TailTailHeadIndex is (1+ TailHeadIndex),
	  flattenAntecedentTailTree(AnteLabel,TailTailHeadIndex,AnteTailTail,FlattenedAnteTailTailTree,FlattenedAlready),
	  createSubLabel(AnteLabel,TailHeadIndex,TailHeadLabel),
	  flattenAntecedentTree([:(TailHeadLabel,AnteTailHead)],1,FlattenedAnteTree,FlattenedAnteTailTailTree) )).


createSubLabel(Label,SubIndex,SubLabel) :-
	%writeL(['CSL l:',Label,' si:',SubIndex]),
	(string(Label) ->
	 LabelAsString = Label ;
	 term_string(Label,LabelAsString) ),
	(string(SubIndex) ->
	 SubIndexAsString = SubIndex ;
	 term_string(SubIndex,SubIndexAsString) ),
	concat_string([LabelAsString,'.',SubIndexAsString],SubLabel).


%DOC
%Adds the given consequent to the db if it's new information.
%Then it does some bookkeeping by asserting that the trigger (a premise
% or conclusion) has been used with a certain rule, so we can avoid triggering
% the same combination during this deduction effort.
%Finally it uses the new fact (consequent) as a trigger for further deduction.
%
fireFwdConclusion(Consequent,Trace,Trigger,RuleLabel) :-
	condWriteL(traceReasoner,['  FFCo ',Consequent,'; ',Trace,'; ',Trigger,'; ',RuleLabel,'; ']),
	!,fireFwdCheck(Consequent),
	%We don't attempt any truth maintenance by checking for negations of Fact; assume all facts refer to a timepoint
	A = conclusion(Consequent,Trace),
	!,makeAndAnnounceAssertion(A),
	!,moveToEndOfIndexForPred(Trigger,RuleLabel),
	%Find any rules that can be used to assert conclusions
	!,fireFwdUsingRules(A).
       
fireFwdConclusionL([],_AnteTrace,_PremiseOrConclusion,_RuleLabel).

fireFwdConclusionL([Conseq|ConseqsL],AnteTrace,PremiseOrConclusion,RuleLabel) :-
	condWriteL(traceReasoner,['  FFCoL ',[Conseq|ConseqsL],'; ',AnteTrace,'; ',PremiseOrConclusion,'; ',RuleLabel,'; ']),
	Trace = [RuleLabel,Conseq,AnteTrace],
	!,(fireFwdConclusion(Conseq,Trace,PremiseOrConclusion,RuleLabel); true),
	!,fireFwdConclusionL(ConseqsL,AnteTrace,PremiseOrConclusion,RuleLabel).
   
%DOC
%Supports backchaining (which traditionally does not assert any of the
% conclusions reached during inference).
%
query([],_AllowBackchaining,[]).

query(compute([]),_AllowBackchaining,['compute',[],[]]) :- !.

query(compute([Query|QList]),AllowBackchaining,['compute',[Query|QList],[]]) :-
	once(condWriteL(traceReasoner,['  Qcl ',compute([Query|QList])])),
	!,query(compute(Query),AllowBackchaining,_),
	!,query(compute(QList),AllowBackchaining,_).

query(compute(Query),_AllowBackchaining,['compute',Query,[]]) :-
	Query \= [_|_],
	once(condWriteL(traceReasoner,['  Qcc ',compute(Query)])),
	!,call(Query).

query(Query,_AllowBackchaining,[PremiseLabel,Query,[]]) :-
	once(condWriteL(traceReasoner,['  Qpr ',Query])),
	once(premise(PremiseLabel,Query)),
	!.

query(Query,_AllowBackchaining,['conclusion',Query,Trace]) :-
	once(condWriteL(traceReasoner,['  Qco ',Query])),
	once(conclusion(Query,Trace)),
	!.

query(Query,allowBackchaining,[RuleLabel,Query,AnteTrace]) :-
	condWriteL(traceReasoner,['  Qbc ',Query]),
	!,relevantRuleBackward(Query, RuleLabel, Consequents, AnteList),
	transferBindsAndAllowBacktracking(Query,Consequents), %Make sure all bindings in the Query are transfered to AnteList via Consequent
	queryAnteList(AnteList,true,AnteTrace). %'true' means "allow backchaining"


%DOC
%This is used both when forward-chaining and when backward-chaining. In forward-chaining, you have a premise or
% conclusion for which you've found a rule with a matching ante, and you need to make sure the binds in the
% premise or conclusion are transfered into the rule via the matching ante. In backward-chaining, you have a query
% for which you've found a rule, and you need to make sure the binds in the query are transferred into the rule
% via the matching consequent.
%Allow backtracking into this rule because multiple generalized propos (e.g. multiple antes of same rule) could
% match and differ only in vars that fill slots.
%
transferBindsAndAllowBacktracking(BoundPropo,[GeneralizedProposHead|GeneralizedProposTail]) :-
	!,member(BoundPropo,[GeneralizedProposHead|GeneralizedProposTail]).

transferBindsAndAllowBacktracking(BoundPropo,GeneralizedPropo) :-
	!,GeneralizedPropo = BoundPropo.


   
queryAnteList([],_,[]).

%This defn of QAL flattens the antecedents from a nested list to a flat list (for use by following QAL defn)
%
queryAnteList([:(AnteLabel,[AnteHead|AnteTail])|AntesList],AllowBackchaining,AnteTrace) :-
	condWriteL(traceReasoner,['  QALa bc? ',AllowBackchaining,' ah:',AnteHead,' at:',AnteTail,' ala:',AnteLabel,' ali:',AntesList]),
	!,createSubLabel(AnteLabel,1,AnteHeadLabel),
	createSubLabel(AnteLabel,2,AnteTailLabel),
	queryAnteList([:(AnteHeadLabel,AnteHead)|[:(AnteTailLabel,AnteTail)|AntesList]],AllowBackchaining,AnteTrace),
	!.

queryAnteList([:(AnteLabel,Ante)|AntesList],AllowBackchaining,[[AnteLabel,Trace]|AnteTrace]) :-
	Ante \= [_|_],
	condWriteL(traceReasoner,['  QALb bc? ',AllowBackchaining,' a:',Ante,' ala:',AnteLabel,' ali:',AntesList]),
	!,
	(query(Ante,AllowBackchaining,Trace)
	 ;
	 %This would print out a constraint if the constraint cannot be satisfied. Note that constraints that
	 %initially succeed will also be printed out if a constraint that is further down the constraint list
	 %fails and backtracking causes these earlier constraints to be considered to have failed as well.
	 %We cannot use the if then else construct with the query as the If, 'true' as the Then and the condWriteL
	 %followed by 'fail' as the Else, because we want to allow query to be satisfied more than once and the
	 %if then else construct cuts into the If clause as soon as it is satisfied once.    
	 condWriteL(traceFailedConstraints,[' Failed constraint: antelabel: ',AnteLabel,' ante: ',Ante]),
	 fail
	), 
	queryAnteList(AntesList,AllowBackchaining,AnteTrace),
	!.


%DOC
%Bookkeeping to prevent a fact from triggering the same consequent
% multiple times.
%
moveToEndOfIndexForPred(PremiseOrConclusion,RuleLabel) :-
	condWriteL(traceReasoner,['  MTEOIFP ',PremiseOrConclusion,'; ',RuleLabel]),
	retract(PremiseOrConclusion),
	assertz(PremiseOrConclusion), %move this fact to the end of its pred's index so similar facts have a chance to trigger things
	asserta(movedToEndOfIndexForPred(PremiseOrConclusion,RuleLabel)),
	!.
   
%DOC
%An modularized design for all the extra stuff we want to do whenever
% asserting something through inference. For example, we want to build
% a list of everything asserted so that verifyAssertions/3 can check that
% all and only what a test expected actually was asserted.
%
makeAndAnnounceAssertion(Fact) :-
	condWriteL(traceReasoner,['  MAAA ',Fact]),
	asserta(Fact),
	(hideAppNoticesFlag ; nl,writeL(['Asserted <',Fact,'>'])),
	newAssertionsList(OldList), %Update the new assertions list by adding the new one
	retract(newAssertionsList(OldList)),
	assert(newAssertionsList([Fact|OldList])),
	updateIndices(Fact),
	!.


%DOC
%Whenever we try to fire forward from a consequent, or backchain to support an antecedent, we should look only at rules
% that have a matching antecedent or consequent, respectively. That kind of match seems too expensive to compute "live",
% so for every predicate that appears in any rule, we cache two lists: first, all the rules where the predicate appears
% in an antecedent; second, all the rules where the predicate appears in a consequent. So during inference, one can
% access all relevant rules in a single retrieval step using just the predicate.
%
updateIndices(Fact) :-
	((Fact = rule(_RuleLabel,<=(Consequent,AnteList)) ;
	  (Fact = rule(_RuleLabel,causeWhenEnabledBy(Triggers,Consequent,Enablers)),
	   append(Triggers,Enablers,AnteList) )),
	 ((Consequent = [_|_],
	   updateIndicesOverTerms(Consequent, Fact, predicateAppearsInConsequentsOf) ) ;
	  updateIndicesOverTerms([Consequent], Fact, predicateAppearsInConsequentsOf) ),
	 updateIndicesOverTerms(AnteList, Fact, predicateAppearsInAntecedentsOf) ) ;
	true.

updateIndicesOverTerms([], _, _).

updateIndicesOverTerms([Term|Terms], Rule, StorageFunctor) :-
	updateIndicesOverTerms1(Term, Rule, StorageFunctor),
	!,updateIndicesOverTerms(Terms, Rule, StorageFunctor).

updateIndicesOverTerms1(compute(_Expr),_,_). %Dont index 'compute' propos, since they are satisfied via "procedural attachment"

updateIndicesOverTerms1(:(_AnteLabel,Ante), Rule, StorageFunctor) :- %Unpack labelled antes
	updateIndicesOverTerms1(Ante, Rule, StorageFunctor).

updateIndicesOverTerms1(Term, Rule, StorageFunctor) :-
	functor(Term,Predicate,_),
	StorageProbe =.. [StorageFunctor,Predicate,PreviouslyStoredRules],
	updateIndex(StorageProbe,StorageFunctor,Predicate,Rule,PreviouslyStoredRules)
	/* There's a bug in this section, but instead of fixing it, I replaced it with a set of rules below
	((call(StorageProbe), %if successful, PreviouslyStoredRules should now be bound
	  (is_member(Rule, PreviouslyStoredRules) ; %if already a member, don't add rule again
	   (retract(StorageProbe),
	    NewStorage =.. [StorageFunctor,Predicate,[Rule|PreviouslyStoredRules]],
	    assert(NewStorage) ))) ;
	 (NewStorage =.. [StorageFunctor,Predicate,[Rule]] ),
	  assert(NewStorage) ))*/
	.

updateIndex(StorageProbe,StorageFunctor,Predicate,Rule,PreviouslyStoredRules) :-
	call(StorageProbe), %if successful, PreviouslyStoredRules should now be bound
	!,updateIndex1(StorageProbe,StorageFunctor,Predicate,Rule,PreviouslyStoredRules),
	!.
	updateIndex(_,StorageFunctor,Predicate,Rule,_) :-
	!,updateIndex2(StorageFunctor,Predicate,[Rule]).

updateIndex1(_,_,_,Rule,PreviouslyStoredRules) :-
	unifiableWithMemberOf(Rule, PreviouslyStoredRules,_,_), %if already a member, don't add rule again
	!.
	updateIndex1(StorageProbe,StorageFunctor,Predicate,Rule,PreviouslyStoredRules) :-
	retract(StorageProbe),
	updateIndex2(StorageFunctor,Predicate,[Rule|PreviouslyStoredRules]),
	!.
    
updateIndex2(StorageFunctor,Predicate,Rules) :-
	NewStorage =.. [StorageFunctor,Predicate,Rules],
	!,assert(NewStorage).
   
%DOC
% Does a few helpful checks before asserting the rule
%
addRule(RuleLabel,Defn) :-
	condWriteL(traceReasoner,['  AR ',RuleLabel,'; ',Defn]),
	!,(Defn = <=(_Consequent,_AnteList) ;
		Defn = causeWhenEnabledBy(_Triggers,_Consequent,_Enablers) ;
	   (errorL(['ERROR: <',RuleLabel,'> must have form <=(Consequent,AnteList) or causeWhenEnabledBy(Triggers,Consequent,Enablers)']),
	    !,fail )),
	!,(ground(RuleLabel) ;
	   (errorL(['ERROR: Rule label <',RuleLabel,'> must be ground']),
	    !,fail )),
	%Make sure the suggested label isn't already in use
	!,(not premise(RuleLabel,_AnyFact) ;
	   (errorL(['ERROR: <',RuleLabel,'> is already used for <',premise(RuleLabel,_AnyFact),'>']),
	    !,fail )),
	!,(not rule(RuleLabel,_AnyRule) ;
	   (errorL(['ERROR: <',RuleLabel,'> is already used for <',rule(RuleLabel,_AnyRule),'>']),
	    !,fail )),
	%We don't attempt any truth maintenance by checking for negations of Fact; assume all facts refer to a timepoint
	A = rule(RuleLabel,Defn),
	!,makeAndAnnounceAssertion(A),nl.

   
%DOC
%"Inference goal" should be an instance of fireFwd/2 or query/2.
%The assertions list built by makeAndAnnounceAssertion/1 is cleared and then
% the goal is called, so we can compare the actual assertions made by the
% call with the provided "target assertions list". The two lists are compared
% so that ordering doesn't matter, and if any variables are present in any
% list items they are unified while not adding to the binds list (because binds
% for one list item should not interfere with the vars of another list item).
%If the two lists are matchable, we print SUCC; otherwise, we print FAIL and
% print which items were unexpected (if any) and which target items couldn't
% be matched (if any) -- one of these two lists must be nonempty in the case
% of a failure.
%
verifyAssertions(TestDescrip, InferenceGoal, TargetAssertsList) :-
	(not(ground(TargetAssertsList)) ->
	 (errorL(['ERROR: Target list of assertions should not contain vars']),
	  !,fail ) ;
	 true ),
	!,resetAssertionsList,
	!,call(InferenceGoal),
	!,newAssertionsList(ActualAssertsList),
	!,listsUnifiableDisregardingOrder(ActualAssertsList, TargetAssertsList, UnmatchedActualAsserts, UnmatchedTargetAsserts),
	!,(( UnmatchedActualAsserts = [],
	     UnmatchedTargetAsserts = [],
	     nl,write('SUCC: '),write(TestDescrip) ) ;
	   ( nl,write('FAIL: '),write(TestDescrip),
	     (UnmatchedActualAsserts = []; writeL(['  unexpected asserts: ',UnmatchedActualAsserts])),
	     (UnmatchedTargetAsserts = []; writeL(['  missing asserts: ',UnmatchedTargetAsserts])) )).
	
	
resetAssertionsList :-
	newAssertionsList([]) ;
	(retract(newAssertionsList(_)), assert(newAssertionsList([]))).
