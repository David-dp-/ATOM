/* TODO
 * - Generate summary report from tests
 * - Figure out why test_fireFwd/2 prints the "bachelor conclusion" even though test/2 should
 *   have suppressed it by asserting hideAppNoticesFlag
 */
 
%Load & test 'utils'
:- ['../src/utils.pro', 'testUtils.pro'].
:- test_testLL1_4.
:- test_unifiableWithMemberOf_4.
:- test_listsUnifiableDisregardingOrder_4.
:- test_noConflict_2.
:- test_trueForAll_3.

%Load & test 'reasoningTracer'
:- ['../src/reasoningTracer.pro', 'testReasoningTracer.pro'].
:- test_flattenAntecedentTree_4.
:- test_updateIndex_5.
:- test_updateIndices_1.
:- test_query_2.
:- test_queryFlattenedAnteListForRule_3.
:- test_fireFwdCheck_1.
:- test_fireFwd_2.

%Load & test 'goalMonitoring'
:- ['../src/goalMonitoring.pro', 'testGoalMonitoring.pro'].
:- test_isMotivatedAbout_3.
:- test_noConflictingIntent_3.
:- test_triggerIntentRule.

/*
atomTrace :-
  tell('atomTrace.txt'),
  set_mode(trace, on),
  test_causation1Rule,
  set_mode(trace, off),
  told.
*/