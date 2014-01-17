/* TODO
 * - When storing rules, update an index of their predicates (one for antes, one for consqs). When inferring, limit the rules we use to those with a relevant predicate.
 * - Fix op's in reasoningTracer so rules are more readable
 * - Add a flag param to query() in reasoningTracer to allow storing all supported subgoals as conclusions
 * - Decide if reasoningTracer should be a module, and if so, fix it
 */
  
 %We test source packages right after loading them so that rules and premises in later, higher-level packages don't influence the tests
 % for earlier, more basic packages.

:- get_flag(prolog_suffix, Old), set_flag(prolog_suffix, [".pro"|Old]).

%Load & test 'utils'
:- ['src/utils.pro', 'src/reasoningTracer.pro'].

:- writeln('Loaded the ATOM project.').