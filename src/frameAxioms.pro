addRule(frameAx_intendWhenFor,
        <=(intendWhenFor(P,do(Q,A),C,G,T3),
           [%1) Make sure the agent had a previous intent for or against the same action, enabler, and goal
            %    so we don't back-chain to an infinitely remove past time looking for one in the next step
            %   We wrap in a compute to avoid fore-chaining.
            1:compute((query(    intendWhenFor(P,do(Q,A),C,G,T),  dontAllowBackchaining, _QTrace) ;
                       query(neg(intendWhenFor(P,do(Q,A),C,G,T)), dontAllowBackchaining, _QTrace) ),
                      T < T3 ),
            %2) Now that we know there is something to backchain to, we take tentative steps back in time
            %   looking for the most recent statement about it (for or against)
            2:frameAx(intendWhenFor(P,do(Q,A),C,G,T3)
           ])).
           
addRule(frameAx_intendWhenFor,
        <=(frameAx(intendWhenFor(P,do(Q,A),C,G,T3)),
            %We wrap in a compute to avoid fore-chaining.
           [1:compute((query(neg(intendWhenFor(P,do(Q,A),C,G,T3)), dontAllowBackchaining, _QTrace1),
                      -> fail
                       ; (T2 is T3 - 1,
                          (query(intendWhenFor(P,do(Q,A),C,G,T2), dontAllowBackchaining, _QTrace1)
                           -> true
                            ; query(frameAx(intendWhenFor(P,do(Q,A),C,G,T2)),allowBackchaining,_QTrace2) )))
           ])).