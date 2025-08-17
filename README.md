the Mondrian art problem in R in various phases

Phase 1 : simple feasibility ot backtracker and small N <= 5
Phase 2 : improved backtracking and early path abandonment and can scale to N <= 7
Phase 3 : parallelises Phase 2, scales to N <= 9
Phase 4 : uses some simple number theory for reducing search space and can scale to N<=20 with the caveat that we get A feasible solution, not the optimal one(s)
