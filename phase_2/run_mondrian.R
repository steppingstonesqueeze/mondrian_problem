source("mondrian_utils.R")
source("mondrian_solver.R")
source("mondrian_viz.R")

N <- 7  # try 3..7 (maybe 8) in Phase 2; (no parallelism and it is SLOW after N=7 #

set.seed(42)
res <- solve_mondrian(N)

cat("Best defect M(", N, ") = ", res$best_defect, "\n", sep = "")

if (length(res$tilings) > 0) {
  # Plot the first best tiling
  print(plot_tiling(res$tilings[[1]], N))
}
