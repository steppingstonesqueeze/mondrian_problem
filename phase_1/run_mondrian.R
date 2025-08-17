source("mondrian_utils.R")
source("mondrian_solver.R")
source("mondrian_viz.R")

N <- 6  # try N=3..6
result <- solve_mondrian(N)

cat("Best defect M(", N, ") = ", result$best_defect, "\n")

# Plot first best tiling
if (length(result$tilings) > 0) {
  print(plot_tiling(result$tilings[[1]], N))
}
