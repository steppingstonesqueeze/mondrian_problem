source("mondrian_utils.R")
source("mondrian_solver.R")
source("mondrian_viz.R")

# Install if needed:
# install.packages("ggplot2")

N <- 6  # try 5..8 to start
res <- solve_mondrian(N, max_partitions = 500L)
cat("Best defect M(", N, ") = ", res$best_defect, "\n", sep = "")

print(plot_tiling(N, res$tiling))
