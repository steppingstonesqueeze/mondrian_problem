source("mondrian_utils.R")
source("mondrian_viz.R")
source("mondrian_parallel.R")   # <- new

N <- 8   # try 6..12 here; parallel helps a lot
mc <- NULL  # auto cores; or set mc <- 6L

set.seed(42)
res <- solve_mondrian_parallel(N, mc.cores = mc)

cat("Parallel best defect M(", N, ") = ", res$best_defect, "\n", sep = "")

if (length(res$tilings) > 0) {
  print(plot_tiling(res$tilings[[1]], N, title = paste0("Parallel Mondrian (N=", N, ")")))
}
