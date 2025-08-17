# ---- Hybrid Mondrian Solver (Min-defect guaranteed across tried partitions) ----
source("mondrian_utils.R")

.fp_to_wh <- function(fp) {
  if (is.list(fp)) {
    if (!is.null(fp$w) && !is.null(fp$h)) return(c(as.integer(fp$w), as.integer(fp$h)))
    x <- unlist(fp, use.names = FALSE); return(c(as.integer(x[1]), as.integer(x[2])))
  } else {
    return(c(as.integer(fp[1]), as.integer(fp[2])))
  }
}

# Try all factor-pair assignments for a partition S; return first DLX tiling found (or NULL)
.try_all_factor_assignments <- function(N, S) {
  m <- length(S)
  # Precompute options per area
  options <- vector("list", m)
  for (i in seq_len(m)) {
    fps <- factor_pairs_for_area(S[i], N)
    if (!length(fps)) return(NULL)  # impossible partition (some area has no pair)
    # normalize to (w,h)
    options[[i]] <- lapply(fps, .fp_to_wh)
  }
  
  rects <- vector("list", m)
  used_shapes <- new.env(parent = emptyenv())  # (w,h) keys if you also want to forbid same dims (redundant since areas are distinct)
  ncols <- N * N
  
  # backtrack over assignments
  assign_rec <- function(k) {
    if (k > m) {
      # All assigned: run DLX
      rows <- build_exact_cover_rows(N, rects)
      sols <- algorithm_x(rows, ncols + length(rects), max_solutions = 1L)
      if (length(sols)) {
        return(list(rects = rects, rows = rows, sol = sols[[1]]))
      }
      return(NULL)
    }
    # iterate options for area S[k]
    for (wh in options[[k]]) {
      key <- paste0(wh[1], "x", wh[2])
      # optional: forbid duplicate shapes; with distinct areas it's unnecessary, but harmless
      if (!is.null(used_shapes[[key]])) next
      rects[[k]] <<- list(w = wh[1], h = wh[2], area = S[k])
      used_shapes[[key]] <- TRUE
      ans <- assign_rec(k + 1L)
      if (!is.null(ans)) return(ans)
      rm(list = key, envir = used_shapes)
      rects[[k]] <<- NULL
    }
    NULL
  }
  
  assign_rec(1L)
}

solve_mondrian <- function(N, max_partitions = 2000L) {
  # Get partitions sorted by increasing defect
  parts <- partitions_distinct_sum(N, max_partitions)
  if (!length(parts)) {
    return(list(best_defect = Inf, tiling = NULL))
  }
  
  # Group partitions by defect so we can ensure minimality
  defects <- vapply(parts, function(S) max(S) - min(S), numeric(1))
  uniq_def <- sort(unique(defects))
  
  best_defect <- Inf
  best <- NULL
  
  for (D in uniq_def) {
    # consider only partitions with this defect
    idx <- which(defects == D)
    # try each partition S of defect D; for each, try all factor-pair assignments
    for (j in idx) {
      S <- parts[[j]]
      til <- .try_all_factor_assignments(N, S)
      if (!is.null(til)) {
        best_defect <- D
        best <- til
        # Since we sweep by increasing D, the first success is truly minimal defect
        return(list(best_defect = best_defect, tiling = best))
      }
    }
    # continue to next defect level if none feasible at this D
  }
  
  # none feasible among enumerated partitions
  list(best_defect = Inf, tiling = NULL)
}
