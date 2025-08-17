# ---- Phase 2.2: Parallel wrapper (keeps Phase 2 core intact) ----
# Strategy:
# 1) Enumerate all legal first placements at (row=1, col=1) using current rect set.
# 2) Launch each first-move branch in parallel, each solving its subtree independently.
# 3) Reduce to the global best defect and collect all best tilings.

# We assume mondrian_utils.R and mondrian_solver.R are sourced already.
# We'll borrow gen_rects(), fits_bit(), place_bit(), remove_bit(), first_empty(), etc.

enumerate_first_moves <- function(N) {
  board <- integer(N)              # empty
  rects <- gen_rects(N)            # already sorted by area desc
  moves <- list()
  idx <- 1L
  row <- 1L; col <- 1L
  
  for (i in seq_along(rects)) {
    r <- rects[[i]]
    if (r$used) next
    for (rot in 1:2) {
      w <- if (rot == 1L) r$w else r$h
      h <- if (rot == 1L) r$h else r$w
      if (col + w - 1L > N || row + h - 1L > N) next
      if (!fits_bit(board, N, row, col, w, h)) next
      moves[[idx]] <- list(i=i, w=w, h=h, area=r$area)
      idx <- idx + 1L
    }
  }
  moves
}

# Self-contained copy of the Phase 2 core recursion, but parameterized so we can
# start from a preset board/rects/placements state (after first move).
solve_mondrian_from_state <- function(N, board, rects, placements, depth, cur_min, cur_max, area_used) {
  areas <- vapply(rects, `[[`, numeric(1), "area")
  best_defect <- Inf
  best_tilings <- list()
  
  recurse <- function(depth, cur_min, cur_max, area_used) {
    if (is_full(board, N)) {
      if (depth >= 2L) {
        defect <- cur_max - cur_min
        if (defect < best_defect) {
          best_defect <<- defect
          best_tilings <<- list(placements)
        } else if (defect == best_defect) {
          best_tilings <<- c(best_tilings, list(placements))
        }
      }
      return(invisible(NULL))
    }
    
    if (depth >= 2L) {
      cur_def <- cur_max - cur_min
      if (cur_def >= best_defect) return(invisible(NULL))
    }
    
    A_remaining <- N*N - area_used
    unused_areas <- areas[!vapply(rects, `[[`, logical(1), "used")]
    if (!feasible_remaining(A_remaining, unused_areas)) return(invisible(NULL))
    
    pos <- first_empty(board, N)
    if (length(pos) == 0L) return(invisible(NULL))
    row <- pos[1L]; col <- pos[2L]
    
    for (i in seq_along(rects)) {
      r <- rects[[i]]
      if (r$used) next
      
      for (rot in 1:2) {
        w <- if (rot == 1L) r$w else r$h
        h <- if (rot == 1L) r$h else r$w
        if (col + w - 1L > N || row + h - 1L > N) next
        if (!fits_bit(board, N, row, col, w, h)) next
        
        board <<- place_bit(board, row, col, w, h)
        rects[[i]]$used <<- TRUE
        
        placements[[depth + 1L]] <<- list(row=row, col=col, w=w, h=h, area=r$area, id=i)
        new_min <- if (depth == 0L) r$area else min(cur_min, r$area)
        new_max <- if (depth == 0L) r$area else max(cur_max, r$area)
        
        if (depth >= 1L && (new_max - new_min) >= best_defect) {
          placements[[depth + 1L]] <<- NULL
          rects[[i]]$used <<- FALSE
          board <<- remove_bit(board, row, col, w, h)
          next
        }
        
        recurse(depth + 1L, new_min, new_max, area_used + r$area)
        
        placements[[depth + 1L]] <<- NULL
        rects[[i]]$used <<- FALSE
        board <<- remove_bit(board, row, col, w, h)
      }
    }
    invisible(NULL)
  }
  
  recurse(depth, cur_min, cur_max, area_used)
  list(best_defect = best_defect, tilings = best_tilings)
}

solve_mondrian_parallel <- function(N, mc.cores = NULL) {
  # Enumerate first moves at (1,1)
  first_moves <- enumerate_first_moves(N)
  if (length(first_moves) == 0L) {
    return(list(best_defect = Inf, tilings = list()))
  }
  
  # Cross-platform: use serial lapply on Windows (no fork), mclapply elsewhere
  use_parallel <- .Platform$OS.type != "windows"
  
  if (is.null(mc.cores)) {
    mc.cores <- max(1L, parallel::detectCores() - 1L)
  }
  
  # Each branch: set up dedicated state with that first placement, then solve subtree
  branch_fun <- function(mv) {
    board <- integer(N)
    rects <- gen_rects(N)
    placements <- vector("list", N*N)
    
    # Apply the first move at (1,1)
    row <- 1L; col <- 1L
    board <- place_bit(board, row, col, mv$w, mv$h)
    rects[[mv$i]]$used <- TRUE
    placements[[1L]] <- list(row=row, col=col, w=mv$w, h=mv$h, area=mv$area, id=mv$i)
    
    # Continue search from this state
    solve_mondrian_from_state(
      N = N,
      board = board,
      rects = rects,
      placements = placements,
      depth = 1L,
      cur_min = mv$area,
      cur_max = mv$area,
      area_used = mv$area
    )
  }
  
  results <- if (use_parallel) {
    parallel::mclapply(first_moves, branch_fun, mc.cores = mc.cores)
  } else {
    lapply(first_moves, branch_fun)
  }
  
  # Reduce: find best defect and gather all best tilings
  defects <- vapply(results, `[[`, numeric(1), "best_defect")
  best <- min(defects)
  best_tilings <- list()
  if (is.finite(best)) {
    for (res in results) {
      if (identical(res$best_defect, best)) {
        best_tilings <- c(best_tilings, res$tilings)
      }
    }
  }
  
  list(best_defect = best, tilings = best_tilings)
}
