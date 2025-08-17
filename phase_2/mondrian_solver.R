# ---- Optimized Backtracking Solver (Phase 2, Pure R, No Parallel) ----
# Uses bitmask board, symmetry breaking, and aggressive pruning.

solve_mondrian <- function(N) {
  board <- integer(N)                 # N rows, all zeros
  rects <- gen_rects(N)               # sorted by descending area
  areas <- vapply(rects, `[[`, numeric(1), "area")
  
  best_defect <- Inf
  best_tilings <- list()
  
  # Track nodes/prunes (optional; uncomment prints if needed)
  nodes <- 0L; prunes <- 0L
  
  recurse <- function(depth, cur_min, cur_max, area_used) {
    nodes <<- nodes + 1L
    
    # If board filled, evaluate
    if (is_full(board, N)) {
      # Require >= 2 rectangles (N x N is already forbidden, so generally ok)
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
    
    # Early defect pruning (needs at least 2 placed to be meaningful)
    if (depth >= 2L) {
      cur_def <- cur_max - cur_min
      if (cur_def >= best_defect) { prunes <<- prunes + 1L; return(invisible(NULL)) }
    }
    
    # Remaining area feasibility pruning
    A_remaining <- N*N - area_used
    unused_areas <- areas[!vapply(rects, `[[`, logical(1), "used")]
    if (!feasible_remaining(A_remaining, unused_areas)) {
      prunes <<- prunes + 1L; return(invisible(NULL))
    }
    
    # Symmetry breaking: always expand from top-leftmost empty cell
    pos <- first_empty(board, N)
    if (length(pos) == 0L) return(invisible(NULL))
    row <- pos[1L]; col <- pos[2L]
    
    # Try each unused rectangle, larger areas first (rects already sorted desc)
    for (i in seq_along(rects)) {
      r <- rects[[i]]
      if (r$used) next
      
      # Two orientations, but still only one use of the shape (non-congruent)
      for (rot in 1:2) {
        w <- if (rot == 1L) r$w else r$h
        h <- if (rot == 1L) r$h else r$w
        
        # Quick boundary check before heavier fits
        if (col + w - 1L > N || row + h - 1L > N) next
        
        # Fit check (bitwise)
        if (!fits_bit(board, N, row, col, w, h)) next
        
        # Place
        board <<- place_bit(board, row, col, w, h)
        rects[[i]]$used <<- TRUE
        
        # Update placements and bounds incrementally
        placements[[depth + 1L]] <<- list(row=row, col=col, w=w, h=h, area=r$area, id=i)
        new_min <- if (depth == 0L) r$area else min(cur_min, r$area)
        new_max <- if (depth == 0L) r$area else max(cur_max, r$area)
        
        # Optional additional pruning: if even the *best-case* new defect ≥ best, prune
        if (depth >= 1L) {
          cur_def <- new_max - new_min
          if (cur_def >= best_defect) {
            # backtrack
            placements[[depth + 1L]] <<- NULL
            rects[[i]]$used <<- FALSE
            board <<- remove_bit(board, row, col, w, h)
            prunes <<- prunes + 1L
            next
          }
        }
        
        # Recurse
        recurse(depth + 1L, new_min, new_max, area_used + r$area)
        
        # Backtrack
        placements[[depth + 1L]] <<- NULL
        rects[[i]]$used <<- FALSE
        board <<- remove_bit(board, row, col, w, h)
      }
    }
    
    invisible(NULL)
  }
  
  placements <- vector("list", N*N)  # upper bound on number of rectangles
  recurse(depth = 0L, cur_min = 0L, cur_max = 0L, area_used = 0L)
  
  list(
    best_defect = best_defect,
    tilings = best_tilings
    # , nodes = nodes, prunes = prunes   # expose if you want stats
  )
}
