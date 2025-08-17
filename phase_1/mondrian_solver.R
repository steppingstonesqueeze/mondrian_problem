# ---- Backtracking Solver for Mondrian Problem ----

solve_mondrian <- function(N) {
  board <- matrix(0, nrow=N, ncol=N)
  rects <- gen_rects(N)
  
  best_defect <- Inf
  best_tilings <- list()
  
  recurse <- function(board, rects, placed) {
    # If board full → evaluate
    if (all(board != 0)) {
      areas <- sapply(placed, function(r) r$area)
      defect <- max(areas) - min(areas)
      if (defect < best_defect) {
        best_defect <<- defect
        best_tilings <<- list(placed)
      } else if (defect == best_defect) {
        best_tilings <<- c(best_tilings, list(placed))
      }
      return()
    }
    
    pos <- tryCatch(first_empty(board), error=function(e) return(NULL))
    if (is.null(pos)) return()
    x <- pos[1]; y <- pos[2]
    
    for (i in seq_along(rects)) {
      r <- rects[[i]]
      if (r$used) next
      
      # try (w,h)
      for (rot in 1:2) {
        w <- ifelse(rot==1, r$w, r$h)
        h <- ifelse(rot==1, r$h, r$w)
        
        if (fits(board, x, y, w, h)) {
          new_board <- place_rect(board, x, y, w, h, i)
          rects[[i]]$used <- TRUE
          recurse(new_board, rects, c(placed, list(list(x=x,y=y,w=w,h=h,area=w*h,id=i))))
          rects[[i]]$used <- FALSE
        }
      }
    }
  }
  
  recurse(board, rects, list())
  
  list(best_defect=best_defect, tilings=best_tilings)
}
