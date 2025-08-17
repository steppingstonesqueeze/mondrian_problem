# ---- Utilities for Mondrian Problem ----

# checks in place for removing trivial solutions
gen_rects <- function(N) {
  rects <- list()
  id <- 1
  for (a in 1:N) {
    for (b in a:N) {  # ensure a <= b, canonical
      # forbid the trivial full N x N square
      if (a == N && b == N) next  
      rects[[id]] <- list(w=a, h=b, area=a*b, shape=paste0(a,"x",b), used=FALSE)
      id <- id + 1
    }
  }
  rects
}

# Find first empty cell (row-major order)
first_empty <- function(board) {
  which(board == 0, arr.ind=TRUE)[1, ]
}

# Check if rectangle (w,h) fits at (x,y) on board
fits <- function(board, x, y, w, h) {
  N <- nrow(board)
  if (x + w - 1 > N || y + h - 1 > N) return(FALSE)
  sub <- board[x:(x+w-1), y:(y+h-1)]
  all(sub == 0)
}

# Place rectangle on board
place_rect <- function(board, x, y, w, h, id) {
  board[x:(x+w-1), y:(y+h-1)] <- id
  board
}
