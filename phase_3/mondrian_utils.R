# ---- Optimized Utilities (Phase 2, Pure R, No Parallel) ----

# Generate all canonical (a <= b) non-congruent rectangles up to N
# Forbid the trivial N x N rectangle entirely.
gen_rects <- function(N) {
  rects <- list()
  id <- 1L
  for (a in 1L:N) {
    for (b in a:N) {
      if (a == N && b == N) next  # forbid full-board tile
      rects[[id]] <- list(w=a, h=b, area=a*b, key=paste0(a, "x", b), used=FALSE)
      id <- id + 1L
    }
  }
  # Sort by descending area (large-first heuristic)
  o <- order(vapply(rects, `[[`, numeric(1), "area"), decreasing = TRUE)
  rects[o]
}

# Bit helpers
.full_mask <- function(N) bitwShiftL(1L, N) - 1L

# First empty cell in row-major order (top-leftmost)
first_empty <- function(board, N) {
  one <- 1L
  for (r in 1L:N) {
    rowmask <- board[r]
    if (rowmask != .full_mask(N)) {
      for (c in 1L:N) {
        bit <- bitwShiftL(one, c - 1L)
        if (bitwAnd(rowmask, bit) == 0L) return(c(r, c))
      }
    }
  }
  integer(0)
}

# Build a width-w mask starting at column c
make_mask <- function(w, c) {
  if (w <= 0L) return(0L)
  ones_w <- bitwShiftL(1L, w) - 1L
  bitwShiftL(ones_w, c - 1L)
}

# Check if w x h rectangle fits at (row, col) without overlap
fits_bit <- function(board, N, row, col, w, h) {
  if (col + w - 1L > N || row + h - 1L > N) return(FALSE)
  mask <- make_mask(w, col)
  r_end <- row + h - 1L
  for (rr in row:r_end) {
    if (bitwAnd(board[rr], mask) != 0L) return(FALSE)
  }
  TRUE
}

# Place w x h rectangle at (row, col): mutate board rows by OR-ing mask
place_bit <- function(board, row, col, w, h) {
  mask <- make_mask(w, col)
  r_end <- row + h - 1L
  for (rr in row:r_end) {
    board[rr] <- bitwOr(board[rr], mask)
  }
  board
}

# Remove w x h rectangle at (row, col): mutate board rows by AND with ~mask
remove_bit <- function(board, row, col, w, h) {
  mask <- make_mask(w, col)
  notm <- bitwNot(mask)
  r_end <- row + h - 1L
  for (rr in row:r_end) {
    board[rr] <- bitwAnd(board[rr], notm)
  }
  board
}

# Is the board fully filled?
is_full <- function(board, N) {
  fm <- .full_mask(N)
  all(board == fm)
}

# GCD helpers for pruning
.vec_gcd <- function(x) {
  x <- as.integer(abs(x))
  x <- x[x != 0L]
  if (!length(x)) return(0L)
  g <- x[1L]
  for (i in 2L:length(x)) {
    a <- g; b <- x[i]
    while (b != 0L) { t <- a %% b; a <- b; b <- t }
    g <- a
    if (g == 1L) return(1L)
  }
  g
}

# Quick necessary feasibility checks on remaining area
# 1) sum(unused_areas) >= A_remaining
# 2) gcd(unused_areas) divides A_remaining (when both > 0)
# 3) min(unused_areas) <= A_remaining  (unless A_remaining == 0)
feasible_remaining <- function(A_remaining, unused_areas) {
  if (A_remaining == 0L) return(TRUE)
  if (!length(unused_areas)) return(FALSE)
  if (sum(unused_areas) < A_remaining) return(FALSE)
  if (min(unused_areas) > A_remaining) return(FALSE)
  g <- .vec_gcd(unused_areas)
  if (g > 0L && (A_remaining %% g) != 0L) return(FALSE)
  TRUE
}
