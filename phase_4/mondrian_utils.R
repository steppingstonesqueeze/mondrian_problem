# ---- Mondrian Utilities ----

# Candidate areas from non-congruent rectangles up to N (forbid N x N)
candidate_areas <- function(N) {
  A <- integer()
  for (a in 1L:N) for (b in a:N) {
    if (a == N && b == N) next
    A <- c(A, a * b)
  }
  sort(unique(A))
}

# Factor pairs for a given area within N.
# RETURNS: list of lists, each like list(w=ww, h=hh, area=area).
factor_pairs_for_area <- function(area, N) {
  out <- list()
  for (w in 1L:min(N, floor(sqrt(area)))) {
    if (area %% w == 0L) {
      h <- area %/% w
      if (h <= N) {
        ww <- min(w, h); hh <- max(w, h)
        out[[length(out) + 1L]] <- list(w = ww, h = hh, area = area)
      }
    }
  }
  # de-duplicate unordered pairs
  if (!length(out)) return(list())
  keys <- vapply(out, function(z) paste0(z$w, "x", z$h), "")
  out[!duplicated(keys)]
}

# Enumerate distinct-area subsets S with sum S == N^2 (ordered by defect).
partitions_distinct_sum <- function(N, max_sets = 200L) {
  target <- N * N
  A <- candidate_areas(N)
  out <- list(); cur <- integer()
  
  backtrack <- function(i, remain) {
    if (length(out) >= max_sets) return()
    if (remain == 0L && length(cur) >= 2L) {
      out[[length(out) + 1L]] <<- sort(cur)
      return()
    }
    if (i > length(A) || remain < 0L) return()
    a <- A[i]
    if (a <= remain) { cur <<- c(cur, a); backtrack(i + 1L, remain - a); cur <<- cur[-length(cur)] }
    backtrack(i + 1L, remain)
  }
  
  backtrack(1L, target)
  if (!length(out)) return(list())
  ord <- order(vapply(out, function(s) max(s) - min(s), numeric(1)))
  out[ord]
}

# ---- DLX Exact Cover (Algorithm X) ----

# Build rows for exact cover:
# - Columns 1..N^2: board cells (must be covered exactly once)
# - Columns N^2+1..N^2+R: one column per rectangle (use each rectangle exactly once)
# rects: list of list(w,h,area)
build_exact_cover_rows <- function(N, rects) {
  n_cells <- N * N
  rows <- list()
  for (k in seq_along(rects)) {
    r <- rects[[k]]
    # unique orientations (avoid duplicate if w==h)
    orients <- if (r$w == r$h) list(c(r$w, r$h)) else list(c(r$w, r$h), c(r$h, r$w))
    for (o in orients) {
      w <- o[1]; h <- o[2]
      for (row in 1:(N - h + 1)) for (col in 1:(N - w + 1)) {
        cells <- as.vector(outer((row - 1):(row + h - 2), (col - 1):(col + w - 2),
                                 function(rr, cc) rr * N + cc + 1L))
        rows[[length(rows) + 1L]] <- c(cells, n_cells + k)
      }
    }
  }
  rows
}

# Simple Algorithm X (sufficient for N up to ~8–10 with the arithmetic filter).
algorithm_x <- function(rows, ncols, max_solutions = 1L) {
  col_rows <- vector("list", ncols)
  for (ri in seq_along(rows)) for (c in rows[[ri]]) col_rows[[c]] <- c(col_rows[[c]], ri)
  
  used_rows <- logical(length(rows))
  used_cols <- logical(ncols)
  solution  <- integer(0)
  solutions <- list()
  
  choose_col <- function() {
    best_c <- 0L; best_len <- Inf
    for (c in which(!used_cols)) {
      cand <- col_rows[[c]]; cand <- cand[!used_rows[cand]]
      ln <- length(cand)
      if (ln < best_len) { best_len <- ln; best_c <- c }
      if (best_len == 0L) break
    }
    best_c
  }
  
  cover <- function(ri) {
    rcols <- rows[[ri]]
    stashed <- vector("list", length(rcols))
    for (i in seq_along(rcols)) {
      c <- rcols[[i]]
      if (used_cols[c]) next
      used_cols[c] <<- TRUE
      bad <- col_rows[[c]]; bad <- bad[!used_rows[bad]]
      stashed[[i]] <- bad
      used_rows[bad] <<- TRUE
    }
    stashed
  }
  uncover <- function(ri, stashed) {
    rcols <- rows[[ri]]
    for (i in seq_along(rcols)) {
      c <- rcols[[i]]
      used_cols[c] <<- FALSE
      used_rows[stashed[[i]]] <<- FALSE
    }
  }
  
  search <- function() {
    if (all(used_cols)) {
      solutions[[length(solutions) + 1L]] <<- solution
      return(length(solutions) < max_solutions)
    }
    c <- choose_col()
    if (c == 0L) return(TRUE)
    candidates <- col_rows[[c]]; candidates <- candidates[!used_rows[candidates]]
    if (!length(candidates)) return(TRUE)
    
    for (ri in candidates) {
      solution <<- c(solution, ri)
      stash <- cover(ri)
      cont <- search()
      uncover(ri, stash)
      solution <<- solution[-length(solution)]
      if (!cont) return(FALSE)
    }
    TRUE
  }
  
  search()
  solutions
}

# ---- Rehydrate a DLX solution into rectangle placements for plotting ----
rehydrate_solution <- function(N, rects, rows, sol_rows) {
  # Build the same linear order mapping we used in build_exact_cover_rows()
  inv <- vector("list", length(rows))
  idx <- 0L
  for (k in seq_along(rects)) {
    r <- rects[[k]]
    orients <- if (r$w == r$h) list(c(r$w, r$h)) else list(c(r$w, r$h), c(r$h, r$w))
    for (o in orients) {
      w <- o[1]; h <- o[2]
      for (row in 1:(N - h + 1)) for (col in 1:(N - w + 1)) {
        idx <- idx + 1L
        inv[[idx]] <- list(row = row, col = col, w = w, h = h, area = r$area, id = k)
      }
    }
  }
  til <- lapply(sol_rows, function(ri) inv[[ri]])
  ord <- order(vapply(til, `[[`, integer(1), "row"),
               vapply(til, `[[`, integer(1), "col"))
  til[ord]
}
