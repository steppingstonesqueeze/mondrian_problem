library(ggplot2)

plot_tiling <- function(N, tiling) {
  if (is.null(tiling)) {
    return(ggplot() + ggtitle("No tiling found") + theme_void())
  }
  rects <- tiling$rects
  rows  <- tiling$rows
  sol   <- tiling$sol
  
  placed <- rehydrate_solution(N, rects, rows, sol)
  df <- do.call(rbind, lapply(seq_along(placed), function(i) {
    r <- placed[[i]]
    data.frame(
      xmin = r$col - 1, xmax = r$col - 1 + r$w,
      ymin = r$row - 1, ymax = r$row - 1 + r$h,
      id = factor(i), label = paste0(r$w, "x", r$h)
    )
  }))
  
  ggplot(df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = id),
              color = "black", linewidth = 0.4) +
    geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label), size = 3) +
    coord_fixed(xlim = c(0, N), ylim = c(0, N), expand = FALSE) +
    theme_void() +
    guides(fill = "none") +
    ggtitle(paste0("Mondrian tiling (N = ", N, ")"))
}
