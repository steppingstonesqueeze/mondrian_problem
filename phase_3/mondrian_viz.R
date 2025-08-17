library(ggplot2)

# Convert a "placements" tiling (list of rectangles) into a data frame for plotting
tiling_to_df <- function(tiling) {
  do.call(
    rbind,
    lapply(tiling, function(r) {
      if (is.null(r)) return(NULL)
      data.frame(
        xmin = r$col - 1,
        xmax = r$col - 1 + r$w,
        ymin = r$row - 1,
        ymax = r$row - 1 + r$h,
        id   = factor(r$id),
        label = paste0(r$w, "x", r$h)
      )
    })
  )
}

plot_tiling <- function(tiling, N, title = NULL) {
  df <- tiling_to_df(tiling)
  if (is.null(df) || nrow(df) == 0) return(ggplot() + ggtitle("No tiling"))
  
  ggplot(df) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = id),
      color = "black", linewidth = 0.4
    ) +
    geom_text(
      aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
      size = 3
    ) +
    coord_fixed(xlim = c(0, N), ylim = c(0, N), expand = FALSE) +
    theme_void() +
    guides(fill = "none") +
    ggtitle(if (is.null(title)) paste("Mondrian tiling (N =", N, ")") else title)
}
