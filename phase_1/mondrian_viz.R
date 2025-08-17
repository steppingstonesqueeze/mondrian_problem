library(ggplot2)

# Convert tiling to df for plotting
tiling_to_df <- function(tiling) {
  do.call(rbind, lapply(tiling, function(r) {
    data.frame(
      xmin=r$y-1, xmax=r$y-1+r$h,
      ymin=r$x-1, ymax=r$x-1+r$w,
      id=factor(r$id),
      label=paste0(r$w,"x",r$h)
    )
  }))
}

plot_tiling <- function(tiling, N) {
  df <- tiling_to_df(tiling)
  ggplot(df) +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=id), color="black") +
    geom_text(aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=label), size=3) +
    coord_fixed() +
    theme_void() +
    ggtitle(paste("Mondrian tiling, N =", N))
}
