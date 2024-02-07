## build one data frame with all series
## utility functions

library(plotrix)

# display R matrix in text

write_matex2 <- function(x) {
  begin <- "\\begin{bmatrix}"
  end <- "\\end{bmatrix}"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  paste(c(begin, X, end), collapse = "")
}

# find time index closest to a given date

closest.index <- function(ts, date) {
  delta <- abs(as.Date(time(ts)) - date)
  idx <- which.min(delta)
  idx
}

# a plot when some values are off-the-chart

gap.plot.helper <- function(x, y, from, to, ...) {
  gp <- gap.plot(x, y, gap=c(from, to), ...)
  axis.break(2, from, breakcol="snow", style="gap")
  axis.break(2, from*(1+0.02), breakcol="black", style="slash")
  axis.break(4, from*(1+0.02), breakcol="black", style="slash")
  axis(2, at=from)
  gp
}
