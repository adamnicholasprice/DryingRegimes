max_rle <- function(x, val) {
  y <- rle(x)
  len <- y$lengths[y$value == val]
  if (length(len) > 0) max(len) else NA
}

x = c(0,1, 1, 1, 1, 1, 0,NA,NA)

max_rle(x,0)