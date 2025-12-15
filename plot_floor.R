plot_floor <- function(landmarks_df, plot=TRUE) {
  x <- landmarks_df[,1]
  y <- landmarks_df[,2]
  z <- landmarks_df[,3]
  fit <- lm(z ~ x + y)
  coefs <- coef(fit)
  a <- coefs["x"]
  b <- coefs["y"]
  c <- -1
  d <- coefs["(Intercept)"]
  if (plot) {
    rgl::planes3d(a, b, c, d, alpha=0.5)
  }
  list(a=a, b=b, c=c, d=d)
}
