## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6, fig.height = 3.5)
library(adheaping)

## -----------------------------------------------------------------------------
set.seed(20260627)
n <- 4000
x <- ifelse(runif(n) < 0.5, rnorm(n, -1.2, 0.5), rnorm(n, 1.2, 0.5))
D <- 0.5
y <- D * round(x / D)
grid <- seq(-6, 6, length.out = 2048)

f_true  <- 0.5 * dnorm(grid, -1.2, 0.5) + 0.5 * dnorm(grid, 1.2, 0.5)
f_naive <- naive_kde(y, grid)
f_adk   <- adkde(y, D, grid)
attr(f_adk, "pick")   # which component the band-capacity gate selected

## ----echo = FALSE-------------------------------------------------------------
plot(grid, f_true, type = "l", lwd = 2, xlab = "x", ylab = "density", xlim = c(-3, 3))
lines(grid, f_naive, col = "grey55", lty = 3, lwd = 1.6)
lines(grid, as.numeric(f_adk), col = "#b2182b", lwd = 1.8)
legend("topright", c("truth", "naive KDE", "adkde (combined)"),
       col = c("black", "grey55", "#b2182b"), lty = c(1, 3, 1), lwd = 1.8, bty = "n")

## -----------------------------------------------------------------------------
heap_grid(y, grid, near = D)                 # locate the grid from the comb tooth
heap_detect(y = D * round(x / D), span = c(-12.8, 12.8))$D_hat  # blind spectral detection

