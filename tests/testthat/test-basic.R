context("Basic Sanity Checks")
## Setting test specific variables
tol <- 1e-3
## simulates white noise
## * Simulating artificial problem
## Time steps
T <- 100
## Number of predictors
N <- 25
## Number of irrelevant factors
K_f <- 1
## Number of proxies
L <- 1
sim <- sim_problem(T, N, K_f)
X <- sim$X
y <- sim$y
## --------------------------------------------------------------
test_that("auto-proxies selection procedure is consitent", {
    expect_equal(coef(TPRF(X, y, L=1)), coef(TPRF(X, y, y)),
                 tolerance=tol, scale=1)
})

## --------------------------------------------------------------
test_that("handling of missing value when there is none does nothing", {
    expect_equal(coef(TPRF(X, y, L=1, check_missing=FALSE)),
                 coef(TPRF(X, y, L=1, check_missing=TRUE)),
                 tolerance=tol, scale=1)
})
