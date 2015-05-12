context("Basic Sanity Checks")
## Setting test specific variables
tol <- 1e-6
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
test_that("auto-proxies selection procedure", {
    expect_equal(coef(TPRF(X, y, L=1)), coef(TPRF(X, y, y)),
                 tolerance=tol, scale=1)
})

## --------------------------------------------------------------
test_that("missing values handling when there is none", {
    expect_equal(coef(TPRF(X, y, L=1, check_missing=FALSE)),
                 coef(TPRF(X, y, L=1, check_missing=TRUE)),
                 tolerance=tol, scale=1)
})
