context("Three Pass Regression Filter Estimators")
## Setting test specific variables
tol <- 1e-6
## simulates white noise
## * Simulating artificial problem
## Time steps
T <- 100
## Number of predictors
N <- 25
## --------------------------------------------------------------
test_that("closed form/iterative 3PRF estimations are consitent (auto-proxies)", {
    for (L in 1:2) {
        K_f <- L
        sim <- sim_problem(T, N, K_f, sigma_g=NULL)
        X <- sim$X
        y <- sim$y
        expect_equal(fitted(TPRF(X, y, L=L, pls=FALSE, closed_form=FALSE)),
                     fitted(TPRF(X, y, L=L, pls=FALSE, closed_form=TRUE)),
                     tolerance=tol, scale=1)
    }
}
          )
## --------------------------------------------------------------
test_that("closed form/iterative 3PRF estimation are consistent (generated proxies)", {
    for (L in 1:2) {
        K_f <- L
        sim <- sim_problem(T, N, K_f, L=L, sigma_g=NULL)
        X <- sim$X
        y <- sim$y
        Z <- sim$Z
        expect_equal(fitted(TPRF(X, y, Z=Z, pls=FALSE, closed_form=FALSE)),
                     fitted(TPRF(X, y, Z=Z, pls=FALSE, closed_form=TRUE)),
                     tolerance=tol, scale=1)
    }
}
          )
## --------------------------------------------------------------
test_that("closed form/iterative PLS estimation are consistent", {

    K_f <- 1
    sim <- sim_problem(T, N, K_f, L=1, sigma_g=NULL)
    X <- sim$X
    y <- sim$y
    Z <- sim$Z
    expect_equal(fitted(TPRF(X, y, L=1, pls=TRUE, closed_form=FALSE)),
                 fitted(TPRF(X, y, L=1, pls=TRUE, closed_form=TRUE)),
                 tolerance=tol, scale=1)
}
          )
## --------------------------------------------------------------
test_that("estimator returns error with multivariate target", {
    K_f <- 1
    sim <- sim_problem(T, N, K_f, L=1, sigma_g=NULL)
    X <- sim$X
    y <- sim$y
    expect_error(TPRF(X, cbind(y, y), L=1), "y should be univariate")
})
## --------------------------------------------------------------
test_that("estimator returns error when neither Z or L are specified", {
    K_f <- 1
    sim <- sim_problem(T, N, K_f, L=1, sigma_g=NULL)
    X <- sim$X
    y <- sim$y
    expect_error(TPRF(X, y),
                 "please either provide proxies or choose a number of automatic proxies to build")
})
