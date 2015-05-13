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
test_that("closed form/iterative estimations are consitent (auto-proxies)", {
    for (L in 1:2) {
        K_f <- L
        sim <- sim_problem(T, N, K_f, sigma_g=NULL)
        X <- sim$X
        y <- sim$y
        X <- apply(X, 2, function(X) X/sd(X))
        expect_equal(fitted(TPRF(X, y, L=L, closed_form=FALSE)),
                     fitted(TPRF(X, y, L=L, closed_form=TRUE)),
                     tolerance=tol, scale=1)
    }
}
          )
## --------------------------------------------------------------
test_that("closed form/iterative estimation are consistent (generated proxies)", {
    for (L in 1:2) {
        K_f <- L
        sim <- sim_problem(T, N, K_f, L=L, sigma_g=NULL)
        X <- sim$X
        y <- sim$y
        Z <- sim$Z
        X <- apply(X, 2, function(X) X/sd(X))
        expect_equal(fitted(TPRF(X, y, Z=Z, closed_form=FALSE)),
                     fitted(TPRF(X, y, Z=Z, closed_form=TRUE)),
                     tolerance=tol, scale=1)
    }
}
          )
