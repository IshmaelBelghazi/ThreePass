context("Three Pass Regression Filter Procedure")
## Setting test specific variables
tol <- 1e-6
## simulates white noise
## * Simulating artificial problem
## Time steps
T <- 100
## Number of predictors
N <- 25
## Number of relevant factors
K_f <- 1
## Number of proxies
L <- 1
sim <- sim_problem(T, N, K_f)
X <- sim$X
y <- sim$y
X <- apply(X, 2, function(X) X/sd(X))
## --------------------------------------------------------------
test_that("Testing closed form/iterative estimation consistency",

          expect_equal(fitted(TPRF(X, y, L=1, closed_form=FALSE)),
                       fitted(TPRF(X, y, L=1, closed_form=TRUE)),
                       tolerance=tol, scale=1)


          )
