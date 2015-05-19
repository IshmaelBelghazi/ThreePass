context("Utility functions")
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
test_that("corruption function is well behaved", {
    n_corrupted <- sample(floor((T * N)/2), 1)
    expect_equal(sum(is.na(corrupt(X, NA, n_corrupted=n_corrupted))), n_corrupted)
    expect_equal(sum(is.na(corrupt(X, NA, n_corrupted=n_corrupted))),
                 sum(is.na(corrupt(X, function(x, ...) NA, n_corrupted=n_corrupted))),
                 tolerance=tol, scale=1)
    expect_equal(sum(is.na(corrupt(X, NA, n_corrupted=n_corrupted))),
                 sum(is.na(corrupt(X, function(x) NA, n_corrupted=n_corrupted))),
                 tolerance=tol, scale=1)
    expect_error(corrupt(X, NA, n_corrupted= -1),
                 "number of entries to corrupt has to be positive")
    expect_error(corrupt(X, NA, n_corrupted=length(X) + 1),
                 "number of entries to corrupt than entries")
}
          )
## --------------------------------------------------------------
test_that("NA handling function is well behaved", {
    n_corrupted <- sample(floor(T/2), 1)
    y_cor <- corrupt(y, NA, n_corrupted=n_corrupted)
    X_cor <- corrupt(X, NA, n_corrupted=n_corrupted)
    expect_error(.get_valid_idx(X, y_cor, y),
                 "one or missing values in target. aborting.")
    expect_error(.get_valid_idx(X, y, y_cor),
                 "one or more missing values in proxies. aborting.")
    expect_equal(sum(.get_valid_idx(X_cor, y, y)), length(X_cor) - n_corrupted)
}
          )
