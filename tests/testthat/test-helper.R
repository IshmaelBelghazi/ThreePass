context("Helper functions checks")
## Setting test specific variables
tol <- 1e-6
## simulates white noise
## * Simulating artificial problem
## Time steps
T <- 100
## Number of predictors
N <- 10
## Number of irrelevant factors
K_f <- 1
## Number of proxies
L <- 1
sim <- sim_problem(T, N, K_f)
X <- sim$X
y <- sim$y
## --------------------------------------------------------------
test_that("recursive forecasts are consistent", {
    ## Manual two periods recursive forecast
    X_1 <- X[1:(NROW(X) - 2), ]
    y_1 <- y[1:(NROW(X) - 2)]
    newdata_1 <- X[NROW(X) - 1,, drop=FALSE]
    X_2 <- X[1:(NROW(X) - 1), ]
    y_2 <- y[1:(NROW(X) - 1)]
    newdata_2 <- X[NROW(X),, drop=FALSE]

    forecast_1 <- predict(TPRF(X_1, y_1, L=L), newdata=newdata_1)
    forecast_2 <- predict(TPRF(X_2, y_2, L=L), newdata=newdata_2)
    forecast_manual <- matrix(c(rep(NA, NROW(X) - 2), forecast_1, forecast_2), ncol=1)
    expect_equal(predict_recursive(X, y, L=L, train_periods=NROW(X)-2),
                 forecast_manual,
                 tolerance=tol, scale=1)
})
## ---------------------------------------------------------------
test_that("rolling forecasts are consistent", {
    ## Manual two periods rolling forecast
    window <- 30
    X_1 <- X[1:window, ]
    y_1 <- y[1:window]
    newdata_1 <- X[window + 1, , drop=FALSE]

    X_2 <- X[2:(window + 1), ]
    y_2 <- y[2:(window + 1)]
    newdata_2 <- X[window + 2, , drop=FALSE]

    forecast_1 <- predict(TPRF(X_1, y_1, L=L), newdata=newdata_1)
    forecast_2 <- predict(TPRF(X_2, y_2, L=L), newdata=newdata_2)
    forecast_manual <- matrix(c(rep(NA, window), forecast_1, forecast_2), ncol=1)

    X_roll <- X[1:(window + 2), ]
    y_roll <- y[1:(window + 2)]

    expect_equal(predict_rolling(X_roll, y_roll, L=L, train_window=window),
                 forecast_manual, tolerance=tol, scale=1)
})
