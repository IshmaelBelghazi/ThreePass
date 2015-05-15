context("Generics")
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

split_idx <- floor(T/2)

fit_tprf<- TPRF(X[1:split_idx, ], y[1:split_idx], L=L, closed_form=FALSE)
fit_pls <- TPRF(X[1:split_idx, ], y[1:split_idx], L=L, pls=TRUE, closed_form=FALSE)
newdata <- X[(split_idx + 1):NROW(X), ]

## --------------------------------------------------------------
test_that("coef generic is consistent", {
    expect_equal(coef(fit_tprf), fit_tprf$fit$coefficients,
                 tolerance=tol, scale=1)
    expect_equal(coef(fit_pls), fit_pls$fit$coefficients,
                 tolerance=tol, scale=1)
})
## --------------------------------------------------------------
test_that("fitted generic is consistent", {
    expect_equal(fitted(fit_tprf), fit_tprf$fit$fitted.values,
                 tolerance=tol, scale=1)
    expect_equal(fitted(fit_pls), fit_pls$fit$fitted.values,
                 tolerance=tol, scale=1)
})
## --------------------------------------------------------------
test_that("resid generic is consistent", {
    expect_equal(resid(fit_tprf), y[1:split_idx] - fitted(fit_tprf),
                 tolerance=tol, scale=1)
    expect_equal(resid(fit_pls), y[1:split_idx] - fitted(fit_pls),
                 tolerance=tol, scale=1)
})
## --------------------------------------------------------------
test_that("predict generic is consistent", {
    if (fit_tprf$scaled) newdata_manual <- t(apply(newdata, 1,
                                              function(row) row/fit_tprf$scales))
    newfactor <- apply(newdata_manual, 1, function(row){
        coef(lm.fit(cbind(1, fit_tprf$loadings[, -1]), row))[-1]
    })
    forecast_manual <- cbind(1, newfactor) %*% coef(fit_tprf)
    expect_less_than(abs(predict(fit_tprf, newdata) - forecast_manual), tol)
}
          )
## ---------------------------------------------------------------
