## * Three pass regression filter
##' Three Pass Regression Filter
##'
##' Fits the three pass regression filter
##' @title Three Pass Regression filter
##' @param X Observations (typically lagged)
##' @param y Targets
##' @param Z Proxies
##' @param L Scalar. Number of proxies if Z is NULL
##' @param center Center the data?
##' @param scale Scale the data
##' @param fitalg OLS fit algorithm
##' @return TPRF object
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
TPRF <- function(X, y, Z=NULL, L=NULL, center=TRUE, scale=TRUE, fitalg=2) {

    if(is.null(Z) && is.null(L)) {
        stop("Please either provide proxies or choose a number of automatic proxies to build")
    }

    if(center) {
        X_mean <- apply(X, 2, function(X) mean(X, na.rm=TRUE))
        X <- apply(X, 2, function(X) X - X_mean)
    } else {
        X_mean <- NULL
    }

    if (scale) {
        X_sd <- apply(X, 2, function(X) sd(X, na.rm=TRUE))
        X <- apply(X, 2, function(X) X/X_sd)
    } else {
        X_sd <- NULL
    }

    ## Computing automatic proxies
    if(is.null(Z)) {
        r <- matrix(NA, NROW(y), L)
        r[, 1] <- y
        k <- 1
        while(k < L) {
            k <- k + 1
            r[, k] <- resid(.tprf_fit(X, y, r[, k - 1], fitalg=fitalg))
        }
        Z <- r
    }

    ## Running three pass regression filter
    fit <- .tprf_fit(X, y, Z, fitalg=fitalg)

    ## Creating TPRF object
    structure(list(fit=fit,
                   L=L,
                   loadings=fit$loadings,
                   factors=fit$factors,
                   centered=center,
                   means=X_mean,
                   scaled=scale,
                   scales=X_sd),
              class="t3prf")

    }
##' @export
.tprf_fit <- function(X, y, Z, fitalg=2) {

    if(!is.matrix(X)) X <- as.matrix(X)
    if(!is.matrix(y)) y <- as.matrix(y)
    if(!is.matrix(Z)) Z <- as.matrix(Z)

    ## Step 1 Time series regression
    ## Setting proxies with intercept
    Z_intercept <- cbind(1, Z)
    loadings <- t(apply(X, 2,
                      function(X) coef(RcppEigen::fastLmPure(Z_intercept, X,
                                                  method=fitalg))))
    ## Step II Cross section regression
    loadings_intercept <- cbind(1, loadings[, -1, drop=FALSE])
    factors <- t(apply(X, 1,
                     function(X) coef(RcppEigen::fastLmPure(loadings_intercept, X,
                                                 method=fitalg))))
        ## Should Factors be scaled? Seems that factor are unique up to scaling ...
    ##factors <- scale(factors)
    ## Step III predictive regression
    factors_intercept <- cbind(1, factors[, -1, drop=FALSE])
    predictive_reg <- RcppEigen::fastLmPure(factors_intercept, y, method=fitalg)
    ## Adding loadings and factors to list
    predictive_reg$loadings <- loadings
    predictive_reg$factors <- factors
    return(predictive_reg)
}
