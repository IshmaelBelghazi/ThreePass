#############################################
## Three Pass Regression Filter Estimators ##
#############################################
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
##' @param check_missing Checks for missing values
##' @param closed_form Use closed form estimation
##' @param fitalg OLS fit algorithm
##' @return TPRF object
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
TPRF <- function(X, y, Z=NULL, L=NULL,
                 center=FALSE, scale=TRUE,
                 check_missing=FALSE,
                 closed_form=FALSE,
                 fitalg=2) {

    ## y should be univariate
    if (NCOL(y) != 1) {
        stop("y should be univariate")
    }
    ## Both proxies Z and the number automatic proxies cannot be unspecified
    if(is.null(Z) && is.null(L)) {
        stop("please either provide proxies or choose a number of automatic proxies to build")
    }

    ## Missing Values not handled in closed form estimator
    if (closed_form && check_missing) {
        stop("missing values not handled in closed form estimator")
    }

    if (check_missing) {
        valid_idx <- .get_valid_idx(X, y, Z)
    } else {
        valid_idx <- NULL
    }

    ## Scaling and centering
    ## Should be parallelized, moved to C, or maybe use matrixstats
    if(center) {
        X_mean <- apply(X, 2, function(col) mean(col, na.rm=TRUE))
        X <- t(apply(X, 1, function(row) row - X_mean))
    } else {
        X_mean <- NULL
    }

    if (scale) {
        X_sd <- apply(X, 2, function(col) sd(col, na.rm=TRUE))
        X <- t(apply(X, 1, function(row) row/X_sd))
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
            r[, k] <- resid(.tprf_fit(X, y, r[, k - 1],
                                      valid_idx=valid_idx,
                                      closed_form=closed_form,
                                      fitalg=fitalg))
        }
        Z <- r
    }

    ## Running three pass regression filter
    fit <- .tprf_fit(X, y, Z, valid_idx=valid_idx,
                     closed_form=closed_form, fitalg=fitalg)

    ## Creating TPRF object
    structure(list(fit=fit,
                   L=L,
                   loadings=fit$loadings,
                   factors=fit$factors,
                   closed_form=closed_form,
                   alpha_hat=fit$alpha_hat,
                   centered=center,
                   means=X_mean,
                   scaled=scale,
                   scales=X_sd
                   ),
              class="t3prf")
}

##' @export
.tprf_fit <- function(X, y, Z, valid_idx, closed_form, fitalg=2) {

    if(!is.matrix(X)) X <- as.matrix(X)
    if(!is.matrix(y)) y <- as.matrix(y)
    if(!is.matrix(Z)) Z <- as.matrix(Z)

    if(closed_form) {
        return(.tprf_fit_closed(X, y, Z))
    } else {
        return(.tprf_fit_iter(X, y, Z, valid_idx=valid_idx, fitalg=fitalg))
    }
}

##' @export
.tprf_fit_iter <- function(X, y, Z, valid_idx, fitalg=2) {


    if(is.null(valid_idx)) {
        valid_idx <- matrix(TRUE, nrow=NROW(X), ncol=NCOL(X))
    }

    ## Step 1 Time series regression
    ## Setting proxies with intercept
    Z_intercept <- cbind(1, Z)

    loadings <- mat.or.vec(nr=NCOL(X), nc=NCOL(Z) + 1)

    for (j in 1:NCOL(X)) {
        idx_j <- valid_idx[, j]
        loadings[j, ] <- coef(RcppEigen::fastLmPure(Z_intercept[idx_j, ],
                                                    X[idx_j, j],
                                                    method=fitalg))
    }

    ## Step II Cross section regression
    loadings_intercept <- cbind(1, loadings[, -1, drop=FALSE])
    factors <- mat.or.vec(nr=NROW(y), nc=NCOL(Z) + 1)

    for (i in 1:NROW(factors)) {
        idx_i <- valid_idx[i, ]
        factors[i, ] <- coef(RcppEigen::fastLmPure(loadings_intercept[idx_i, ],
                                                   X[i, idx_i],
                                                   method=fitalg))
    }
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

##' @export
.tprf_fit_closed <- function(X, y, Z) {
    J <- function(len) {
        diag(rep(1, len)) - 1/len * matrix(1, nrow=len, ncol=len)
    }
    T <- NROW(X)
    N <- NCOL(X)

    W_XZ <- J(N) %*% t(X) %*% J(T) %*% Z
    S_XX <- t(X) %*% J(T) %*% X
    S_Xy <- t(X) %*% J(T) %*% y

    alpha_hat <- W_XZ %*% solve(t(W_XZ) %*% S_XX %*% W_XZ) %*% t(W_XZ) %*% S_Xy
    y_hat <- mean(y) + J(T) %*% X %*% alpha_hat

    ## part1 <- J(T) %*% X %*% W_XZ
    ## part2 <- solve(t(W_XZ) %*% S_XX %*% W_XZ)
    ## part3 <- t(W_XZ) %*% S_Xy
    ## y_hat <- mean(y) + part1 %*% part2 %*% part3

    fit <- list()
    fit$alpha_hat <- alpha_hat
    fit$fitted.values <- as.vector(y_hat)
    fit$residuals <- as.vector(y - y_hat)
    return(fit)
}
