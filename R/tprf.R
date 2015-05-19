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
##' @param pls Partial Least Squares
##' @param center Center the data?
##' @param scale Scale the data
##' @param closed_form Use closed form estimation
##' @param fitalg OLS fit algorithm
##' @return TPRF object
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
TPRF <- function(X, y, Z=NULL, L=NULL, pls=FALSE,
                 center=FALSE, scale=TRUE,
                 closed_form=FALSE,
                 fitalg=2) {

    ## y should be univariate
    if (NCOL(y) != 1) {
        stop("y should be univariate")
    }
    ## partial least squares is equivalent to TPRF with no intercept,
    ## standardized predictors and L=1
    if (pls) {
        center <- TRUE
        scale <- TRUE
        if(closed_form) L <- 1
    }

    ## Both proxies Z and the number automatic proxies cannot be unspecified
    if(is.null(Z) && is.null(L)) {
        stop("please either provide proxies or choose a number of automatic proxies to build")
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
                                      pls=pls, closed_form=closed_form,
                                      fitalg=fitalg))
        }
        Z <- r
    }

    ## Running three pass regression filter
    fit <- .tprf_fit(X, y, Z,
                     pls=pls, closed_form=closed_form,
                     fitalg=fitalg)

    ## Creating TPRF object
    structure(list(fit=fit,
                   pls=pls,
                   L=L,
                   loadings=fit$loadings,
                   factors=fit$factors,
                   closed_form=closed_form,
                   alpha_hat=fit$alpha_hat,
                   centered=center,
                   means=X_mean,
                   scaled=scale,
                   scales=X_sd),
              class="t3prf")
}

##' @export
.tprf_fit <- function(X, y, Z, pls, closed_form, fitalg=2) {

    if(!is.matrix(X)) X <- as.matrix(X)
    if(!is.matrix(y)) y <- as.matrix(y)
    if(!is.matrix(Z)) Z <- as.matrix(Z)

    if(closed_form) {
        return(.tprf_fit_closed(X, y, Z, pls=pls))
    } else {
        return(.tprf_fit_iter(X, y, Z, pls=pls, fitalg=fitalg))
    }
}

##' @export
.tprf_fit_iter <- function(X, y, Z, pls, fitalg=2) {

    ## Pass 1 Time series regression
    ##
    ## Preallcating loadings

    loadings <- matrix(NA, nrow=NCOL(X), ncol= NCOL(Z) + as.numeric(!pls))
    colnames(loadings) <- paste0("phi",
                                 seq(if(pls) 1 else 0,
                                     length.out = NCOL(loadings)))
    if (pls) {
        for (j in 1:NCOL(X)) {
            loadings[j, ] <- coef(lm(formula=X[, j] ~ Z - 1,
                                     na.action=na.exclude, model=FALSE))
        }
    } else {
        for (j in 1:NCOL(X)) {
            loadings[j, ] <- coef(lm(formula=X[, j] ~ 1 + Z,
                                     na.action=na.exclude, model=FALSE))
        }
    }

    ## Pass II Cross section regression
    factors <- matrix(NA, nrow=NROW(X), ncol=NCOL(loadings))
    colnames(factors) <- paste0("F",
                                seq(if(pls) 1 else 0,
                                    length.out = NCOL(factors)))
    ## Loadings has no intercept in pls

    if (pls) {
        for (i in 1:NROW(factors)) {
            factors[i, ] <- coef(lm(formula=X[i,] ~ loadings - 1,
                                    na.action=na.exclude, model=FALSE))
        }
    } else {
        for (i in 1:NROW(factors)) {
            factors[i, ] <- coef(lm(formula=X[i,] ~ 1 + loadings[, -1],
                                    na.action=na.exclude, model=FALSE))
        }

    }

    ## Pass III predictive regression
    ## Factors has no intercept in pls
    factors_reg <- if(pls) factors else factors[, -1, drop=FALSE]
    predictive_reg <- lm(formula=y ~ 1 + factors_reg,
                         na.action=na.exclude, model=FALSE)
    ## Adding loadings and factors to list
    predictive_reg$loadings <- loadings
    predictive_reg$factors <- factors
    return(predictive_reg)
}

##' @export
.tprf_fit_closed <- function(X, y, Z, pls) {

    if (pls) {
        XX <- X %*% t(X)
        part1 <- XX %*% y
        part2 <- solve(t(y) %*% XX %*% XX %*% y)
        part3 <- t(y) %*% XX %*% y
        y_hat <- mean(y) + part1 %*% part2 %*% part3  ## Should y be demeaned?
        alpha_hat <- NULL  ## No alpha_hat in pls

    } else {
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
    }

    fit <- list()
    ##rownames(alpha_hat) <- paste0("alpha", 1:NROW(alpha_hat))
    fit$alpha_hat <- alpha_hat
    y_hat <- as.vector(y_hat)
    names(y_hat) <- as.character(1:length(y_hat))
    fit$fitted.values <- y_hat
    fit$residuals <- as.vector(y - y_hat)
    return(fit)
}
