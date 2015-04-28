## * Various getters
##' @export
coef.t3prf <- function(object, ...) coef(object$fit)
##' @export
fitted.t3prf <- function(object, ...) fitted(object$fit)
##' @export
residuals.t3prf <- function(object, ...) resid(object$fit)
## * Prediction
##' @export
predict.t3prf <- function(object, newdata, fitalg=2, ...) {
    ## Observations should be arrayed in rows
    ## Step I
    loadings_intercept <- cbind(1, object$loadings)
    if (object$scaled) {
        newdata <- apply(newdata, 1, function(X) X/object$scales)
    }
    newdata_factors<- apply(newdata, 1,
                            function(X) coef(RcppEigen::fastLmPure(loadings_intercept,
                                                        X,
                                                        method=fitalg))[-1])

    newdata_factors <- matrix(newdata_factors, ncol=object$L, byrow=TRUE)
    ## Adding intercept to newly derived factors
    newdata_factors <- cbind(1, newdata_factors)
    newdata_factors %*% coef(object)
}
