######################
## Methods Generics ##
######################
## * Three Pass Regression Filter
## ** Various getters
##' @export
coef.t3prf <- function(object, ...) coef(object$fit)
##' @export
fitted.t3prf <- function(object, ...) fitted(object$fit)
##' @export
residuals.t3prf <- function(object, ...) resid(object$fit)
## ** Prediction
##' @export
predict.t3prf <- function(object, newdata, fitalg=2, ...) {
    ## Observations should be arrayed in rows
    ## Step I
    if (is.vector(newdata)) newdata <- matrix(newdata, nrow=1)

    if (object$scaled) {  ## Object is always scaled in pls fit
        newdata <- t(apply(newdata, 1, function(row) row/object$scales))
    }

    if (object$closed_form) {
        stop("forecasting not supported for closed form estimator")
    } else {

        ## Loadings have no intercept in pls
        if(object$pls) {
            loadings_reg <- object$loadings
        } else {
            loadings_reg <- cbind(1, object$loadings[, -1])
        }

        newdata_factors <- apply(newdata, 1,
                                 function(row) {
                                     coef(lm(row ~ 0 + loadings_reg,
                                             na.action=na.exclude,
                                             model=FALSE))
                                 })

        newdata_factors <- matrix(newdata_factors, nrow=NROW(newdata) , byrow=TRUE)
        newdata_factors <- if(object$pls) newdata_factors else newdata_factors[, -1]
        ## Adding intercept to newly derived factors
        newdata_factors <- cbind(1, newdata_factors)
        return(newdata_factors %*% coef(object))
    }
}
