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

        loadings_reg <- object$loadings[, -1]
        if(!object$pls) loadings_reg <- cbind(1, loadings_reg)
        newdata_factors <- apply(newdata, 1,
                                 function(row) {
                                     ## Omitting missing values
                                     valid_idx <- !is.na(row)
                                     coef(RcppEigen::fastLmPure(loadings_reg[valid_idx, , drop=FALSE],
                                                                row[valid_idx],
                                                                method=fitalg))[-1]
                                 })

        newdata_factors <- matrix(newdata_factors, ncol=object$L, byrow=TRUE)
        ## Adding intercept to newly derived factors
        newdata_factors <- cbind(1, newdata_factors)
        return(newdata_factors %*% coef(object))
    }
}
