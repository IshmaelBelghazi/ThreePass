##########################################
## Various forecasting helper functions ##
##########################################
## * Recursive predictions
##' @export
predict_recursive <- function(X, y, Z=NULL, L=NULL, pls=FALSE,
                              train_periods=floor(NROW(X)/2),
                              center=FALSE, scale=TRUE, fitalg=2){

    forecasts <- matrix(NA, nrow=NROW(X), ncol=1)
    oos_range <- train_periods:(NROW(X) - 1)

    for (i in oos_range) {
        train_range <- 1:i
        X_i <- X[train_range, ]
        y_i <- y[train_range]
        Z_i <- if(is.null(Z)) NULL else Z[train_range, ]

        rec_fit <- TPRF(X_i, y_i, Z_i, L, pls=pls, center=center, scale=scale, fitalg=fitalg)
        newdata_i <- X[i + 1,, drop=FALSE]

        forecasts[i + 1] <- predict(rec_fit, newdata=newdata_i)
    }
    return(forecasts)
}
## * Rolling predictions
##' @export
predict_rolling <- function(X, y, Z=NULL, L=NULL, pls=FALSE, train_window,
                            inc=1, max_missing=NULL, center=FALSE, scale=TRUE, fitalg=2) {

    train_window <- abs(train_window)
    if (is.null(max_missing)) max_missing <- train_window

    max_missing <- min(train_window, abs(max_missing))

    forecasts <- matrix(NA, nrow=NROW(X), ncol=1)
    roll_range <- train_window:(NROW(X) - 1)
    for(i in roll_range) {
        train_range <- (i - train_window + inc):i
        X_i <- X[train_range, ]
        y_i <- y[train_range]
        Z_i <- if(is.null(Z)) NULL else Z[train_range, ]

        ## Exlude the predictor if the number of missing value is greater than
        ## max_missing
        if(max_missing < train_window) {
            X_i <- X_i[, (apply(X_i, 2, function(X) sum(is.na(X)))) <= max_missing]
        }
        roll_fit <- TPRF(X_i, y_i, Z_i, L, pls=pls, center=center, scale=scale,
                         closed_form=FALSE, fitalg=fitalg)

        newdata_i <- X[i + 1,, drop=FALSE]
        forecasts[i + 1] <- predict(roll_fit, newdata=newdata_i)
    }
    return(forecasts)
}
