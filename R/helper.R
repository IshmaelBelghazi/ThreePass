## Various forecasting helper functions
## * Recursive forecasts
##' @export
predict_recursive <- function(X, y, Z=NULL, L=NULL, train_periods=floor(NROW(X)/2),
                              center=TRUE, scale=TRUE, fitalg=2){

    forecasts <- matrix(NA, nrow=NROW(X), ncol=1)
    oos_range <- train_periods:(NROW(X) - 1)
    for (i in oos_range) {
        train_range <- 1:i
        X_i <- X[train_range, ]
        y_i <- y[train_range]
        Z_i <- Z[train_range, ]

        rec_fit <- TPRF(X_i, y_i, Z_i, L, center=center, scale=scale, fitalg=fitalg)
        newdata_i <- X[i + 1,, drop=FALSE]

        forecasts[i + 1] <- predict(rec_fit, newdata=newdata_i)
    }
    return(forecasts)
}
## * Rolling forecasts
##' @export
predict_rolling <- function(X, y, Z=NULL, L=NULL, train_window=60, inc=1, center=TRUE,
                            scale=TRUE, fitalg=2) {

    forecasts <- matrix(NA, nrow=NROW(X), ncol=1)
    roll_range <- train_window:(NROW(X) - 1)
    for(i in roll_range) {
        train_range <- (i - train_window + inc):i
        X_i <- X[train_range, ]
        y_i <- y[train_range]
        Z_i <- Z[train_range, ]

        roll_fit <- TPRF(X_i, y_i, Z_i, L, center=center, scale=scale, fitalg=fitalg)
        newdata_i <- X[i + 1,, drop=FALSE]

        forecasts[i + 1] <- predict(roll_fit, newdata=newdata_i)
    }

    return(forecasts)

}
