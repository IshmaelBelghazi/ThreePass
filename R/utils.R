## * Handles missing values
##' @export
.get_valid_idx <- function(predictors, target, proxies) {

    ## ** Checking for missing values in target
    if(any(is.na(target))) {
        stop("one or missing values in target. aborting.")
    }
    ## ** Checking for missing values in Proxies
    if(!is.null(proxies) && any(is.na(proxies))) {
        stop("one or more missing values in proxies. aborting.")
    }
    ## ** Handling missing values in predictors
    pred_valid_idx <- !is.na(predictors)

    return(pred_valid_idx)

}
