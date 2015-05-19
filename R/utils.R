#######################
## Various Utilities ##
#######################
## * Missing Values Handling
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
## * Array Corruption
##' @export
corrupt <- function(mat, pollutant,
                    corrupt_prob=1,
                    n_corrupted=floor(length(mat)/2)) {

    if(n_corrupted <= 0) {
        stop("number of entries to corrupt has to be positive")
    }
    if(n_corrupted > length(mat)) {
        stop("number of entries to corrupt than entries")
    }
    if(is.function(pollutant)) {
        if (!("..." %in% names(formals(pollutant)))) {
            formals(pollutant) <- c(formals(pollutant), alist(...= ))
        }
        cor_fun <- pollutant
    } else {
        cor_fun <- function(x, ...) pollutant
    }
    ## Sampling indices to corrupt
    corrupt_idx <- (function(x) x[sample(NROW(x), n_corrupted),])(expand.grid(1:NROW(mat), 1:NCOL(mat)))
    apply(corrupt_idx, 1, function(id) {
        if(runif(1) <= corrupt_prob) {
            mat[id[1], id[2]] <<- cor_fun(mat[id[1]], mat[id[2]])
        }
    })

    return(mat)

}
