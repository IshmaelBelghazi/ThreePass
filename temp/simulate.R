##' Simulates Factors for the three pass regression filter
##'
##'
##' @title sim_factors
##' @param n sample size
##' @param rho_f serial correlation of relevant factors
##' @param rho_g serial correlation of irrelevant factors
##' @param sigma_g variance of irrelevant factors
##' @return list of relevant and irrelevant factors
##' @author Mohamed Ishmael Diwan Belghazi
sim_factors <- function(n, K_f=1, rho_f=0, rho_g=0,
                        sigma_g=c(1.25, 1.75, 2.25, 2.75)) {

    ## * Simulating relevant factor innovations
    u_f <- matrix(rnorm(n * K_f), ncol=K_f, nrow=n)
    f <- matrix(0, nrow=n, ncol=K_f)
    f[1] <- u_f[1]
    for(i in 2:NROW(f)) {
        f[i] <- rho_f * f[i - 1] + u_f[i]
    }

    ## * Simulating irrelevant factors innovations
    K_g <- length(sigma_g)
    ## Variance have to be adjusted so that the variance of each irrelevant
    ## factor is greater than the variance of the relevant factor by a the
    ## coefficients given in sigma_g
    sigma_g <- diag(var(f) * sigma_g)
    if (K_g > 0) {
        u_g <- matrix(rnorm(n * K_g), nrow=n, ncol=K_g) %*% sqrt(sigma_g)
        g <- matrix(0, nrow=n, ncol=K_g)
        g[1, ] <- u_g[1, ]
        for(i in 2:NROW(g)) {
            g[i, ] <- rho_g * g[i - 1, ] + u_g[i, ]
        }
    } else {
        g <- NULL
    }
    ## * returning factors
    return(list(f=f, g=g))
}
##' @export
sim_target <- function(factors, beta_0, beta, sigma_y=1) {
    n <- length(factors$f)
    ## * Generating innovations
    u_y <- matrix(rnorm(n), nrow=n, ncol=1)
    y <- beta_0 + factors$f %*% beta + sigma_y * rnorm(n)
    return(y)
}
sim_observations <- function(factors) {}
