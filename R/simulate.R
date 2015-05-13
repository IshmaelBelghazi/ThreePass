###################################################
## Three Pass Regression Filter Model Simulation ##
###################################################
##' @export
.sim_factors <- function(T, K_f=1, rho_f=0, rho_g=0,
                        sigma_g=c(1.25, 1.75, 2.25, 2.75)) {

    ## * Simulating relevant factor innovations
    u_f <- matrix(rnorm(T * K_f), ncol=K_f, nrow=T)
    f <- matrix(0, nrow=T, ncol=K_f)
    f[1, ] <- u_f[1, ]
    for(i in 2:NROW(f)) {
        f[i, ] <- rho_f * f[i - 1, ] + u_f[i, ]
    }

    ## * Simulating irrelevant factors innovations
    K_g <- length(sigma_g)
    ## Variance have to be adjusted so that the variance of each irrelevant
    ## factor is greater than the variance of the relevant factor by a the
    ## coefficients given in sigma_g
    sigma_g <- diag(var(f[, 1]) * sigma_g)
    if (K_g > 0) {
        u_g <- matrix(rnorm(T * K_g), nrow=T, ncol=K_g) %*% sqrt(sigma_g)
        g <- matrix(0, nrow=T, ncol=K_g)
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
.sim_target <- function(factors, beta_0, beta, sigma_y=1) {
    F <- do.call(cbind, factors)
    T <- NROW(F)
    ## * Generating innovations
    u_y <- matrix(rnorm(T), nrow=T, ncol=1)
    y <- beta_0 + F %*% beta + sigma_y * u_y

    return(y)
}
##' @export
.sim_observations <- function(N, factors, phi_0, phi) {
    T <- NROW(factors$f)
    F <- do.call(cbind, factors)
    K <- NCOL(F)
    epsilon <- matrix(rnorm(T * N), nrow=T, ncol=N)
    X <- phi_0 +  F %*% t(phi) + epsilon

    return(X)
}
##' export
.sim_proxies <- function(L, factors, lambda_0, lambda) {
    .sim_observations(L, factors, lambda_0, lambda)
}
##' @export
sim_problem <- function(T, N, K_f, sigma_g=c(1.25, 1.75, 2.25, 2.75), L=NULL, sigma_y=1) {

    ## Simulate Factors
    factors <- .sim_factors(T=T, K_f=K_f, sigma_g=sigma_g)
    K <- NCOL(do.call(cbind, factors))
    ## Simulate observations
    phi_0 <- runif(1, -1, 1)
    phi <- matrix(runif(N * K, -1, 1), nrow=N, ncol=K)
    X <- .sim_observations(N, factors, phi_0, phi)
    ## Simulate proxies
    if(!is.null(L)) {
        lambda_0 <- runif(1, -1, 1)
        lambda <- matrix(runif(L * K), nrow=L, ncol=K)
        Z <- .sim_proxies(L, factors, lambda_0, lambda)
    } else {
        lambda_0 <- lambda <- Z <- NULL
    }
    ## Simulate targets
    beta_0 <- runif(1, -1, 1)

    K_g <- K - K_f
    beta <- matrix(c(runif(K_f, -1, 1), rep(0, K - K_f)),
                   nrow=K, ncol=1, byrow=TRUE)
    y <- .sim_target(factors, beta_0, beta)


    simulation <- list(factors=factors,
                       phi_0=phi_0, phi=phi, X=X, K=K, K_f=K_f, K_g=K_g, sigma_g=sigma_g,
                       lambda_0=lambda_0, lambda=lambda, Z=Z,
                       beta_0=beta_0, beta=beta, y=y,
                       sigma_y=sigma_y)

    return(simulation)
}
