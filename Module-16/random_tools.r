sobolInnovations <- function(nbPaths, pathLength) {
#' Random matrix generator
#'
#' @title Generator of Random Matrices (Sobol)
#' @param nbPaths [numeric] number of paths
#' @param pathLength [numeric] number of steps
#'
#' @return a matrix of uniform (0,1) random numbers
#' @export
#'
     innovations = rnorm.pseudo(nbPaths*pathLength, dimension=1, init=T)
     dim(innovations) <- c(nbPaths, pathLength)
     # Return Value:
     innovations }

rnormInnovations <- function(nbPaths, pathLength) {
#' Random matrix generator
#'
#' @title Generator of Random Matrices (rnorm)
#' @param nbPaths [numeric] number of paths
#' @param pathLength [numeric] number of steps
#'
#' @return a matrix of uniform (0,1) random numbers

     innovations = rnorm(nbPaths*pathLength, mean=0, sd=1)
     dim(innovations) <- c(nbPaths, pathLength)
     # Return Value:
     innovations }

logNormal <- function(S0, eps, delta.t, mu, sigma) {
# Log-normal path generator

#' Log-normal dynamic with constant drift and volatility
#' @param S0 [numeric] initial value
#' @param eps [numeric] matrix of uniform (0,1) deviates
#' @param delta.t [numeric] time step
#' @param mu [numeric]  drift (annual rate)
#' @param sigma [numeric] standard deviation (annual rate)

#' @return matrix of paths, one path by column
#'
     b = params[["mu"]]
     sigma = params[["sigma"]]
     n <- dim(eps)[1]

     path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps

     S0*cbind(rep(1, n), exp(t(apply(path, 1, cumsum))))
}

pathSimulator <- function (dtSim=NULL, horizon=NULL, delta.t=NULL,
                           nbPaths=NULL, innovations.gen=sobolInnovations,
                           path.gen=logNormal, path.param,
                           S0=100, antithetic = TRUE,
                           standardization = FALSE, trace = FALSE) {

#' Path simulator
#'
#' Function for generating saple paths, given a generator of random (0,1) sequences and the
#' definition of the dynamic process
#'
#' @param dtSim [timeDate] vector of sampling dates
#' @param horizon [numeric] simulation horizon, in fraction of years
#' @param delta.t [numeric] time step
#' @param nbPaths [numeric] number of paths to be generated
#' @param innovations.gen [function] generator of uniform (0,1) random numbers
#' @param path.gen [function] price dynamic
#' @param path.param [numeric] list of parameters specific to \code{path.gen}
#' @param S0 [numeric] initial value
#' @param antithetic [boolean] draw antithetic variates?
#' @param standardization [boolean] recenter and normalize the output of \code{innovation.gen}?
#' @param trace [boolean] output debugging information?
#'
#' @return time series of paths, one path per column
#'
    if(is.null(dtSim)) {
      # unit is fraction of year
      pathLength <- round(horizon/delta.t)
      delta.t <- horizon/pathLength

      # date sequence starts on 01-jan-2000, arbitrarily.
      # +1 to account for both end points
      # use timeDate to handle fractional days
      dt <- timeDate("2000-01-01") +
        seq(from=0, to=horizon*365*24*3600, length.out=(pathLength+1))
    }
    else {
      pathLength <- length(dtSim)-1
      delta.t <- as.numeric(dtSim[pathLength+1]-dtSim[1])/(365*pathLength)
    }
    if(trace)
      print(dtSim)

    if(trace)
      print(paste("date array length", length(dtSim)))

    if(antithetic)
      nbPaths <- round(nbPaths/2)

    if (trace)
        cat("\nMonte Carlo Simulation Path:\n\n")

    eps = innovations.gen(nbPaths, pathLength)

    if (standardization)
      eps = (eps - mean(eps))/sqrt(var(as.vector(eps)))

    if(trace) {
      cat("summary eps\n")
      print(summary(t(eps)))
      cat("std eps \n")
      print(diag(var(t(eps))))
      cat("eps\n")
      print(eps)
    }

    if (antithetic)
      eps = rbind(eps, -eps)

    path = path.gen(S0, eps, delta.t, path.param)
    if(trace) {
    cat("paths...\n")
    print(dim(path))
    print(length(dtSim))
    summary(t(path))
    }

    timeSeries(t(path), dtSim)
}
