##' Trinomial model
##' 
##' Trinomial model for pricing European or American vanilla options.
##' @title Trinomial tree model 
##' @param TypeFlag the option type:
##' \describe{
##' \item{\code{ce}}{European call}
##' \item{\code{pe}}{European put}
##' \item{\code{ca}}{American call}
##' \item{\code{pa}}{American put}
##' }
##' @param S price of underlying asset
##' @param X strike
##' @param Time time to expiry
##' @param r risk-free interest rate
##' @param b cost of carry
##' @param sigma annualized volatility
##' @param n number of steps in tree
##' @return a data structure with the following elements:
##' \describe{
##' \item{\code{param}}{list of input parameters}
##' \item{\code{price}}{price}
##' \item{\code{delta}}{delta}
##' \item{\code{gamma}}{gamma}
##' }
##' @examples
##' res <- CRRTrinomial(TypeFlag="ce", S=100, X=100, Time=1, r=.03, 
##'    b=.03, sigma=.3, n=100) 
##' @export

CRRTrinomial <- function (TypeFlag = c("ce", "pe", "ca", "pa"), S, X, Time, r, 
    b, sigma, n) 
{
    TypeFlag = TypeFlag[1]
    z = NA
    if (TypeFlag == "ce" || TypeFlag == "ca") 
        z = +1
    if (TypeFlag == "pe" || TypeFlag == "pa") 
        z = -1
    if (is.na(z)) 
        stop("TypeFlag misspecified: ce|ca|pe|pa")
    dt = Time/n
    u = exp(sigma * sqrt(2*dt))
    d = 1/u
    dd <- exp(-sigma*sqrt(dt/2))
    pu = ((exp(b * dt/2) - dd)/(1/dd - dd))^2
    pd = (1-sqrt(pu))^2
    pm <- 1-pu-pd
    Df = exp(-r * dt)
    
    # add 1 steps to tree 
    n <- n+1
    # exponent
    iExp <- (1:(2*(n+1)-1))-(n+1)
    OptionValue = z * (S * u^iExp - X)
    OptionValue = (abs(OptionValue) + OptionValue)/2
    if (TypeFlag == "ce" || TypeFlag == "pe") {
        for (j in seq(from = (n), to = 2, by = -1)) 
          for (i in 1:(2*j-1)) 
            OptionValue[i] = (pu*OptionValue[i+2] +
                              pm*OptionValue[i+1] +
                              pd*OptionValue[i]) * Df
    }

    if (TypeFlag == "ca" || TypeFlag == "pa") {
        for (j in seq(from = (n), to = 2, by = -1))
          for (i in 1:(2*j-1)) {
              SS = S * d^(j-1) * u^(i-1)
              exVal =  z * (SS - X)
            OptionValue[i] = (pu*OptionValue[i + 2] +
                              pm*OptionValue[i+1] +
                              pd*OptionValue[i]) * Df
            OptionValue[i] = max(exVal, OptionValue[i])
	  }
    }
    # the middle node is the price
    Sup <- S*u
    Sdown <- S*d
    
    # delta by central difference
    delta <- (OptionValue[3] - OptionValue[1])/(Sup-Sdown)
    du <- (OptionValue[3] - OptionValue[2])/(Sup-S)
    dd <- (OptionValue[2] - OptionValue[1])/(S-Sdown)
    gamma <- (du-dd)/((Sup-Sdown)/2)
 
    param = list(TypeFlag, S, X, Time, r, b, sigma, n)
    list(param, price=OptionValue[2], delta, gamma)
}

