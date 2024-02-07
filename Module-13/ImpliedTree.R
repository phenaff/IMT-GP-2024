## Derman-Kani Trinomial Implied Tree

library(fOptions)
source('~/dev/R/Pricing/CRRTrinomial.R')
source('~/dev/R/Pricing/TrinomialTreePlot.R')

## vol function
sigma <- .3
a <- -.2/100
b <- 0
bsvol <- function(S,T) {
  sigma*(1+a*(S-100)) + b*T
}

## all rates = 0
r <- 0
b <- 0

## time steps
n <- 3
dt <- .01

nr <- 2*(n+1)-1
nc <- (n+1)
T <- seq(0, nc) * dt

## Trinomial tree
u <- exp(sigma*sqrt(2*dt))
d <- 1/u
## constant prob
pu <- ((exp(r*dt/2)-exp(-sigma*sqrt(dt/2)))/(exp(sigma*sqrt(dt/2))-exp(-sigma*sqrt(dt/2))))^2
pd <- (1-sqrt(pu))^2
pm <- 1-pu-pd
## matrice du sous-jascent
S <- matrix(data=NA, nrow=nr, ncol=nc)
for(j in seq(1,nc)) {
  S[1:(1+2*(j-1)),j] <- 100 * u^(j-1) * d^seq(0, 2*(j-1))
}

x11()
TrinomialTreePlot(S, dx = -0.025, dy = 0.4, cex = 1, 
    digits = 1, main='Constant Volatility Tree') 
 
## matrice des prix d'etat
Lambda <- matrix(data=NA, nrow=nr, ncol=nc)
Lambda[1,1] = 1.0

## matrice des vol BS 

Vol <- matrix(data=NA, nrow=nr, ncol=nc)
for(i in seq(1,nr)) {
  for(j in seq(1, nc))
    if(!is.na(S[i,j])) Vol[i,j] <- bsvol(S[i,j], T[j])
}

## matrice des prix call/put 
Call <- matrix(data=NA, nrow=nr, ncol=nc)
Put <- matrix(data=NA, nrow=nr, ncol=nc)
for(i in seq(1,nr)) {
  for(j in seq(2, nc))
    if(!is.na(S[i,j])) {
        Call[i,j] <- CRRTrinomial('ce', S[1,1], S[i,j], T[j], r, b, Vol[i,j], j-1)$price
        Put[i,j] <- CRRTrinomial('pe', S[1,1], S[i,j], T[j], r, b, Vol[i,j], j-1)$price
    }
}

x11()
par(mfrow=c(1,2))
TrinomialTreePlot(Call, dx = -0.025, dy = 0.4, cex = 1, 
    digits = 1, main='Call Value', ylim=c(-(nc-1), nc)) 
TrinomialTreePlot(Put, dx = -0.025, dy = 0.4, cex = 1, 
    digits = 1, main='Put Value',ylim=c(-(nc-1), nc)) 
par(mfrow=c(1,1))

## probabilite de transition p au noeud [i,j]
p <- function(i, j) {
  # S'_{i+1} and S'_{i+2}
  SP1 <- S[i+1, j+1]
  SP2 <- S[i, j+1]
  # vector of lambdas
  tmp = 0
  if(i>1) {
    l <- Lambda[1:(i-1),j]
    F <- S[1:(i-1),j]*exp(r*dt)
    #print(paste('F: ', F, ' SP1: ', SP1, ' SP2: ', SP2))
    tmp = t(l) %*% (F-SP1)
  }
  # prob
  (exp(r*dt)*Call[i+1,j+1] - tmp)/(Lambda[i,j]*(SP2-SP1))
}  (exp(r*dt)*Call[i+1,j+1] - tmp)/(Lambda[i,j]*(SP2-SP1))

## prob q 
q <- function(i, j) {
  # S'_{i+2}, S'_{i+1} and S'_{i}
  SP2 <- S[i, j+1]
  SP1 <- S[i+1, j+1]
  SP <- S[i+2, j+1]
  F <- S[i,j]*exp(r*dt)
  (F-p(i,j)*(SP2-SP1)-SP1)/(SP-SP1)
}

## transition probabilities at original node
Pu <- p(1,1)
Pd <- q(1,1)
Pm <- 1-Pu-Pd

## state prices for time step dt
Lambda[1,2] <- Pu * exp(-r*dt) 
Lambda[2,2] <- Pm * exp(-r*dt)
Lambda[3,2] <- Pd * exp(-r*dt)

## state prices for time step 2*dt
Lambda[1,3] <- p(1,2)*Lambda[1,2]
Lambda[2,3] <- (1-p(1,2)-q(1,2))*Lambda[1,2] + p(2,2)*Lambda[2,2]
Lambda[3,3] <- q(1,2)*Lambda[1,2] + (1-p(2,2)-q(2,2))*Lambda[2,2] + p(3,2)*Lambda[3,2]
Lambda[4,3] <- (1-p(3,2)-q(3,2))*Lambda[3,2] + q(2,2)*Lambda[2,2]
Lambda[5,3] <- q(3,2)*Lambda[3,2]

## state prices for time step 3*dt
Lambda[1,4] <- p(1,3)*Lambda[1,3]
Lambda[2,4] <- (1-p(1,3)-q(1,3))*Lambda[1,3] + p(2,3)*Lambda[2,3]
Lambda[3,4] <- q(1,3)*Lambda[1,3] + (1-p(2,3)-q(2,3))*Lambda[2,3] + p(3,3)*Lambda[3,3]
Lambda[4,4] <- q(2,3)*Lambda[2,3] + (1-p(3,3)-q(3,3))*Lambda[3,3] + p(4,3)*Lambda[4,3]
Lambda[5,4] <- q(3,3)*Lambda[3,3] + (1-p(4,3)-q(4,3))*Lambda[4,3] + p(5,3)*Lambda[5,3]
Lambda[6,4] <- (1-p(5,3)-q(5,3))*Lambda[5,3] + q(4,3)*Lambda[4,3]
Lambda[7,4] <- q(5,3)*Lambda[5,3]

## verify consistancy of state prices
apply(Lambda, 2, function(x){sum(x[!is.na(x)])})

## matrix of up, down, mid probabilities

Pup <- matrix(data=NA, nrow=nr, ncol=nc)
Pdn <- matrix(data=NA, nrow=nr, ncol=nc)
Pmd <- matrix(data=NA, nrow=nr, ncol=nc)
for(i in seq(1,nr)) {
  for(j in seq(1, nc-1))
    if(!is.na(S[i,j])) {
      Pup[i,j] <- p(i,j)
      Pdn[i,j] <- q(i,j)
      Pmd[i,j] <- 1-Pup[i,j]-Pdn[i,j]
    }
}

x11()
par(mfrow=c(1,2))
TrinomialTreePlot(Pup[,1:(nc-1)], dx = -0.025, dy = 0.4, cex = 1, 
    digits = 2, main='Prob Up', ylim=c(-(nc-2), nc-1)) 
TrinomialTreePlot(Pdn[,1:(nc-1)], dx = -0.025, dy = 0.4, cex = 1, 
    digits = 2, main='Prob down', ylim=c(-(nc-2), nc-1)) 
par(mfrow=c(1,1))

## local vol
lvol <- function(i,j) {
  SP2 <- S[i, j+1]
  SP1 <- S[i+1, j+1]
  SP <- S[i+2, j+1]
  F <- S[i,j]*exp(r*dt)
  sqrt((Pup[i,j]*(SP2-F)^2 + Pdn[i,j]*(SP-F)^2 + Pmd[i,j]*(SP1-F)^2)/(F^2*dt))
}

LVol <- matrix(data=NA, nrow=nr, ncol=nc)
for(i in seq(1,nr)) {
  for(j in seq(1, nc-1))
    if(!is.na(S[i,j])) {
      LVol[i,j] <- lvol(i,j)
    }
}

x11()
par(mfrow=c(1,2))
TrinomialTreePlot(LVol[,1:(nc-1)]*100, dx = -0.025, dy = 0.4, cex = 1, 
    digits = 1, main='Local Vol', ylim=c(-(nc-2), nc-1)) 
TrinomialTreePlot(Vol[,1:(nc)]*100, dx = -0.025, dy = 0.4, cex = 1, 
    digits = 1, main='BS Vol', ylim=c(-(nc-1), nc)) 
par(mfrow=c(1,1))
