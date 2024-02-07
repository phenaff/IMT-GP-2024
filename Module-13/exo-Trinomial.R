# Arbre trinomial Jarrow-Rudd

T <- T
S.0 <- 100
N <- 200
sigma <- .3
r <- .02
K <- 100
mu <- r - (1/2)*sigma^2
dt <- T/N

u <- exp(sigma*sqrt(2*dt))

iExp <- (-N):N

S <- S.0 * exp(mu*T) * u^iExp

#V <- pmax(S-K, 0)
p.u <- 1/4
p.m <- 1/2
p.d <- 1/4
df <- exp(-r*dt)

# Vérification: actualisation du prix à terme
V <- S
for(i in seq(N, 1, -1)) {
  # etape i: 2i+1 noeuds
  V <- df*(p.u*V[3:(2*i+1)] + p.m*V[2:(2*i)] + p.d* V[1:(2*i-1)])
}
print(V)

# Option Euro
V <- pmax(S-K, 0)
for(i in seq(N, 1, -1)) {
  # etape i: 2i+1 noeuds
  V <- df*(p.u*V[3:(2*i+1)] + p.m*V[2:(2*i)] + p.d* V[1:(2*i-1)])
}
# Formule analytique
P <- GBSOption(TypeFlag="c", S=S.0, X=K, Time=T, r=r, b=r, sigma=sigma)@price

print(c(V, P))


  
