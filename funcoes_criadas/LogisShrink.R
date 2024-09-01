# Shrinkage da logistica

# d - coeficientes empiricos
# a - alpha
# s - desvio padrão
# t - parâmetro da logistica (tau)

LogisShrink <- function(d, a, s, t)
{
  u <- rnorm(10000)
  delta <- vector(mode='double')
  
  for(i in 1:length(d)) {
    int1 <- mean((s*u + d[i]) * dlogis(s*u + d[i], scale=t))
    int2 <- mean(dlogis(s*u + d[i], scale=t))
    delta[i] <- (1-a) * int1/(a * dnorm(d[i], sd=s)/s + (1-a) * int2)
  }
  return(delta)
}