# -------- Shrinkage da logistica -------- #
# d - coeficientes empiricos
# a - alpha
# s - desvio padrão
# t - parâmetro da logistica

LogisShrink <- function(d, a, s, t){
  u <- rnorm(5000)
  delta <- vector(mode='double')
  
  for(i in 1:length(d)){
    int1 <- mean((s*u + d[i]) * dlogis(s*u + d[i], scale=t))
    int2 <- mean(dlogis(s*u + d[i], scale=t))
    delta[i] <- (1-a) * int1/(a * dnorm(d[i], sd=s)/s + (1-a) * int2)
  }
  
  return(delta)
}


# -------- Exemplo -------- #
# Definindo parâmetros
x <- seq(-7, 7, 0.05)
t <- c(3, 5, 10, 20, 30, 40, 50)
a <- c(0.3, 0.5, 0.8, 0.9, 0.99)


# Variando t e alpha
## Variando t para alpha=0.8
par(mfrow=c(1,2), cex.axis=1.3, cex.main=1.3, mai=c(0.5, 0.7, 0.7, 0.5))

plot(x=1, main='Variando t', type='n',
     xlab='', ylab='', xlim=c(-6,6), ylim=c(-6,6))
abline(v=c(-3, 0, 3), h=0, lty=c(1,9,1,9))
for (i in 1:length(t)){
  lines(x, LogisShrink(x, 0.8, 1, t[i]), col=i)
}


## Variando alpha para t=10
plot(x=1, main=expression(bold(Variando ~ alpha)), type='n',
     xlab='', ylab='', xlim=c(-6,6), ylim=c(-6,6))
abline(v=c(-3, 0, 3), h=0, lty=c(1,9,1,9))
for (i in 1:length(a)){
  lines(x, LogisShrink(x, a[i], 1, 10), col=i)
}


# 3 modelos para alpha=0.6,0.9,0.99 e variando t
par(mfrow=c(1,3), cex.axis=1.6, cex.main=2.2, mai=c(0.5, 0.5, 0.5, 0.2))

# alpha=0.6
plot(x=1, main=expression(bold(alpha == 0.6)), type='n',
     xlab='', ylab='', xlim=c(-6,6), ylim=c(-6,6))
abline(v=c(-3, 0, 3), h=0, lty=c(1,9,1,9))
for (i in 1:length(a)){
  lines(x, LogisShrink(x, 0.6, 1, t[i]), col=i)
}

# alpha=0.9
plot(x=1, main=expression(bold(alpha == 0.9)), type='n',
     xlab='', ylab='', xlim=c(-6,6), ylim=c(-6,6))
abline(v=c(-3, 0, 3), h=0, lty=c(1,9,1,9))
for (i in 1:length(a)){
  lines(x, LogisShrink(x, 0.9, 1, t[i]), col=i)
}

# alpha=0.99
plot(x=1, main=expression(bold(alpha == 0.99)), type='n',
     xlab='', ylab='', xlim=c(-6,6), ylim=c(-6,6))
abline(v=c(-3, 0, 3), h=0, lty=c(1,9,1,9))
for (i in 1:length(a)){
  lines(x, LogisShrink(x, 0.99, 1, t[i]), col=i)
}
