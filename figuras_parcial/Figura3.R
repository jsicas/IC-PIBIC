library(latex2exp)

# Funções
## phi_{jk}(x)
phi_jk <- function(x, j, k=0){
  ifelse(k/2^j < x & x <= (k+1)/2^j, 2^(j/2), 0)
}

## Aproximação de Haar para funções no intervalo [0,1]
f_j <- function(x, fun, j, wt=0){
  apx <- 0
  integrais <- vector(mode='numeric')
  pontos <- seq(0,1, length=1 + 2^j)
  
  for (k in 1:(length(pontos)-1)){
    integrais <- append(integrais,
                        integrate(fun, pontos[k], pontos[k+1])$value)
  }
  
  c <- integrais * 2^(j/2)
  
  for (k in 1:length(c)){
    apx <- apx + c[k]*phi_jk(x, j, k-1)
  }
  
  return(apx)
}

# Figura
f <- function(x){4*sin(4*pi*x) - sign(x - 0.3) + sign(x - 0.72)}

curve(f, type='l', n=10000, ylim=c(-6,4), xlim=c(0,1), ylab='y')
par(new=T)
curve(f_j(x, fun=f, j=3), col='red', n=10000, type='s', axes=F,
      ylim=c(-6,4), xlim=c(0,1), xlab='', ylab='')
par(new=T)
curve(f_j(x, fun=f, j=5), col='blue', n=10000, type='s', axes=F,
      ylim=c(-6,4), xlim=c(0,1), xlab='', ylab='')
legend('topright', pch = 16, bty='n', cex=0.9,
       legend=c(TeX(r'($f(x)$)'), TeX(r'($f_3(x)$)'), TeX(r'($f_5(x)$)')),
       col=c('black', 'red', 'blue'))