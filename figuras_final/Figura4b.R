a <- c(0.2, 0.5, 0.7, 0.9)
x <- seq(-7, 7, 0.05)

# funcao utilizada
source('../funcoes_criadas/LogisShrink.R')

## Variando alpha para t=50
par(mai=c(1,0.55,0.2,0.35))
plot(x=1, type='n',
     xlab='', ylab='', xlim=c(-6,6), ylim=c(-6,6))
abline(v=0, h=0, lty=1)
for (i in 1:length(a)){
  lines(x, LogisShrink(x, a[i], 1, 50), col=i, lwd=2)
}
legend('bottomright', legend=c(expression(alpha == 0.2),
                               expression(alpha == 0.5),
                               expression(alpha == 0.7),
                               expression(alpha == 0.9)),
       bty='n', lwd=2, col=1:4, cex=0.8)
