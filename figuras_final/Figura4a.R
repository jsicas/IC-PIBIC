# Definindo parâmetros
x <- seq(-7, 7, 0.05)
t <- c(1, 20, 50, 100)

# funcao utilizada
source('../funcoes_criadas/LogisShrink.R')

# Variando t e alpha
## Variando t para alpha=0.8
par(mai=c(1,0.55,0.2,0.35))
plot(x=1, type='n',
     xlab='', ylab='', xlim=c(-6,6), ylim=c(-6,6))
abline(v=0, h=0, lty=1)
for (i in 1:length(t)){
  lines(x, LogisShrink(x, 0.7, 1, t[i]), col=i, lwd=2)
}
legend('bottomright', legend=c(expression(tau == 1), expression(tau == 20), expression(tau == 50), expression(tau == 100)), bty='n', lwd=2, col=1:4, cex=0.8)
