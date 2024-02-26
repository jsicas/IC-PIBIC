library(wavethresh)

y = c(1,1,7,9,2,8,8,6)

ywt <- wd(y, filter.number=1, family='DaubExPhase')
ywt_t_hard <- threshold(ywt, policy='manual', type='hard', value=2, levels=0:2)
ywr_hard <- wr(ywt_t_hard, filter.number=1, family='DaubExPhase')

ywt <- wd(y, filter.number=1, family='DaubExPhase')
ywt_t_soft <- threshold(ywt, policy='manual', type='soft', value=2, levels=0:2)
ywr_soft <- wr(ywt_t_soft, filter.number=1, family='DaubExPhase')

plot(y, type='b', xlab='x' ,ylab='y')
lines(ywr_hard, type='b', col='red')
lines(ywr_soft, type='b', col='blue')
legend('bottomright', pch = 16, bty='n', cex=0.9,
       legend=c('Dados', 'Limiar duro', 'Limiar suave'),
       col=c('black', 'red', 'blue'))