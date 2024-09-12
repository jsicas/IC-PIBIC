require(wavethresh)

# Leitura de dados
db <- read.csv('../dados/spike_sorting_reduzido.csv')

#------- Wavelets -------#
ywd <- wd(db$V1[1:1024], filter.number=10, family='DaubExPhase')  # DWT
ywd.T <- threshold(ywd, policy='cv')  # Thresholding
fun.recuperada <- wr(ywd.T)  # IDWT

par(mfrow=c(2,2), mai=c(1,0.9,0.6,0.3), cex.lab=1.5, cex.main=1.5)
plot(db$V1[1:1024], type='l', xlab='Tempo', ylab='Potencial de Ação Neuronal', main='(a) Função observada', cex.lab=1.3, cex.axis=1.5, cex.main=1.5)
plot(fun.recuperada, type='l', xlab='Tempo', ylab='Potencial de Ação Neuronal', main='(b) Função recuperada', cex.lab=1.3, cex.axis=1.5, cex.main=1.5)
plot(ywd, main='(c) Coeficientes empíricos de ondaletas', sub='', xlab='Translação', ylab='Nível de Resolução')
plot(ywd.T, main='(d) Coeficientes estimados de ondaletas', sub='', xlab='Translação', ylab='Nível de Resolução')
