library(wavethresh)

y <- c(1,1,7,9,2,8,8,6)
ywd <- wd(y, filter.number=1, family='DaubExPhase') # Haar DWT

plot(ywd, main=NULL, sub=NULL,
     ylab='Nível de Resolução', xlab='Translação')