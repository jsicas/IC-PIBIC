db_wv <- readRDS(file='../dados/db_wv.rds')
par(mgp = c(1.9, 0.5, 0), mar = c(3, 3, 0.5, 0.5), cex=0.6)
boxplot(db_wv, xlab='Funções', ylab='EQM')