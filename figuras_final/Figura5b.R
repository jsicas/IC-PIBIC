db_sp <- readRDS(file='../dados/db_sp.rds')
par(mgp = c(1.9, 0.5, 0), mar = c(3, 3, 0.5, 0.5), cex=0.6)
boxplot(db_sp, xlab='Funções', ylab='EQM')