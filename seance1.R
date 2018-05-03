# au prealable, vous devez executer l'instruction suivante
# install.packages('randtoolbox')

library(randtoolbox)
source('generateurs.R')

sMT <- 2504
Nsimu <- 1000
Nrepet <- 20


############################################################
##  Section 2
############################################################

sob <- Sobol(Nsimu,Nrepet)
mt <- MersenneTwister(Nsimu,Nrepet,sMT)$x

par(mfrow=c(1,2))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(sob[,1],xlab='',main='Sobol')

par(mfrow=c(1,2))
plot(mt[,1:2],xlab='dimension 1', ylab='dimension 2', main='Mersenne Twister')
plot(sob[,1:2],xlab='dimension 1', ylab='dimension 2', main='Sobol')

# Sequence de bits pour les tests
(bit_mt <- binary(mt[1,1]))
