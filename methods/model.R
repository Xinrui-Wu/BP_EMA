## Models for BP-EMA data
## script by : Xinrui Wu

## library
library(tidyverse)
library(ggplot2)


## data
blake = readRDS('./Blake_EMA.RDS')
bh = readRDS('./BH_SI.RDS')

## data manipulation
source('./dataManipulation.R')
time.window = c(0.5, 1, 2, 3, 5)
n_win = length(time.window)
bp_ema = BPEMA(bh, blake, time.window)

## models 

# reshape the data
bp_ema_long = as.data.frame(rbind(as.matrix(bp_ema[,c(1,5, seq(from = 7, to = 5+2*n_win, by = 2))]), 
                                  as.matrix(bp_ema[,c(1,5, seq(from = 8, to = 6+2*n_win, by = 2))])))
colnames(bp_ema_long) = c("ID", "si_score", paste("k.", time.window, sep = ""))
bp_ema_long = mutate(bp_ema_long, post = c(rep(0, dim(bp_ema)[1]), rep(1, dim(bp_ema)[1]))) 

# m1: without covariate
m1.3 = glm(k.3 ~ post, data = bp_ema_long, family = "poisson")
summary(m1.3)


# m2: with si_score -- truncate
truncateSI = function(si){
  result = 1*(0<=si & si<=3) +
    2*(4<=si & si<=16) +
    3*(17<=si & si<=27)
  return(as.factor(result))
}
m2.3 = glm(k.3 ~ post*truncateSI(si_score), data = bp_ema_long, family = "poisson")
summary(m2.3)


# m3: with si_score -- linear
m3.3 = glm(k.3 ~ post*si_score, data = bp_ema_long, family = "poisson")
summary(m3.3)


# m4: with si_score -- quadratic
quadraticSI = function(si){return(si^2)}
m4.3 = glm(k.3 ~ post*si_score + post*quadraticSI(si_score), data = bp_ema_long, family = "poisson")
summary(m4.3)


# m5: with si_score -- B-spline
library(splines)
m5.3 = glm(k.3 ~ post*bs(si_score, df = 3), data = bp_ema_long, family = "poisson")
summary(m5.3)






