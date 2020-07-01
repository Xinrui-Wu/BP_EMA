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


# m2: with si_score -- linear
m2.3 = glm(k.3 ~ post*si_score, data = bp_ema_long, family = "poisson")
summary(m2.3)

post2.3 = predict(m2.3, data.frame(si_score = 0:27, post = 1), type = "response", se.fit = TRUE)
pre2.3 = predict(m2.3, data.frame(si_score = 0:27, post = 0), type = "response", se.fit = TRUE)

(post2.3$fit - 1.96 * post2.3$se.fit) - (pre2.3$fit + 1.96 * pre2.3$se.fit)

plot(0:27, post2.3$fit,type = "l", axes = FALSE, ylab = "Expect time of presses", xlab = "SI score", 
     main = "Linear (SI score in the model)") 
points(0:27, post2.3$fit + 1.96 * post2.3$se.fit,  type = "l", lty = 2)
points(0:27, post2.3$fit - 1.96 * post2.3$se.fit, type = "l", lty = 2)
points(0:27, pre2.3$fit,  col = "red", type = "l")
points(0:27, pre2.3$fit + 1.96 * pre2.3$se.fit, col = "red", type = "l", lty = 2)
points(0:27, pre2.3$fit - 1.96 * pre2.3$se.fit, col = "red", type = "l", lty = 2)
legend(1, 1, legend=c("post", "pre"),
       col=c("black", "red"), lty=1, cex=0.8)
abline(v = 13, col = "gray")
text(x = 11.5, y = 0.03, labels = "SI = 13", col = "gray", cex=0.6)
abline(v = 14, col = "gray")
text(x = 15.5, y = 0.03, labels = "SI = 14", col = "gray", cex=0.6)
axis(side = 1)
axis(side = 2)


# m3: with si_score -- B-spline
library(splines)
m3.3 = glm(k.3 ~ post*bs(si_score, df = 3), data = bp_ema_long, family = "poisson")
summary(m3.3)

post3.3 = predict(m3.3, data.frame(si_score = 0:27, post = 1), type = "response", se.fit = TRUE)
pre3.3 = predict(m3.3, data.frame(si_score = 0:27, post = 0), type = "response", se.fit = TRUE)

(post5.3$fit - 1.96 * post5.3$se.fit) - (pre5.3$fit + 1.96 * pre5.3$se.fit)

plot(0:27, post3.3$fit,type = "l", axes = FALSE, ylab = "Expect time of presses", xlab = "SI score", 
     main = "B spline (SI score in the model)", xlim = c(0, 28)) 
points(0:27, post3.3$fit + 1.96 * post3.3$se.fit,  type = "l", lty = 2)
points(0:27, post3.3$fit - 1.96 * post3.3$se.fit, type = "l", lty = 2)
points(0:27, pre3.3$fit,  col = "red", type = "l")
points(0:27, pre3.3$fit + 1.96 * pre3.3$se.fit, col = "red", type = "l", lty = 2)
points(0:27, pre3.3$fit - 1.96 * pre3.3$se.fit, col = "red", type = "l", lty = 2)
legend(1, 0.8, legend=c("post", "pre"),
       col=c("black", "red"), lty=1, cex=0.8)
abline(v = 13, col = "gray")
text(x = 11.5, y = 0.03, labels = "SI = 13", col = "gray", cex=0.6)
abline(v = 14, col = "gray")
text(x = 15.5, y = 0.03, labels = "SI = 14", col = "gray", cex=0.6)
abline(v = 25, col = "gray")
text(x = 23.5, y = 0.03, labels = "SI = 25", col = "gray", cex=0.6)
abline(v = 26, col = "gray")
text(x = 27.5, y = 0.03, labels = "SI = 26", col = "gray", cex=0.6)
axis(side = 1)
axis(side = 2)





