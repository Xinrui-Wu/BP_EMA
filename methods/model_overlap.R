## Models for BP-EMA data  ---- Overlap data
## script by : Xinrui Wu

## Description: Sensitivity test for overlap data


## data manipulation (overlap)
source('./dataManu_sens.R')
time.window = c(0.5, 1, 2, 3, 5)
n_win = length(time.window)
bp_ema_overlap = BPEMA_overlap(bh, blake, time.window)


#  compared with non-overlap data
library(gridExtra)
grid.arrange(plot_BPEMA(bp_ema, time.window), 
             plot_BPEMA_overlap(bp_ema_overlap, time.window), 
             nrow = 1)


## models for overlap data
# reshape the data
bp_ema_overlap_long = as.data.frame(rbind(as.matrix(bp_ema_overlap[,c(1,5, seq(from = 7, to = 5+2*n_win, by = 2))]), 
                                  as.matrix(bp_ema_overlap[,c(1,5, seq(from = 8, to = 6+2*n_win, by = 2))])))
colnames(bp_ema_overlap_long) = c("ID", "si_score", paste("k.", time.window, sep = ""))
bp_ema_overlap_long = mutate(bp_ema_overlap_long, post = c(rep(0, dim(bp_ema_overlap)[1]), rep(1, dim(bp_ema_overlap)[1]))) 

# m1: without covariate
m1.3_o = glm(k.3 ~ post, data = bp_ema_overlap_long, family = "poisson")
summary(m1.3)


# m2: with si_score -- linear
m2.3_o = glm(k.3 ~ post*si_score, data = bp_ema_overlap_long, family = "poisson")
summary(m2.3)

post2.3_o = predict(m2.3_o, data.frame(si_score = 0:27, post = 1), type = "response", se.fit = TRUE)
pre2.3_o = predict(m2.3_o, data.frame(si_score = 0:27, post = 0), type = "response", se.fit = TRUE)

split.screen(c(1,2))
screen(1)
plot(0:27, post2.3_o$fit,type = "l", axes = FALSE, ylab = "Expect time of presses", xlab = "SI score", 
     main = "Linear (SI score in the model)") 
points(0:27, post2.3_o$fit + 1.96 * post2.3_o$se.fit,  type = "l", lty = 2)
points(0:27, post2.3_o$fit - 1.96 * post2.3_o$se.fit, type = "l", lty = 2)
points(0:27, pre2.3_o$fit,  col = "red", type = "l")
points(0:27, pre2.3_o$fit + 1.96 * pre2.3_o$se.fit, col = "red", type = "l", lty = 2)
points(0:27, pre2.3_o$fit - 1.96 * pre2.3_o$se.fit, col = "red", type = "l", lty = 2)
legend(1, 1, legend=c("post", "pre"),
       col=c("black", "red"), lty=1, cex=0.8)
axis(side = 1)
axis(side = 2)


# m3: with si_score -- B-spline
library(splines)
m3.3_o = glm(k.3 ~ post*bs(si_score, df = 3), data = bp_ema_overlap_long, family = "poisson")
summary(m3.3)


screen(2)
post3.3_o = predict(m3.3_o, data.frame(si_score = 0:27, post = 1), type = "response", se.fit = TRUE)
pre3.3_o = predict(m3.3_o, data.frame(si_score = 0:27, post = 0), type = "response", se.fit = TRUE)

plot(0:27, post3.3_o$fit,type = "l", axes = FALSE, ylab = "Expect time of presses", xlab = "SI score", 
     main = "B spline (SI score in the model)", xlim = c(0, 28)) 
points(0:27, post3.3_o$fit + 1.96 * post3.3_o$se.fit,  type = "l", lty = 2)
points(0:27, post3.3_o$fit - 1.96 * post3.3_o$se.fit, type = "l", lty = 2)
points(0:27, pre3.3_o$fit,  col = "red", type = "l")
points(0:27, pre3.3_o$fit + 1.96 * pre3.3_o$se.fit, col = "red", type = "l", lty = 2)
points(0:27, pre3.3_o$fit - 1.96 * pre3.3_o$se.fit, col = "red", type = "l", lty = 2)
legend(1, 0.8, legend=c("post", "pre"),
       col=c("black", "red"), lty=1, cex=0.8)
axis(side = 1)
axis(side = 2)
close.screen(all.screens = T)




