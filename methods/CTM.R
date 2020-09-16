## Continuous Time Markov Model for BP-EMA data
## script by : Xinrui Wu
## Update Date: Sep 15, 2020


source("./dataManipulation.R")
blake = readRDS('../rawdata/Blake_EMA.RDS')
bh = readRDS('../rawdata/BH_SI.RDS')


#### ------ Manipulate data for estimation ------ ####
library(tidyverse)

ema_ctm = data_rm(transmute(blake, ID, timestamp, SI = sitb_si_sum, study_day = as.numeric(study_day))) %>%
  group_by(ID) %>% 
  mutate(trans_time = c(diff(timestamp), 0) / 3600) %>% # (hour)
  mutate(SI_next = c(SI[-1], 0)) %>%
  filter(trans_time != 0) %>%
  ungroup() %>%
  select(-timestamp) %>% 
  mutate(trans_time = as.numeric(trans_time), change = SI_next - SI)



#### ---------------------- Some summary for the ema_ctm ---------------------- ####
table(abs(ema_ctm$change))
ema_ID = ema_ctm %>%
  group_by(ID) %>% 
  summarize(SI_max = max(c(SI, SI_next)), SI_min = min(c(SI, SI_next)), 
         change_abs_max = max(abs(change)), change_abs_min = min(abs(change)))



#### ---------- Sanity check ---------- ####
si_change = ema_ctm %>%
  transmute(ID = ID, SI = SI, change = change, 
            change_frac = 0+1*(change<0)+2*(change>0))

si_change_summary = si_change %>%
  group_by(SI) %>%
  summarise(up_percent = mean(change>0), 
            down_percent = mean(change<0), 
            same_percent = mean(change==0), 
            num_of_obs = length(unique(ID)), 
            num_of_survey= n())
si_change_summary

    # scatter plot
plot(si_change_summary$SI, si_change_summary$up_percent, pch = 1, col = "red", ylim = c(0, 1), 
     xlab = "SI score", ylab = "Percentage", main = "Change at diffrent SI scores")
points(si_change_summary$SI, si_change_summary$down_percent, pch = 2, col = "blue")
points(si_change_summary$SI, si_change_summary$same_percent, pch = 3, col = "black")
legend(3, 1, legend=c("Increase", "Decrease", "Hold"),
              col=c("red", "blue", "black"), pch=1:3, cex=0.8)
    # box plot
boxplot(change~SI,data=si_change, main="Change at diffrent SI scores", 
        xlab="SI scores", ylab="Change")
abline(h = 0, col = "red")
text(x = 1:28, y = rep(20, 28), labels = si_change_summary$num_of_obs, 
     col = "blue", cex = .6)
text(x = 1:28, y = rep(22, 28), labels = si_change_summary$num_of_survey, 
     col = "pink", cex = .6)
legend(0, -10, legend=c("Total Entries", "Observations"),
       col=c("pink", "blue"), lty = 1, cex=0.8)


boxplot(si_change$change ~ si_change$SI, main="Change at diffrent SI scores", 
        xlab="SI scores", ylab="Change", ylim = c(-20, 27))
abline(h = 0, col = "red")
points(x = 1:28, y = si_change_summary$num_of_obs*9/7 - 20, 
     pch = 2, col = "blue", axes = FALSE, xlab = "", ylab = "")
axis(4, at = seq(0, 35, length.out=8) * 9/7 - 20, labels = seq(0, 35, length.out=8))
text(x = 1:28, y = si_change_summary$num_of_obs*9/7 - 18.5, 
     labels = si_change_summary$num_of_obs, 
     col = "blue", cex = .8)
legend(0, -10, legend="Num. of Obs.",
       col="blue", pch = 2, cex=0.8)


    # --regression-- #
    ## multinomial regression
library(nnet) # for "multinom"
mult_reg = multinom(change_frac ~ SI, data = si_change)
summary(mult_reg)
new_data <- data.frame(SI = 0:27)
pred_mult = predict(mult_reg, newdata = new_data, "probs")
plot(0:27, pred_mult[,3], pch = 1, col = "red", ylim = c(0, 1), 
     xlab = "SI score", ylab = "Percentage", 
     main = "Predicted change at diffrent SI scores", 
     sub = "Multinomial Regression with linear SI")
points(0:27, pred_mult[,2], pch = 2, col = "blue")
points(0:27, pred_mult[,1], pch = 3, col = "black")
legend(3, 1, legend=c("Increase", "Decrease", "Hold"),
       col=c("red", "blue", "black"), pch=1:3, cex=0.8)
    ## multinomial regression & Bspline on SI
library(splines) 
b_df = 5 #degrees of freedom
mult_reg2 = multinom(change_frac ~ bs(SI, b_df), data = si_change)
summary(mult_reg2)
pred_mult2 = predict(mult_reg2, newdata = new_data, "probs")
plot(0:27, pred_mult2[,3], pch = 1, col = "red", ylim = c(0, 1), 
     xlab = "SI score", ylab = "Percentage", 
     main = "Predicted change at diffrent SI scores", 
     sub = paste("Multinomial Regression with B-splines (df = ", b_df, ")", sep = ""))
points(0:27, pred_mult2[,2], pch = 2, col = "blue")
points(0:27, pred_mult2[,1], pch = 3, col = "black")
legend(3, 1, legend=c("Increase", "Decrease", "Hold"),
       col=c("red", "blue", "black"), pch=1:3, cex=0.8)

    ## ordinal regression
library(MASS) # for "polr"
si_change_ord = ema_ctm %>%
  transmute(ID = ID, SI = SI, change = change, 
            change_frac = 0-1*(change<0)+1*(change>0))

ord_reg = polr(as.factor(change_frac) ~ SI, data = si_change_ord, Hess=TRUE)
summary(ord_reg)
pred_ord = predict(ord_reg, newdata = new_data, "probs")
plot(0:27, pred_ord[,3], pch = 1, col = "red", ylim = c(0, 1), 
     xlab = "SI score", ylab = "Percentage", 
     main = "Predicted change at diffrent SI scores", 
     sub = "Ordinal Regression with linear SI")
points(0:27, pred_ord[,2], pch = 2, col = "blue")
points(0:27, pred_ord[,1], pch = 3, col = "black")
legend(3, 1, legend=c("Increase", "Decrease", "Hold"),
       col=c("red", "blue", "black"), pch=1:3, cex=0.8)

    ## ordinal regression & Bspline on SI   
ord_reg2 = polr(as.factor(change_frac) ~ bs(SI, b_df), data = si_change_ord, Hess=TRUE)
summary(ord_reg2)
pred_ord2 = predict(ord_reg2, newdata = new_data, "probs")
plot(0:27, pred_ord2[,3], pch = 1, col = "red", ylim = c(0, 1), 
     xlab = "SI score", ylab = "Percentage", 
     main = "Predicted change at diffrent SI scores", 
     sub = paste("ordinial Regression with B-splines (df = ", b_df, ")", sep = ""))
points(0:27, pred_ord2[,2], pch = 2, col = "blue")
points(0:27, pred_ord2[,1], pch = 3, col = "black")
legend(3, 1, legend=c("Increase", "Decrease", "Hold"),
       col=c("red", "blue", "black"), pch=1:3, cex=0.8)




#### ---------------------- Use Birth and Death Process ---------------------- ####
library(Matrix)
library(data.table)

holdTime = 20

mean(ema_ctm$trans_time)
  # Parameters Initialization
transLess = rep(0.5, 26)
holdPara = rep(holdTime, 28)#rep(1, 28)
  # Transition matrix Q
q = c(-holdPara * rep(1,28), holdPara[2:27] * transLess)
Q = matrix(0, nrow = 28, ncol = 28)
diag(Q) = q[1:28]
diag(Q[-1, -28]) = c(q[29:54], -q[28]) # lower part
diag(Q[-28, -1]) = c(-q[1], -q[2:27]-q[29:54]) # upper part
  # negative log-likelihood of the data
sub_ind = sort(sample(1:nrow(ema_ctm), 500))
ema_ctm_sub = ema_ctm[sub_ind,]

nl_v = apply(ema_ctm, 1, function(x) log(expm((x[4])*Q)[x[2]+1, x[5]+1]))
data_delete = which(is.na(nl_v))
ema_ctm_prac = ema_ctm
if(is.na(sum(nl_v))){ ema_ctm_prac = ema_ctm[-data_delete,] }
nl = -sum(apply(ema_ctm_prac, 1, function(x) log(expm((x[4])*Q)[x[2]+1, x[5]+1])))

  # Optimization
f_emaCTM = function(q){
  Q = matrix(0, nrow = 28, ncol = 28)
  diag(Q) = q[1:28]
  diag(Q[-1, -28]) = c(q[29:54], -q[28]) # lower part
  diag(Q[-28, -1]) = c(-q[1], -q[2:27]-q[29:54]) # upper part
  
  nl = -sum(apply(ema_ctm_prac, 1, function(x) log(expm((x[4])*Q)[x[2]+1, x[5]+1])))
  return(nl)
}

optim_emaCTM = optim(q, f_emaCTM)
q_opt = optim_emaCTM$par
round(q_opt, 2)

Q_opt = matrix(0, nrow = 28, ncol = 28)
diag(Q_opt) = q_opt[1:28]
diag(Q_opt[-1, -28]) = c(q_opt[29:54], -q[28]) # lower part
diag(Q_opt[-28, -1]) = c(-q_opt[1], -q_opt[2:27]-q_opt[29:54]) # upper part
Q_opt = round(Q_opt,2)

hold_opt = q_opt[1:28]
plot(y = -hold_opt, x = 0:27, xlab = "SI score", ylab = "Holding time expectation", xlim = c(0,27), 
     main = "Holding time expectation (negative diagonal of transition matrix)", 
     sub = paste("(q_{xx} = 1/", holdTime, ")", sep = ""))


transLess_opt = q_opt[29:54] / (-q_opt[2:27])
transLess_opt
plot(y = c(0,transLess_opt,1), x = 0:27, xlab = "SI score", ylab = "Probability", xlim = c(0,27), 
     main = "Probability of transiting to smaller SI score when transition happens", 
     sub = paste("(q_{xx} = 1/", holdTime, ")", sep = ""))
abline(h = 0.5, lty = 2)
text(0, 0.45, labels = "0.5")



#### ---------------------- Use Birth and Death Process (less parameter)---------------------- ####
holdTime = 12

mean(ema_ctm$trans_time)
cutpoint = c(4, 17, 28)
repeat_para = c(cutpoint[1], diff(cutpoint))
# Parameters Initialization
transLess3 = rep(0.5, 3)
holdPara3 = rep(holdTime, 3)#rep(1, 28)
# Transition matrix Q
q3 = c(-holdPara3 * rep(1,3), holdPara3 * transLess3)
q3_long = c(rep(q3[1:3], repeat_para), rep(q3[4:6], (repeat_para-c(1,0,1))))
Q3 = matrix(0, nrow = 28, ncol = 28)
diag(Q3) = q3_long[1:28]
diag(Q3[-1, -28]) = c(q3_long[29:54], -q3_long[28]) # lower part
diag(Q3[-28, -1]) = c(-q3_long[1], -q3_long[2:27]-q3_long[29:54]) # upper part
# negative log-likelihood of the data
nl_v = apply(ema_ctm, 1, function(x) log(expm((x[4])*Q3)[x[2]+1, x[5]+1]))
data_delete = which(is.na(nl_v))
if(is.na(sum(nl_v))){ ema_ctm_prac = ema_ctm[-data_delete,] }
nl = -sum(apply(ema_ctm_prac, 1, function(x) log(expm((x[4])*Q)[x[2]+1, x[5]+1])))


# Optimization
f_emaCTM_3 = function(q3){
  # q3 is a vector with length 6
  Q3 = matrix(0, nrow = 28, ncol = 28)
  q3_long = c(rep(q3[1:3], repeat_para), rep(q3[4:6], (repeat_para-c(1,0,1))))
  diag(Q3) = q3_long[1:28]
  diag(Q3[-1, -28]) = c(q3_long[29:54], -q3_long[28]) # lower part
  diag(Q3[-28, -1]) = c(-q3_long[1], -q3_long[2:27]-q3_long[29:54]) # upper part
  
  nl = -sum(apply(ema_ctm_prac, 1, function(x) log(expm((x[4])*Q3)[x[2]+1, x[5]+1])))
  return(nl)
}

optim_emaCTM_3 = optim(q3, f_emaCTM_3)
q3_opt = optim_emaCTM_3$par
round(q3_opt, 2)

hold3_opt = q3_opt[1:3]
hold3_opt
plot(y = -rep(hold3_opt, repeat_para), x = 0:27, 
     xlab = "SI score", ylab = "Expectation", xlim = c(0,27), 
     main = "Holding time expectation", 
     sub = paste("(q_{xx} = 1/", holdTime, ")", sep = ""))


transLess3_opt = q3_opt[4:6] / (-q3_opt[1:3])
transLess3_opt
plot(y = c(0, rep(transLess3_opt, repeat_para - c(1,0,1)), 1), x = 0:27, 
     xlab = "SI score", ylab = "Probability", xlim = c(0,27), 
     main = "Probability of transiting to smaller SI score when transition happens", 
     sub = paste("(q_{xx} = 1/", holdTime, ")", sep = ""))
abline(h = 0.5, lty = 2)
text(0, 0.45, labels = "0.5")



#### ---------------------- Change of EMA in SI---------------------- ####
ema_ctm_change = ema_ctm[1,]
for (id in unique(ema_ctm$ID)) {
  sub = filter(ema_ctm, ID == id)
  n_sub = dim(sub)[1]
  delete = 0
  if(n_sub>1){
    for (i in 1:(n_sub-1)) {
      if(sub$change[i] == 0){
        delete = c(delete, i)
        sub$trans_time[i+1] = sub$trans_time[i+1] + sub$trans_time[i]
      }
    }
    if(length(delete)>1){sub = sub[-delete[-1], ]}
  }
  ema_ctm_change = rbind(ema_ctm_change, sub)
}
ema_ctm_change = ema_ctm_change[-1, ] %>%
  filter(change != 0)  #Delete the observations without observed transit

dim(ema_ctm_change)[1]
length(which(ema_ctm_change$change > 0))
length(which(ema_ctm_change$change < 0))

mean(ema_ctm_change$change)
mean(ema_ctm_change$change[which(ema_ctm_change$change > 0)])
mean(ema_ctm_change$change[which(ema_ctm_change$change < 0)])

mean(ema_ctm_change$trans_time)


    #### ANOVA: change ~ ID + study_day ####
library(car)

aov_change_IDday = aov(change ~ ID + study_day, data = ema_ctm_change)
summary(aov_change_IDday)




