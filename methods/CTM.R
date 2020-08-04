## Continuous Time Markov Model for BP-EMA data
## script by : Xinrui Wu


#### ------ Manipulate data for estimation ------ ####
library(tidyverse)
source("./dataManipulation.R")

ema_ctm = data_rm(transmute(blake, ID, timestamp, SI = sitb_si_sum)) %>%
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


#### ---------------------- Use Birth and Death Process ---------------------- ####
library(Matrix)
library(data.table)
  # Parameters
transLess = rep(0.5, 26)
holdPara = rep(1, 28)
  # Transition matrix Q
q = c(-holdPara * rep(1,28), holdPara[2:28] * transLess)
Q = matrix(0, nrow = 28, ncol = 28)
diag(Q) = q[1:28]
diag(Q[-1, -28]) = c(q[29:54], -q[28]) # lower part
diag(Q[-28, -1]) = c(-q[1], -q[2:27]-q[29:54]) # upper part
  # negative log-likelihood of the data
nl = -sum(apply(ema_ctm, 1, function(x) log(expm((x[3])*Q)[x[2]+1, x[4]+1])))


  # Optimization
f_emaCTM = function(q){
  Q = matrix(0, nrow = 28, ncol = 28)
  diag(Q) = q[1:28]
  diag(Q[-1, -28]) = c(q[29:54], -q[28]) # lower part
  diag(Q[-28, -1]) = c(-q[1], -q[2:27]-q[29:54]) # upper part
  
  nl = -sum(apply(ema_ctm, 1, function(x) log(expm((x[3])*Q)[x[2]+1, x[4]+1])))
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
main = "Holding time expectation (negative diagonal of transition matrix)")

transLess_opt = q_opt[29:54] / (-q_opt[2:27])
transLess_opt
plot(y = c(0,transLess_opt,1), x = 0:27, xlab = "SI score", ylab = "Probability", xlim = c(0,27), 
     main = "Probability of transiting to smaller SI score when transition happens")
abline(h = 0.5, lty = 2)
text(0, 0.45, labels = "0.5")
