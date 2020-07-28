## Continuous Time Markov Model for BP-EMA data
## script by : Xinrui Wu
## Date: Jul 27, 2020

## ------ Manipulate data for estimation ------ ##
library(tidyverse)

 # Function for removing missing or duplicate daa
data_rm = function(data, dup_time = 5){
  # Input:
  #   data: the uncleaned EMA or Button Press data (only required variables 
  #         and keep the original variable names for ID and timestamp).
  #   dup_time: time window for duplicate observations
  # Output:
  #   result: the cleaned data, NA observations and duplicate observations removed. 
  #           (view observations for the same person within dup_time minuets 
  #           as duplicate observations.)
  
  require(tidyverse)
  
  result = drop_na(data) %>%
    group_by(ID) %>% 
    arrange(timestamp, .by_group = T) %>%
    ungroup()
  diff_time = 0 ^ diff(result$ID) * diff(result$timestamp) * 60 + 
    10e4 * diff(result$ID)
  result = result[-(which(diff_time < dup_time) + 1), ]
  return(result)
}

  
#### ---------------------- Data for CTMC ---------------------- ####

     ## Version 1: include stationary situations
ema_ctm = data_rm(transmute(blake, ID, timestamp, SI = sitb_si_sum)) %>%
  group_by(ID) %>% 
  mutate(trans_time = c(diff(timestamp), 0) / 3600) %>% # (hour)
  mutate(SI_next = c(SI[-1], 0)) %>%
  filter(trans_time != 0) %>%
  ungroup() %>%
  select(-timestamp) %>% 
  mutate(change = SI_next - SI)


     ## Version 2: omit stationary situations
EMA_CTM = function(ema){
  # Input:
  #   ema: the cleaned EMA data
  # Output:
  #   ema_ctm: the transition data for continuous time Markov prediction
  ema_ctm = ema %>%
    group_by(ID) %>% 
    mutate(trans_time = c(diff(timestamp), 0) / 3600) %>% # (hour)
    mutate(SI_next = c(SI[-1], 0)) %>%
    filter(trans_time != 0) %>%
    ungroup() %>%
    select(-timestamp) %>% 
    mutate(change = SI_next - SI)
  
  result = ema_ctm[1,]
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
    result = rbind(result, sub)
  }
  ema_ctm = result[-1, ] %>%
    filter(change != 0)  #Delete the observations without observed transit
  return(ema_ctm)
}

ema_ctm = EMA_CTM(data_rm(transmute(blake, ID, timestamp, SI = sitb_si_sum)))

#### ---------------------- Some summary for the ema_ctm ---------------------- ####
table(abs(ema_ctm$change))
ema_ID = ema_ctm %>%
  group_by(ID) %>% 
  summarize(SI_max = max(c(SI, SI_next)), SI_min = min(c(SI, SI_next)), 
         change_abs_max = max(abs(change)), change_abs_min = min(abs(change)))


ctmc_sum_trans_time = function(data, n_state){
  # Input:
  #   data: 
  #   n_state: number of states
  # Output:
  #   time: n_state * n_state matrix for the sum of transition time
  #   freq: n_state * n_state matrix for the number of observed transition
  
  trans_time_sum = matrix(0, n_state, n_state)
  trans_freq = matrix(0, n_state, n_state)
  for (i in 1:dim(data)[1]){
    trans_time_sum[data$SI[i]+1, data$SI_next[i]+1] = 
      trans_time_sum[data$SI[i]+1, data$SI_next[i]+1] + data$trans_time[i]
    trans_freq[data$SI[i]+1, data$SI_next[i]+1] = 
      trans_freq[data$SI[i]+1, data$SI_next[i]+1] + 1
  }
  result = list(time = trans_time_sum, freq = trans_freq)
  return(result)
}

trans_freq = ctmc_sum_trans_time(ema_ctm, 28)$freq
trans_time_sum = ctmc_sum_trans_time(ema_ctm, 28)$time
