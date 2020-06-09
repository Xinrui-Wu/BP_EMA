## Mobile data  Week_3 
## script by : Xinrui Wu

## purpose
# <1> (Up to how many tests does an observation take per day?) -- 3-hour interval or 2-hour interval or less?
# <2> Considering presses near each test for each person (group by observation and test)
# <3> Seperating observations by si score

## library
library(lubridate)
library(tidyverse)
library(ggplot2)

## data
blake = readRDS('./Blake_EMA.RDS')
bh = readRDS('./BH_SI.RDS')

## ---------------------------------------------------------------------------------------
## ----------------------------------------- <1> -----------------------------------------
## ---------------------------------------------------------------------------------------

blake_count = blake %>%
  group_by(ID, date) %>%
  arrange(time_day) %>%
  mutate(count = n(), min_int = min(diff(time_day)), max_int = max(diff(time_day))) %>%
  summarize(count = count[1], min_int = min_int[1], 
            min2_int = min(setdiff(diff(time_day), min_int)), max_int = max_int[1])

blake_count[which(blake_count$max_int == 0), c("count", "min_int", "min2_int", "max_int")] = 
  rep(c(1, Inf, Inf, -Inf), each = length(which(blake_count$max_int == 0)))
blake_count[which(blake_count$min_int == 0), "min_int"] = blake_count[which(blake_count$min_int == 0), "min2_int"]
blake_count = blake_count[, c("ID", "date", "count", "min_int", "max_int")]

table(blake_count$count)
table(blake_count$min_int)
table(blake_count$max_int)

blake_count[which(blake_count$min_int == 1), c("ID", "date")]



## ---------------------------------------------------------------------------------------
## ----------------------------------------- <2> -----------------------------------------
## ---------------------------------------------------------------------------------------

k = 3 # (hour) time inerval for each side

ema = blake %>%
  select(ID = ID, timestamp = timestamp, date = date, time_day = time_day, si_score = sitb_si_sum, run.time = run.time)  %>%
  mutate(int_low = timestamp - k*3600, int_up = timestamp + k*3600, count_pre = 0, count_after = 0, press_day = 0)

press = bh %>%
  mutate(date = as.Date(timestamp, tz = 'UTC')) %>% 
  group_by(ID, date) %>% 
  mutate(press_day = n())

press_count = function(id, day){
  # Output:
  #   conut_pre
  #   count_after
  #   press_day
  sub_ema = as.data.frame(filter(ema, ID == id, date == day))
  n_ema = dim(sub_ema)[1]
  
  if (id %in% press$ID){
    sub_press = filter(press, ID == id)
    if(day %in% sub_press$date){
      sub_press = filter(sub_press, date == day) %>%
        mutate(time_int = rep(k,n()), index = rep(-1,n()), mark = rep(-1,n())) 
        # mark: 0: before, 1: after ; index: which row in sub_ema it belongs to
      sub_press = as.data.frame(sub_press)
      n_press = dim(sub_press)[1]
      sub_ema$press_day = sub_press$press_day[1]
      
      for (i in 1:n_ema){
        for (j in 1:n_press){
          temp = abs(as.numeric(sub_press[j,'timestamp'] - sub_ema[i,'timestamp'])) / 60
          if(sub_press[j, "timestamp"] > sub_ema[i, "int_low"] & 
             sub_press[j, "timestamp"] < sub_ema[i, "timestamp"] &
             temp < sub_press[j,'time_int']){
            sub_press[j,'time_int'] = temp
            sub_press[j,'index'] = i
            sub_press[j, 'mark'] = 0
          }
          else if(sub_press[j, "timestamp"] < sub_ema[i, "int_up"] & 
                  sub_press[j, "timestamp"] > sub_ema[i, "timestamp"] & 
                  temp < sub_press[j,'time_int']){
            sub_press[j,'time_int'] = temp
            sub_press[j,'index'] = i
            sub_press[j, 'mark'] = 1
          }
        }
      }
      
      if (0 %in% sub_press$mark) {
        pre = filter(sub_press, mark == 0) %>%
          group_by(index) %>%
          summarise(count = n()) %>%
          arrange() %>%
          ungroup()
        sub_ema[pre$index,'count_pre'] = sub_ema[pre$index,'count_pre'] + pre$count
      }
      
      if (1 %in% sub_press$mark) {
        after = filter(sub_press, mark == 1) %>%
          group_by(index) %>%
          summarise(count = n()) %>%
          arrange() %>%
          ungroup()
        sub_ema[after$index,'count_after'] = sub_ema[after$index,'count_after'] + after$count
      }
    }
  }
  
  result = cbind(count_pre = sub_ema$count_pre, 
                 count_after = sub_ema$count_after, 
                 press_count = sub_ema$press_day)
  return(result)
}


ema_with_press = ema %>%
  filter(ID %in% unique(press$ID))

for (id in unique(ema_with_press$ID)){
  for(day in unique(ema_with_press[which(ema_with_press$ID == id), 'date'])){
    ema_with_press[which(ema_with_press$ID == id & ema_with_press$date == day), 
                   c('count_pre', 'count_after', 'press_day')] = press_count(id, day)
  }
}

ema_with_press = mutate(ema_with_press, count_sum = count_pre + count_after) %>%
  select(-int_low, -int_up)

ema_obs_date = ema_with_press %>%
  group_by(ID, date) %>%
  summarise(sum_pre = sum(count_pre), sum_after = sum(count_after), 
            sum_press = sum(count_sum), press_day = press_day[1]) %>%
  ungroup() %>%
  mutate(percentage = round(sum_press / press_day, 2))


#   plot
df = ema_with_press %>%
  filter(count_pre != 0 | count_after != 0) %>%
  transmute(ID = ID, pre_p = count_pre/(count_pre+count_after), 
            after_p = count_after/(count_pre+count_after)) %>%
  gather(key = time, value = percent_of_press, c('pre_p','after_p'))


ggplot(df, aes(x=rep(1:(length(df$ID)/2),2), y = percent_of_press, group = time, colour = time)) + 
  geom_point(size = 0.5) +
  labs(title = "Press and test",
       x = "Observations",
       y = "Percent of Press")

ggplot(df, aes(x=rep(1:(length(df$ID)/2),2), y=percent_of_press, fill=time, colour = time)) +
  geom_bar(stat="identity", position=position_dodge())+ 
  geom_text(aes(label=ID), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) + 
  labs(title = "Press and test",
       x = "Observations",
       y = "Percent of Press")


#  Result:
#    1. each row: per obs. & per day
c(more_after = length(which(ema_obs_date$sum_pre < ema_obs_date$sum_after)), 
  more_pre = length(which(ema_obs_date$sum_pre > ema_obs_date$sum_after)), 
  equal = length(which(ema_obs_date$sum_pre == ema_obs_date$sum_after)))

#    2. each row: per obs. & per test
c(more_after = length(which(ema_with_press$count_pre < ema_with_press$count_after)), 
  more_pre = length(which(ema_with_press$count_pre > ema_with_press$count_after)), 
  equal = length(which(ema_with_press$count_pre == ema_with_press$count_after)), 
  zero = length(which(ema_with_press$count_sum == 0)))


## Test
df_t = ema_with_press %>%
  filter(count_pre != 0 | count_after != 0)
#     mean & sd
gather(data = df_t, key = time, value = press, c('count_pre','count_after')) %>%
  group_by(time) %>%
  summarise(
    count = n(),
    mean = mean(press, na.rm = TRUE),
    sd = sd(press, na.rm = TRUE)
  )


pair_t = t.test(ema_with_press$count_pre , ema_with_press$count_after, paired = TRUE)
pair_t$p.value

two_samp_t = t.test(ema_with_press$count_pre , ema_with_press$count_after)
two_samp_t$p.value



## ---------------------------------------------------------------------------------------
## ----------------------------------------- <3> -----------------------------------------
## ---------------------------------------------------------------------------------------
ema_with_press = filter(ema_with_press, !is.na(si_score))

hist(ema_with_press$si_score, breaks = c(0, 1:26-0.1, 27), 
     main = 'Distribution of SI Score', xlab = 'Score')
abline(v = 4, col = "red")
abline(v = 17, col = "red")
text(x = 4.5, y = 0.1, "4", col = "red")
text(x = 17.6, y = 0.1, "17", col = "red")
table(ema_with_press$si_score)

## <3.1> 0-1 ; 2-3 ; 4-16 ; 17-27
#si_score_interval = rbind(c(0, 1), 
#                          c(2, 3), 
#                          c(4, 16), 
#                          c(17, 27))

#si_score_interval = rbind(c(0, 3), 
#                          c(4, 9),
#                          c(10, 18), 
#                          c(19, 27))

si_score_interval = rbind(c(0, 3), 
                          c(4, 16),
                          c(17, 27))


colnames(si_score_interval) = c('min_score', 'max_score')
n_level = dim(si_score_interval)[1]
results_per_score = as.data.frame(matrix(0,  nrow = n_level, ncol = 5))
colnames(results_per_score) = c('n', 'mean_pre', 'mean_after', 'paired_t', '2_sample_t')

#split.screen(c(2, ceiling((n_level/2))))
for (i in 1:n_level){
  sub_ema = ema_with_press[which(ema_with_press$si_score %in% 
                                   si_score_interval[i,1]:si_score_interval[i,2]),] %>%
    filter(count_pre != 0 | count_after != 0) 
  results_per_score[i, 'n'] = dim(sub_ema)[1]
  
  sub_df = sub_ema %>%
    transmute(ID = ID, pre_p = count_pre/(count_pre+count_after), 
              after_p = count_after/(count_pre+count_after)) %>%
    gather(key = time, value = percent_of_press, c('pre_p','after_p'))
  #screen(i)
  ggplot(sub_df, aes(x=rep(1:(length(sub_df$ID)/2),2), y = percent_of_press, group = time, colour = time)) + 
    geom_point(size = 0.5) +
    labs(title = "Press and test",
         x = "Observations",
         y = "Percent of Press")
  
  results_per_score[i, 'mean_pre'] = mean(sub_ema$count_pre)
  results_per_score[i, 'mean_after'] = mean(sub_ema$count_after)
  
  pair_t = t.test(sub_ema$count_pre , sub_ema$count_after, paired = TRUE)
  results_per_score[i, 'paired_t'] = pair_t$p.value
  
  two_samp_t = t.test(sub_ema$count_pre , sub_ema$count_after)
  results_per_score[i, '2_sample_t'] = two_samp_t$p.value
}
#close.screen(all.screens = TRUE)

results_per_score = cbind(si_score_interval, results_per_score)
results_per_score



