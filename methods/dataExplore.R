## Data exploration for BP-EMA data
## script by : Xinrui Wu
## Date: Jul 28, 2020

#### ---------- Plot : SI distribution for every observation ---------- ####
library(gridExtra)
library(ggplot2)
library(tidyverse)


df = transmute(bp_ema, ID = as.character(ID), timestamp, si_score, press_3 = pre_press_3+ post_press_3) %>% 
  mutate(timeIndex = rep(0, length(ID)))
for (id in df$ID){
  df$timeIndex[which(df$ID == id)] = 1:length(df$ID[which(df$ID == id)])
}

p = list()
for(i in 1:9){
  if (i %in% 1:6) {id = unique(df$ID)[(6*i-5):(6*i)]}
  else {id = unique(df$ID)[(5*i+2):(5*i+6)]}
  df_sub = filter(df, ID %in% id)
  p[[i]] = ggplot(data=df_sub, mapping = aes(x = timestamp, y = si_score)) + 
    geom_line(aes(color = ID), linetype = 1) + 
    xlab("Time") + 
    ylab("SI score")
}
do.call(grid.arrange,p)


#### ---------- ANOVA ---------- ####

bp_ema_aov = bp_ema %>%
  mutate(day = rep(0, length(ID)), 
         press_1 = pre_press_1 + post_press_1, 
         press_3 = pre_press_3 + post_press_3, 
         press_5 = pre_press_5 + post_press_5)

##  day: How many days has each person been in the experiment   
for (id in bp_ema_aov$ID){
  day = as.factor(bp_ema_aov$date[which(bp_ema_aov$ID == id)])
  levels(day) = 1:length(unique(day))
  bp_ema_aov$day[which(bp_ema_aov$ID == id)] = day
}

hist(bp_ema_aov$day,breaks = 1:max(bp_ema_aov$day), xlab = "Days of Participation", main = "")
max(bp_ema_aov$day)

## ANOVA 1: si_score ~ ID + day
library(car)
aov_si_IDday = aov(si_score ~ ID + day, data = bp_ema_aov)
summary(aov_si_IDday)
Anova(aov_si_IDday, type = "III")

## ANOVA 2: press ~ ID + si_score
aov_press_IDsi = aov(press_3 ~ ID + si_score, data = bp_ema_aov)
summary(aov_press_IDsi)
Anova(aov_press_IDsi, type = "III")

aov_prePress_IDsi = aov(pre_press_3 ~ ID + si_score, data = bp_ema_aov)
summary(aov_prePress_IDsi)
Anova(aov_prePress_IDsi, type = "III")

aov_postPress_IDsi = aov(post_press_3 ~ ID + si_score, data = bp_ema_aov)
summary(aov_postPress_IDsi)
Anova(aov_postPress_IDsi, type = "III")


## ANOVA 3: press ~ ID + si_score + day
aov_press_IDsiDay = aov(press_3 ~ ID + si_score + day, data = bp_ema_aov)
summary(aov_press_IDsiDay)
Anova(aov_press_IDsiDay, type = "III")

aov_prePress_IDsiDay = aov(pre_press_3 ~ ID + si_score+day, data = bp_ema_aov)
summary(aov_prePress_IDsiDay)
Anova(aov_prePress_IDsiDay, type = "III")

aov_postPress_IDsiDay = aov(post_press_3 ~ ID + si_score+ day, data = bp_ema_aov)
summary(aov_postPress_IDsiDay)
Anova(aov_postPress_IDsiDay, type = "III")


#### ---------- Summary for Observations ---------- ####
obs_summary = transmute(bp_ema_aov, ID = ID, day = day, SI = si_score, press = press_3) %>%
  group_by(ID) %>%
  summarize(num = n(), 
            si_mean = mean(SI), 
            si_max = max(SI), 
            si_min = min(SI), 
            si_range = si_max - si_min, 
            si_sd = sd(SI), 
            mean_press = mean(press), 
            max_press = max(press), 
            min_press = min(press), 
            sd_press = sd(press))
save(obs_summary,file="obs_summary.Rda")







