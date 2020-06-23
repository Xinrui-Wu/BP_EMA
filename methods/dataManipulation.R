## BP_EMA data manipulation function
## Script by : Xinrui Wu

# Main function: BPEMA
# Assist function: press_count


press_count = function(bp, ema, id, k){
  # Input:
  #   bp, ema: the cleaned Button Press data and EMA data 
  #   id: an integer representing the observation to be explored
  #   k: an integer representing the time window
  # Output:
  #   pre_press: a vector for numbers of presses in the pre-survey window for every survey this observation took.
  #   post_press: a vector for numbers of presses in the post-survey window for every survey this observation took.
  
  sub_ema = filter(ema, ID == id)
  n_ema = dim(sub_ema)[1]
  sub_bp = filter(bp, ID == id) %>%
    mutate(time_int = rep(k,n()), index = rep(-1,n()), mark = rep(-1,n())) 
  # time_int: time interval between this press and the nearest survey
  # index: which row in sub_ema it belongs to
  # mark: 0: before, 1: after ; 
  n_bp = dim(sub_bp)[1]
  
  # Initializing output
  pre_press = rep(0, n_ema)
  post_press = rep(0, n_ema)
  
  for (i in 1:n_ema){
    time = sub_ema$timestamp[i] # the timestamp of survey i
    int_low = time - k*3600
    int_up = time + k*3600
    
    for (j in 1:n_bp){
      temp = abs(as.numeric(sub_bp[j,'timestamp'] - sub_ema[i,'timestamp'])) / 60 
      # temp: time interval (by hour) between press j and survey i
      
      if(sub_bp[j, "timestamp"] > int_low & 
         sub_bp[j, "timestamp"] < time &
         temp < sub_bp[j,'time_int']){
        sub_bp[j,'time_int'] = temp
        sub_bp[j,'index'] = i
        sub_bp[j, 'mark'] = 0
      }
      else if(sub_bp[j, "timestamp"] < int_up & 
              sub_bp[j, "timestamp"] > time & 
              temp < sub_bp[j,'time_int']){
        sub_bp[j,'time_int'] = temp
        sub_bp[j,'index'] = i
        sub_bp[j, 'mark'] = 1
      }
    }
  }
  
  if (0 %in% sub_bp$mark) {
    pre = filter(sub_bp, mark == 0) %>%
      group_by(index) %>%
      summarise(count = n()) %>%
      ungroup()
    pre_press[pre$index] = pre$count
  }
  
  if (1 %in% sub_bp$mark) {
    post = filter(sub_bp, mark == 1) %>%
      group_by(index) %>%
      summarise(count = n()) %>%
      ungroup()
    post_press[post$index] = post$count
  }
  
  result = cbind(pre_press, post_press)
  return(result)
}


BPEMA = function(BP, EMA, time.win, returnPlot = 1){
  # Input:
  #   BP: Button Press data, including variables "ID, timestamp, time_day, run.time"
  #   EMA: EMA data, including variables "ID, timestamp, date, time_day, run.time, sitb_si_sum"
  #   time.win: a vector of time windows for finding presses around a survey
  #   returnPlot: a logic variable for showing the visualization plot or not. 
  #               Defult is 1(show the plot)
  # Output:
  #   bp_ema: dataframe including variables "ID, timestamp, date, time_day, si_score" 
  #           and variables "pre_press, post_press" for different time windows.
  # Remarks:           
  #   The output omits rows with missing values in its variables, 
  #   and only records observations involved in both BP data and EMA data. 
  
  require(tidyverse)
  

  ema = EMA %>% 
    transmute(ID = ID, timestamp = timestamp, date = date, time_day = time_day, si_score = sitb_si_sum, run.time = run.time) %>%
    drop_na() %>%
    filter(ID %in% BP$ID)
  
  bp = BP %>%
    transmute(ID = ID, timestamp = as.POSIXct(timestamp, tz = attr(ema$timestamp[1], 'tzone')), 
              date = as.Date(timestamp, tz = attr(ema$timestamp[1], 'tzone')), 
              time_day = time_day, run.time = run.time)
  
  n_win = length(time.win)
  n_ema = dim(ema)[1]
  
  # Initializing the variables for counting presses
  count = data.frame(ema$ID, matrix(0, nrow = n_ema, ncol = 2*n_win))
  colnames(count) = c('ID', paste(c('pre_press_', 'post_press_'), rep(time.win, each = 2), sep = ""))
  
  for (id in unique(ema$ID)){
    for (k in 1:n_win){
      count[which(ema$ID == id), c(2*k, 2*k+1)] = press_count(bp, ema, id, time.win[k])
    }
  }
  
  if(returnPlot == 1){
    df_plot = count[,2:3]
    df_plot = as.matrix(filter(df_plot, rowSums(df_plot) != 0))
    dim = dim(df_plot)[1]
    
    if(n_win > 1){
      for (k in 2:n_win){
        temp = count[, c(2*k, 2*k+1)]
        temp = as.matrix(filter(temp, rowSums(temp) != 0))
        df_plot = rbind(df_plot, temp)
        dim = c(dim, dim(temp)[1])
      }
    }
    
    df_plot = data.frame(Window = as.factor(rep(time.win, times = 2*dim)), 
                         Time = rep(rep(c("pre", "post"), n_win), times = rep(dim, each = 2)), 
                         Count = as.numeric(df_plot))
    require(ggplot2)
    plot = ggplot(df_plot, aes(x=Window, y=Count, fill=Time)) + 
      geom_boxplot() +
      labs(title = "Count of presses for observations with non-zero press in the window")
    print(plot)
  }
  
  bp_ema = as.data.frame(cbind(ema, count[,-1]))
  return(bp_ema)
}

plot_BPEMA = function(bp_ema, time.win){
  # Input:
  #   bp_ema: the Output from BPEMA function
  #   time.win: a vector of time windows for finding presses around a survey
  #             (the same with the input of BPEMA)
  
  # Output:
  #   Visualization plot for bp_ema
  
  count = bp_ema[, -(1:6)]
  n_win = dim(count)[2]/2
  
  df_plot = count[,1:2]
  df_plot = as.matrix(filter(df_plot, rowSums(df_plot) != 0))
  dim = dim(df_plot)[1]
  
  if(n_win > 1){
    for (k in 2:n_win){
      temp = count[, c(2*k-1, 2*k)]
      temp = as.matrix(filter(temp, rowSums(temp) != 0))
      df_plot = rbind(df_plot, temp)
      dim = c(dim, dim(temp)[1])
    }
  }
  
  df_plot = data.frame(Window = as.factor(rep(time.win, times = 2*dim)), 
                       Time = rep(rep(c("pre", "post"), n_win), times = rep(dim, each = 2)), 
                       Count = as.numeric(df_plot))
  require(ggplot2)
  print(ggplot(df_plot, aes(x=Window, y=Count, fill=Time)) + 
    geom_boxplot() +
    labs(title = "Count of presses for observations with non-zero press in the window"))
}


