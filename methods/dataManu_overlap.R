## BP_EMA data manipulation function (overlap)
## Script by : Xinrui Wu

# Main function: BPEMA_overlap
# Assist function: press_count_overlap
# Draw boxplot for the output data from BPEMA: plotBPEMA_overlap


press_count_overlap = function(bp, ema, id, k){
  # Input:
  #   bp, ema: the cleaned Button Press data and EMA data 
  #   id: an integer representing the observation to be explored
  #   k: an integer representing the time window
  # Output:
  #   pre_press: a vector for numbers of presses in the pre-survey window for every survey this observation took.
  #   post_press: a vector for numbers of presses in the post-survey window for every survey this observation took.
  
  sub_ema = filter(ema, ID == id)
  n_ema = dim(sub_ema)[1]
  sub_bp = filter(bp, ID == id)
  n_bp = dim(sub_bp)[1]
  
  # Initializing output
  pre_press = rep(0, n_ema)
  post_press = rep(0, n_ema)
  
  for (i in 1:n_ema){
    time = sub_ema$timestamp[i] # the timestamp of survey i
    int_low = time - k*3600
    int_up = time + k*3600
    
    for (j in 1:n_bp){
      
      if(sub_bp[j, "timestamp"] > int_low & 
         sub_bp[j, "timestamp"] < time){
        pre_press[i] = pre_press[i] + 1
      }
      else if(sub_bp[j, "timestamp"] < int_up & 
              sub_bp[j, "timestamp"] > time){
        post_press[i] = post_press[i] + 1
      }
    }
  }
  
  result = cbind(pre_press, post_press)
  return(result)
}


BPEMA_overlap = function(BP, EMA, time.win, returnPlot = 1){
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
      count[which(ema$ID == id), c(2*k, 2*k+1)] = press_count_overlap(bp, ema, id, time.win[k])
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
      labs(title = "Count of presses for observations with non-zero press in the window (overlap)")
    print(plot)
  }
  
  bp_ema = as.data.frame(cbind(ema, count[,-1]))
  return(bp_ema)
}

plot_BPEMA_overlap = function(bp_ema_overlap, time.win){
  # Input:
  #   bp_ema_overlap: the Output from BPEMA_overlap function
  #   time.win: a vector of time windows for finding presses around a survey
  #             (the same with the input of BPEMA)
  
  # Output:
  #   Visualization plot for bp_ema_overlap
  
  count = bp_ema_overlap[, -(1:6)]
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
          labs(title = "Count of presses for observations with non-zero press in the window (overlap)"))
}




