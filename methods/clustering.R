## Clustering for EMA data
## script by : Xinrui Wu
## Update Date: Sep 15, 2020

source("./dataManipulation.R")
blake = readRDS('../rawdata/Blake_EMA.RDS')
bh = readRDS('../rawdata/BH_SI.RDS')


#### ---------------------- kmeans with statistics ---------------------- ####
library(tidyverse)
ema_km_stat_data = transmute(blake, ID, timestamp = timestamp, NO. = study_obsv_num, SI = sitb_si_sum) %>%
  pivot_wider(id_cols = ID, values_from = SI, names_from = NO.)
ema_km_stat_data = ema_km_stat_data %>%
  filter(rowSums(!is.na(ema_km_stat_data)) > 4)

  ## compute statistics as objects for k-means in ema_km_stat##
ema_km_stat = ema_km_stat_data %>%
  transmute(ID = ID, mean = rowMeans(ema_km_stat_data[,-1], na.rm = T), 
            sd = apply(ema_km_stat_data[,-1], 1, function(x) sd(x, na.rm = T)), 
            min = apply(ema_km_stat_data[,-1], 1, function(x) min(x, na.rm = T)), 
            max = apply(ema_km_stat_data[,-1], 1, function(x) max(x, na.rm = T)), 
            p_non0 = apply(ema_km_stat_data[,-1], 1, function(x) mean(x[which(!is.na(x))]>0)), 
            rmssd = apply(ema_km_stat_data[,-1], 1, function(x) sqrt(mean((diff(as.numeric(x[which(!is.na(x))])))^2)))) %>%
  mutate(switch_freq = rep(0, n()))
for (i in 1:nrow(ema_km_stat)){
  sub = filter(blake, ID == ema_km_stat$ID[i])
  ema_km_stat$switch_freq[i] = mean(difftime(sub$timestamp[-1], sub$timestamp[-nrow(sub)], unit = "hours"))
}

ID_stat = unique(ema_km_stat$ID)


  ## try different numbers of clusters (k) to find which k to use ##
k.max = 20
wss = sapply(2:k.max,function(k){kmeans(ema_km_stat[,-1], k)$tot.withinss})
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", 
     main = "K-means with statistics") ## k = 5 or 9
    
  ## k-means ##
k_stat = 5
km_stat = kmeans(ema_km_stat[,-1], centers = k_stat)

km_stat_cluster = cbind(ID = ema_km_stat[,1], cluster = km_stat$cluster)

km_stat_dist = cbind(km_stat_cluster, dist = rep(0, nrow(km_stat_cluster)))
for (i in 1:nrow(km_stat_cluster)){
  km_stat_dist[i, "dist"] = sqrt(sum((ema_km_stat[i, -1] - km_stat$centers[km_stat_cluster$cluster,])^2))
} 

  ## re-order clusters by the average SI mean ##
stat_mean = rep(0,k_stat)
for (i in 1:k_stat){
  temp = unlist(as.vector(ema_km_stat_data[which(ema_km_stat_data$ID %in% km_stat_cluster[which(km_stat_cluster[,2] == i),1]), -1]))
  stat_mean[i] = round(mean(temp, na.rm = TRUE), 2)
}
stat_mean
rank(stat_mean)
temp = rep(0,nrow(km_stat_cluster))
for(i in 1:k_stat){
  temp[which(km_stat_cluster[,2]==i)] = rank(stat_mean)[i]
}
km_stat_cluster[,2] = temp

  ## plots 1.1: all obs. & in both ema and bp##
library(gridExtra)
library(ggplot2)
library(grid)
ema_plot = data_rm(transmute(blake, ID = ID, timestamp = timestamp, SI = sitb_si_sum))
bp_plot = data_rm(transmute(bh, ID = ID, timestamp = timestamp))

ema_stat_plot = filter(ema_plot, ID %in% bp_plot$ID, ID %in% ID_stat ) %>%
  group_by(ID) %>%
  mutate(timestamp = timestamp, SI = SI, SI_centr = SI - mean(SI),  SI_scale = scale(SI)) %>%
  ungroup()
ema_stat_plot$SI_scale[which(is.na(ema_stat_plot$SI_scale))] = 0
ema_stat_plot = mutate(ema_stat_plot, 
                       cluster = apply(ema_stat_plot, 1, function(x){km_stat_cluster[which(km_stat_cluster[,1] == x[1]),2]})) %>%
  arrange(cluster)

bp_stat_plot = filter(bp_plot, ID %in% ema_stat_plot$ID)

n_obs_stat = length(unique(ema_stat_plot$ID))

ylim_stat = c(min(ema_stat_plot$SI), max(ema_stat_plot$SI))
ylim_centr_stat = c(min(ema_stat_plot$SI_centr), max(ema_stat_plot$SI_centr))


col_bg = c("cornsilk", "greenyellow", "skyblue", "grey", 
           "paleturquoise", "ghostwhite", "lightgreen", "antiquewhite", 
           "thistle", "pink")  #color for different clusters
p = list()
for(i in 1:n_obs_stat){
  id = unique(ema_stat_plot$ID)[i]
  ema_plot_sub = filter(ema_stat_plot, ID == id)
  mean_SI = round(mean(ema_plot_sub$SI), 2)
  bp_plot_sub = filter(bp_stat_plot, ID == id)
  
  cluster_sub = ema_plot_sub$cluster[1]
  p[[i]] = ggplot(data=ema_plot_sub, mapping = aes(x = timestamp, y = SI)) + 
    geom_line(linetype = 1) + 
    geom_point(shape = 1) + 
    geom_point(data=bp_plot_sub, mapping = aes(x = timestamp, y = 0), color = "red", size = .5) + 
    geom_vline(xintercept = bp_plot_sub$timestamp, linetype = "dashed", color = "red", size = .1) + 
    xlab("Time") + 
    ylab("SI score") + 
    labs(title = paste(id, ", mean = ", mean_SI)) + 
    ylim(ylim_stat) +
    theme(plot.background = element_rect(fill = col_bg[cluster_sub]))
}
grid.arrange(grobs = p, ncol = 6)



    ## plots 1.2: all obs. ##
ema_stat_plot1.2 = filter(ema_plot, ID %in% ID_stat ) %>%
  group_by(ID) %>%
  mutate(timestamp = timestamp, SI = SI, SI_centr = SI - mean(SI),  SI_scale = scale(SI)) %>%
  ungroup()
ema_stat_plot1.2$SI_scale[which(is.na(ema_stat_plot1.2$SI_scale))] = 0
ema_stat_plot1.2 = mutate(ema_stat_plot1.2, 
                       cluster = apply(ema_stat_plot1.2, 1, 
                                       function(x){km_stat_cluster[which(km_stat_cluster[,1] == x[1]),2]})) %>%
  arrange(cluster)

bp_stat_plot1.2 = filter(bp_plot, ID %in% ema_stat_plot1.2$ID)

n_obs_stat1.2 = length(unique(ema_stat_plot1.2$ID))

ylim_stat1.2 = c(min(ema_stat_plot1.2$SI), max(ema_stat_plot1.2$SI))

p = list()
for(i in 1:n_obs_stat1.2){
  id = unique(ema_stat_plot1.2$ID)[i]
  ema_plot_sub = filter(ema_stat_plot1.2, ID == id)
  mean_SI = round(mean(ema_plot_sub$SI), 2)
  cluster_sub = ema_plot_sub$cluster[1]
  
  if(id %in% bp_stat_plot1.2$ID){
    bp_plot_sub = filter(bp_stat_plot1.2, ID == id)
    
    p[[i]] = ggplot(data=ema_plot_sub, mapping = aes(x = timestamp, y = SI)) + 
      geom_line(linetype = 1) + 
      geom_point(shape = 1) + 
      geom_point(data=bp_plot_sub, mapping = aes(x = timestamp, y = 0), color = "red", size = .5) + 
      geom_vline(xintercept = bp_plot_sub$timestamp, linetype = "dashed", color = "red", size = .1) + 
      xlab("Time") + 
      ylab("SI score") + 
      labs(title = paste(id, ", mean = ", mean_SI)) + 
      ylim(ylim_stat1.2) +
      theme(plot.background = element_rect(fill = col_bg[cluster_sub]))
  }
  
  else{
    p[[i]] = ggplot(data=ema_plot_sub, mapping = aes(x = timestamp, y = SI)) + 
      geom_line(linetype = 1) + 
      geom_point(shape = 1) + 
      xlab("Time") + 
      ylab("SI score") + 
      labs(title = paste(id, ", mean = ", mean_SI)) + 
      ylim(ylim_stat) +
      theme(plot.background = element_rect(fill = col_bg[cluster_sub]))
  }
}
grid.arrange(grobs = p, ncol = 6)


    ## plots 2: 5 obs. for each cluster (nearest to the centers)##
represent_obs = km_stat_dist %>%
  group_by(cluster) %>%
  arrange(dist, .by_group = TRUE) %>%
  mutate(order = 1:n()) %>%
  filter(!order>5) %>%
  ungroup() %>%
  arrange(ID)

ema_stat_plot2 = filter(ema_plot, ID %in% represent_obs$ID ) %>%
  group_by(ID) %>%
  mutate(timestamp = timestamp, SI = SI, SI_centr = SI - mean(SI),  SI_scale = scale(SI)) %>%
  ungroup()
ema_stat_plot2$SI_scale[which(is.na(ema_stat_plot2$SI_scale))] = 0
ema_stat_plot2 = mutate(ema_stat_plot2, 
                       cluster = apply(ema_stat_plot2, 1, function(x){km_stat_cluster[which(km_stat_cluster[,1] == x[1]),2]})) %>%
  arrange(cluster)

bp_stat_plot2 = filter(bp_plot, ID %in% ema_stat_plot2$ID)

ylim_stat2 = c(min(ema_stat_plot2$SI), max(ema_stat_plot2$SI))
ylim_centr_stat2 = c(min(ema_stat_plot2$SI_centr), max(ema_stat_plot2$SI_centr))

p = list()
for(i in 1:nrow(represent_obs)){
  id = unique(ema_stat_plot2$ID)[i]
  ema_plot_sub = filter(ema_stat_plot2, ID == id)
  mean_SI = round(mean(ema_plot_sub$SI), 2)
  cluster_sub = ema_plot_sub$cluster[1]
  if (id %in% bp_stat_plot2$ID){
    bp_plot_sub = filter(bp_stat_plot2, ID == id)
    
    p[[i]] = ggplot(data=ema_plot_sub, mapping = aes(x = timestamp, y = SI)) + 
      geom_line(linetype = 1) + 
      geom_point(shape = 1) + 
      geom_point(data=bp_plot_sub, mapping = aes(x = timestamp, y = 0), color = "red", size = .5) + 
      geom_vline(xintercept = bp_plot_sub$timestamp, linetype = "dashed", color = "red", size = .1) + 
      xlab("Time") + 
      ylab("SI score") + 
      labs(title = paste(id, ", mean = ", mean_SI)) + 
      ylim(ylim_stat) +
      theme(plot.background = element_rect(fill = col_bg[cluster_sub]))
  }
  else{
    p[[i]] = ggplot(data=ema_plot_sub, mapping = aes(x = timestamp, y = SI)) + 
      geom_line(linetype = 1) + 
      geom_point(shape = 1) + 
      xlab("Time") + 
      ylab("SI score") + 
      labs(title = paste(id, ", mean = ", mean_SI)) + 
      ylim(ylim_stat2) +
      theme(plot.background = element_rect(fill = col_bg[cluster_sub]))
  }
  
}
grid.arrange(grobs = p, ncol = 5)




    ## PCA for visualization
ema_stat_pca = princomp(ema_km_stat[,-1])

library(ggfortify)
autoplot(ema_stat_pca, data = cbind(ema_km_stat, cluster = as.factor(km_stat_cluster$cluster)), colour = 'cluster', 
         main = paste("Clustering by K-means with Statistics (k = ", k_stat, ")", sep = ""), 
         label = T, label.size = 3) +
  theme(plot.title = element_text(hjust = 0.5))

  



