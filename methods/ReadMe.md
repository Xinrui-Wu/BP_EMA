## File Description

1. dataManipulation.R: functions for cleaning the raw data and drawing simple boxplot for visulization.
2. model.R: simple glm models for the cleaned data.
3. dataManu_overlap.R: functions for cleaning the raw data and drawing simple boxplot for visulization (where each survey time may be counted more than once, called overlap data).
4. model_overlap.R: simple glm models for the cleaned overlap data.
5. dataExplore.R: further analysis on the bp_ema data, including distribution plots and ANOVA's.
6. CTM.R: try out for modeling the transition of SI score with continuous time Markov Chain. There are mainly 2 settings, one sets each SI state having its own unique parameters, while the other one groups the 28 states into 3 groups where states in a group having the same parameters. Besides, at the begining of the file, there is a sanity check about the transition trend at each SI level.
7. clustering.R: try out for clustering observations (people) in the EMA data. For now, we typically use the statistics on each person's data to perform K-means clustering.
