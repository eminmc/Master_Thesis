library("plyr")
library("ggplot2")

## Read the csv files, arrange Column names and bind all the results.
thompson.results          = read.csv("TS_2.csv", header = FALSE)
names(thompson.results)   = c("Sim", "T", "ChosenArm", "Reward", "CumulativeReward")
thompson.results          = transform(thompson.results, Algorithm = "Thompson Sampling")

epsilon.results           = read.csv("EPS_2.csv", header = FALSE)
names(epsilon.results)    = c("Epsilon", "Sim", "T", "ChosenArm", "Reward", "CumulativeReward")
epsilon.results           = transform(epsilon.results, Algorithm = paste(Epsilon, "-Greedy", sep = ""))
epsilon.results           = epsilon.results[, c("Sim", "T", "ChosenArm", "Reward", "CumulativeReward", "Algorithm")]

ucb1.results              = read.csv("UCB1_2.csv", header = FALSE)
names(ucb1.results)       = c("Sim", "T", "ChosenArm", "Reward", "CumulativeReward")
ucb1.results              = transform(ucb1.results, Algorithm = "UCB1")

results                   = rbind(thompson.results, ucb1.results,epsilon.results)




library("ggplot2")
library("plyr")

stats <- ddply(results,
               c("Algorithm", "T"),
               function (df) {mean(df$Reward)})
ggplot(stats, aes(x = T, y = V1, group = Algorithm, color = Algorithm)) +
  geom_line() +
  #stat_smooth(aes(y=V1, x=T), method = lm, formula = y ~ poly(x, 10), se = FALSE)+
  ylim(0, 1) +
  xlab("Observation") +
  ylab("Average Reward") +
  ggtitle("Performance of Different Algorithms")


#### Look for the gap
# Plot frequency of selecting correct arm as a function of time.
# In this instance, 1 is the correct arm.
stats <- ddply(results,
               c("Algorithm", "T"),
               function (df) {mean(df$ChosenArm == 4)})
ggplot(stats, aes(x = T, y = V1, group = Algorithm, color = Algorithm)) +
  geom_line() +
  #geom_hline(aes(yintercept = mean(V1)))  +
  #stat_smooth(aes(y=V1, x=T), method = lm, formula = y ~ poly(x, 10), se = FALSE)+
  ylim(0, 1) +
  xlab("Observation") +
  ylab("Probability of Selecting Best Arm") +
  ggtitle("Accuracy of Different Algorithms")



# Plot variance of chosen arms as a function of time.
stats <- ddply(results,
               c("Algorithm", "T"),
               function (df) {var(df$ChosenArm)})
ggplot(stats, aes(x = T, y = V1, group = Algorithm, color = Algorithm)) +
  geom_line() +
  xlab("Observation") +
  ylab("Variance of Chosen Arm") +
  ggtitle("Variability of Different Algorithms")



# Plot cumulative reward as a function of time.
stats <- ddply(results,
               c("Algorithm", "T"),
               function (df) {mean(df$CumulativeReward)})
ggplot(stats, aes(x = T, y = V1, group = Algorithm, color = Algorithm)) +
  geom_line() +
  xlab("Observation") +
  ylab("Cumulative Reward of Chosen Arm") +
  ggtitle("Cumulative Reward of Different Algorithms")

