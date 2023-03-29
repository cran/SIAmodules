# load libraries
library(ShinyItemAnalysis)
library(difNLR)
library(ggplot2)
library(moments)

# explore the variables of the dataset (from ShinyItemAnalysis)
names(AttitudesExpulsion)

# convert group variable to integer, assigning '1' to the experimental group
group <- as.numeric(AttitudesExpulsion[, "Group"] == "E")

# total score calculation with respect to group
score <- AttitudesExpulsion$PreMacro # or PreMicro, PostMacro/Micro, DelMacro/Micro
score0 <- score[group == 0] # control group
score1 <- score[group == 1] # experimental group

# summary of total score
tab <- rbind(
  c(
    length(score0), min(score0), max(score0), mean(score0), median(score0),
    sd(score0), skewness(score0), kurtosis(score0)
  ),
  c(
    length(score1), min(score1), max(score1), mean(score1), median(score1),
    sd(score1), skewness(score1), kurtosis(score1)
  )
)

colnames(tab) <- c("N", "Min", "Max", "Mean", "Median", "SD", "Skewness", "Kurtosis")
tab

# create a dataframe for plotting
df <- data.frame(score, group = as.factor(group))

# histogram of total scores with respect to group
ggplot(data = df, aes(x = score, fill = group, col = group)) +
  geom_histogram(binwidth = 1, position = "dodge2", alpha = 0.75) +
  xlab("Total score") +
  ylab("Number of respondents") +
  scale_fill_manual(
    values = c("dodgerblue2", "goldenrod2"), labels = c("Control", "Experimental")
  ) +
  scale_colour_manual(
    values = c("dodgerblue2", "goldenrod2"), labels = c("Control", "Experimental")
  ) +
  theme_app() +
  theme(legend.position = "left")

# t-test to compare total scores
t.test(score0, score1)
