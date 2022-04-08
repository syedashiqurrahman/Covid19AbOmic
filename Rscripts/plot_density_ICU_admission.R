
rm(list = ls())
cat("\014")

library(ggplot2)

setwd("/Users/sar210/Library/CloudStorage/Box-Box/covid19/FCprofiling/ReviewResponse/")

# P_0.05
df <- read.csv("fcProfile_Clinical_matched.csv")
dF <- df

dF$Y <- as.factor(dF$Y)
p <- ggplot(dF, aes(durationICUstay, fill=Y, linetype=Y)) +  geom_density(alpha = 0.4, size=1)
p <- p + theme_classic()
p
# ggsave("distributions_ICUstay.pdf")


dF$Y <- as.factor(dF$Y)
p <- ggplot(dF, aes(x=Y, y=durationICUstay, colour = Y)) + geom_boxplot(width = 0.15) + theme_classic()
p <- p + theme_classic()
p
# ggsave("box_ICUstay.pdf")

dF$Y <- as.factor(dF$Y)
p <- ggplot(dF, aes(x=Y, y=timesamplefromicuadmission, colour = Y)) + geom_boxplot(width = 0.15) + theme_classic()
p <- p + theme_classic()
p
ggsave("box_timeSampleFromICUadmission.pdf")
