
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Library/CloudStorage/Box-Box/covid19/FCprofiling/ReviewResponse/")


library(ggplot2)
library(matrixStats)
library(ggedit)
library(grid)
library(gtools)
library(readxl)
library(gridExtra)

df <- read_excel("Covid_ARDS_data_formatted.xlsx", 1)


# Canonical Features
features <- c("Y", "IgA.Spike", "IgA2.Spike.RBD", "RCA.Spike.RBD")

data <- df[, names(df) %in% features]

data$Y <- as.factor(data$Y)
p1 <- ggplot(data, aes(x = Y, y=IgA.Spike, col=Y)) + geom_point() + theme_classic()
p2 <- ggplot(data, aes(x = Y, y=IgA2.Spike.RBD, col=Y)) + geom_point()+ theme_classic()
p3 <- ggplot(data, aes(x = Y, y=RCA.Spike.RBD, col=Y)) + geom_point() + theme_classic()

p <- grid.arrange(p1, p2, p3, nrow = 1)
p 

# pdf("canonical_ARDS.pdf", width = 8, height = 4) # Open a new pdf file
# p <- grid.arrange(p1, p2, p3, nrow = 1)
# dev.off() # Close the file


# nonCanonical Features
features <- c("Y", "IgA.Orf8", "FcR3a.M", "RCA.M", "IgG3.Nsp13")

data <- df[, names(df) %in% features]

data$Y <- as.factor(data$Y)
p1 <- ggplot(data, aes(x = Y, y=IgA.Orf8, col=Y)) + geom_point() + theme_classic()
p2 <- ggplot(data, aes(x = Y, y=FcR3a.M, col=Y)) + geom_point()+ theme_classic()
p3 <- ggplot(data, aes(x = Y, y=RCA.M, col=Y)) + geom_point() + theme_classic()
p4 <- ggplot(data, aes(x = Y, y=IgG3.Nsp13, col=Y)) + geom_point() + theme_classic()

p <- grid.arrange(p1, p2, p3, p4, nrow = 1)
p 

# pdf("nonCanonical_ARDS.pdf", width = 8, height = 4) # Open a new pdf file
# p <- grid.arrange(p1, p2, p3, p4, nrow = 1)
# dev.off() # Close the file

