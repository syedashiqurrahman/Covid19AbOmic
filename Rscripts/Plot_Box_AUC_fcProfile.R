
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Library/CloudStorage/Box-Box/covid19/FCprofiling/ReviewResponse/")
# setwd("/Users/syedashiqurrahman/Box/covid19/FCprofiling/")

library(ggplot2)
library(matrixStats)
library(ggedit)
library(grid)
library(gtools)
library(readxl)
library(gridExtra)

sig_text <- "P<0.01"
sig_grob = grid.text(sig_text, x=0.5,  y=0.95, gp=gpar(col="black", fontsize=8, fontface="bold"))

not_sig_text <- "n.s."
not_sig_grob = grid.text(not_sig_text, x=0.5,  y=0.95, gp=gpar(col="black", fontsize=8, fontface="bold"))


dF <- read_excel("PlotAUC_fcProfile.xlsx", 1)
Actual <- as.vector(dF[1:10, 2])
Permuted <- as.vector(dF[11:20, 2])
dF$Category <- as.factor(dF$Category)
p1 <- ggplot(dF, aes(x=Category, y=Precision, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p2 <- ggplot(dF, aes(x=Category, y=Recall, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p3 <- ggplot(dF, aes(x=Category, y=F1, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()

p <- grid.arrange(p1, p2, p3, nrow = 1)
p 
# p + theme_classic()  + annotation_custom(sig_grob) + theme(legend.position="None")+ ylim(0.45, 0.9) + xlab(" ") + ylab("AUC")
# ggsave("canonical_precision_recall_F1.pdf")


dF <- read_excel("PlotAUC_fcProfile.xlsx", 2)
Actual <- as.vector(dF[1:10, 2])
Permuted <- as.vector(dF[11:20, 2])
dF$Category <- as.factor(dF$Category)
p1 <- ggplot(dF, aes(x=Category, y=Precision, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p2 <- ggplot(dF, aes(x=Category, y=Recall, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p3 <- ggplot(dF, aes(x=Category, y=F1, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()

p <- grid.arrange(p1, p2, p3, nrow = 1)
p 
# p + theme_classic()  + annotation_custom(sig_grob) + theme(legend.position="None")+ ylim(0.45, 0.9) + xlab(" ") + ylab("AUC")
# ggsave("nonCanonical_precision_recall_F1.pdf")


dF <- read_excel("PlotAUC_fcProfile.xlsx", 3)
Actual <- as.vector(dF[1:10, 2])
Permuted <- as.vector(dF[11:20, 2])
dF$Category <- as.factor(dF$Category)
p1 <- ggplot(dF, aes(x=Category, y=Precision, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p2 <- ggplot(dF, aes(x=Category, y=Recall, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p3 <- ggplot(dF, aes(x=Category, y=F1, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()

p <- grid.arrange(p1, p2, p3, nrow = 1)
p 
# p + theme_classic()  + annotation_custom(sig_grob) + theme(legend.position="None")+ ylim(0.45, 0.9) + xlab(" ") + ylab("AUC")
# ggsave("C_NC_precision_recall_F1.pdf")


dF <- read_excel("PlotAUC_fcProfile.xlsx", 4)
Actual <- as.vector(dF[1:10, 2])
Permuted <- as.vector(dF[11:20, 2])
dF$Category <- as.factor(dF$Category)
p1 <- ggplot(dF, aes(x=Category, y=Precision, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p2 <- ggplot(dF, aes(x=Category, y=Recall, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p3 <- ggplot(dF, aes(x=Category, y=F1, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()

p <- grid.arrange(p1, p2, p3, nrow = 1)
p 
# p + theme_classic()  + annotation_custom(sig_grob) + theme(legend.position="None")+ ylim(0.45, 0.9) + xlab(" ") + ylab("AUC")
# ggsave("C_NC_Healthy_precision_recall_F1.pdf")


dF <- read_excel("PlotAUC_fcProfile.xlsx", 5)
Actual <- as.vector(dF[1:10, 2])
Permuted <- as.vector(dF[11:20, 2])
dF$Category <- as.factor(dF$Category)
p1 <- ggplot(dF, aes(x=Category, y=Precision, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p2 <- ggplot(dF, aes(x=Category, y=Recall, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()
p3 <- ggplot(dF, aes(x=Category, y=F1, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic()

p <- grid.arrange(p1, p2, p3, nrow = 1)
p 
# p + theme_classic()  + annotation_custom(sig_grob) + theme(legend.position="None")+ ylim(0.45, 0.9) + xlab(" ") + ylab("AUC")
# ggsave("2flu_precision_recall_F1.pdf")



rm(list = ls())
cat("\014")

setwd("/Users/sar210/Library/CloudStorage/Box-Box/covid19/FCprofiling/ReviewResponse/")
# setwd("/Users/syedashiqurrahman/Box/covid19/FCprofiling/")

library(ggplot2)
library(matrixStats)
library(ggedit)
library(grid)
library(gtools)
library(readxl)
library(gridExtra)

sig_text <- "P<0.01"
sig_grob = grid.text(sig_text, x=0.5,  y=0.95, gp=gpar(col="black", fontsize=8, fontface="bold"))

not_sig_text <- "n.s."
not_sig_grob = grid.text(not_sig_text, x=0.5,  y=0.95, gp=gpar(col="black", fontsize=8, fontface="bold"))


dF <- read_excel("Plot_AUC_clinicalOnly.xlsx", 2)
Actual <- as.vector(dF[1:10, 2])
Permuted <- as.vector(dF[11:20, 2])
dF$Category <- as.factor(dF$Category)
p <- ggplot(dF, aes(x=Category, y=ACC, colour = Category)) + geom_boxplot(width = 0.15) + theme_classic() + ylim(0.45, 0.9)
p 
p + theme_classic()  + annotation_custom(not_sig_grob) + theme(legend.position="None")+ ylim(0.45, 0.9) + xlab(" ") + ylab("AUC")
ggsave("clinicalOnly_AUC.pdf")

