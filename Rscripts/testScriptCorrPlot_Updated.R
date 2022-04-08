
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Library/CloudStorage/Box-Box/covid19/FCprofiling/")

library(matrixStats)
library(readxl)
library(ggplot2)
library(ggcorrplot)
library(DMwR2)
library(stringr)
library(pheatmap)
library(RColorBrewer)

df <- read.csv("fcProfile_Canonical_SARScov2_Covid_Updated.csv")

CTB <- subset(df, Y == 1)
DCG <- subset(df, Y == 0)

data <- rbind.data.frame(CTB, DCG)
data <- data[ , -c(1,2)]

# Y <- df$Y
# Outcome <- Y
# df <- cbind(Outcome, df)

corrDF <- cor(data, method = "spearman")
plot <- ggcorrplot(corrDF[,51:1]) 
plot + ggtitle("fcProfile correlations (Canonical)") +  theme(plot.title = element_text(hjust = 0.5))
ggsave("corrPlot_fcProfile_Canonical_updated.pdf")


df <- read.csv("fcProfile_nonCanonical_Covid_Updated.csv")

CTB <- subset(df, Y == 1)
DCG <- subset(df, Y == 0)

data <- rbind.data.frame(CTB, DCG)
data <- data[ , -c(1,2)]

# Y <- df$Y
# Outcome <- Y
# df <- cbind(Outcome, df)

corrDF <- cor(data, method = "spearman")
plot <- ggcorrplot(corrDF[, 85:1]) 
plot + ggtitle("fcProfile correlations (non-canonical)") +  theme(plot.title = element_text(hjust = 0.5))
ggsave("corrPlot_fcProfile_nonCanonical_updated.pdf")


df <- read.csv("fcProfile_Covid_2_Flu_Updated.csv")

CTB <- subset(df, Y == 1)
DCG <- subset(df, Y == 0)
data <- rbind.data.frame(CTB, DCG)
data <- data[ , -c(1,2)]
corrDF <- cor(data, method = "spearman")
plot <- ggcorrplot(corrDF[, 34:1]) 
plot + ggtitle("fcProfile correlations (flu)") +  theme(plot.title = element_text(hjust = 0.5))
ggsave("corrPlot_fcProfile_flu.pdf")



df <- read.csv("fcProfile_Canonical_nonCanonical_Updated.csv")

CTB <- subset(df, Y == 1)
DCG <- subset(df, Y == 0)

data <- rbind.data.frame(CTB, DCG)
data <- data[ , -c(1,2)]

# Y <- df$Y
# Outcome <- Y
# df <- cbind(Outcome, df)

corrDF <- cor(data, method = "spearman")
plot <- ggcorrplot(corrDF[,136:1]) 
plot + ggtitle("fcProfile correlations (All)") +  theme(plot.title = element_text(hjust = 0.5))
ggsave("corrPlot_fcProfile_ALL_updated.pdf")


CTB <- subset(df, Y == 1)
data <- CTB[ , -c(1,2)]
corrDF <- cor(data, method = "spearman")
plot <- ggcorrplot(corrDF[,136:1]) 
plot + ggtitle("fcProfile correlations (CTB)") +  theme(plot.title = element_text(hjust = 0.5))
# ggsave("corrPlot_fcProfile_CTB_updated.pdf")
# write.csv(corrDF, "corr_CTB.csv")

DCG <- subset(df, Y == 0)
data <- DCG[ , -c(1,2)]
corrDF <- cor(data, method = "spearman")
plot <- ggcorrplot(corrDF[,136:1]) 
plot + ggtitle("fcProfile correlations (discharged)") +  theme(plot.title = element_text(hjust = 0.5))
# ggsave("corrPlot_fcProfile_discharged_updated.pdf")
# write.csv(corrDF, "corr_DCG.csv")

CTB <- subset(df, Y == 1)
data <- CTB[ , -c(1,2)]
corrCTB <- cor(data, method = "spearman")
DCG <- subset(df, Y == 0)
data <- DCG[ , -c(1,2)]
corrDCG <- cor(data, method = "spearman")
# diff_CTB_DCG <- corrCTB - corrDCG
diff_CTB_DCG <- corrDCG - corrCTB
plot <- ggcorrplot(diff_CTB_DCG[, 136:1]) + scale_fill_gradient2(low = "blue", high = "red", breaks=c(-1.093168, 0.8539036), limit=c(-1.093168, 0.8539036))
pheatmap(as.matrix(diff_CTB_DCG),  main = "", color = colorRampPalette((brewer.pal(n = 11, name ="PiYG")))(100), cluster_rows = F, cluster_cols = F)
# plot <- ggcorrplot(diff_CTB_DCG[,48:1]) + scale_fill_gradient2(low = "blue", high = "red", breaks=c(-2, 2), limit=c(-2, 2))

which((abs(diff_CTB_DCG[41, ])) > 0.6)

plot + ggtitle("cytokine chemokine correlations differential") +  theme(plot.title = element_text(hjust = 0.5))
# ggsave("corrPlot_cytokine_chemokine_differential_updated.pdf")
# write.csv(diff_CTB_DCG, "corr_diff_CTB_DCG.csv")

# ggsave("corrPlot_cytokine_chemokine_differential_updated_test_pheatmap.pdf")

# max = 0.8539036
# min = -1.093168


CTB <- subset(df, Y == 1)
data <- CTB[ , -c(1,2)]
corrCTB <- cor(data, method = "spearman")
DCG <- subset(df, Y == 0)
data <- DCG[ , -c(1,2)]
corrDCG <- cor(data, method = "spearman")
diff_CTB_DCG <- corrDCG - corrCTB
plot <- ggcorrplot(diff_CTB_DCG[,48:1]) + scale_fill_gradient2(low = "blue", high = "red", breaks=c(-0.8539036, 1.093168), limit=c(-0.8539036, 1.093168))
plot + ggtitle("cytokine chemokine correlations differential") +  theme(plot.title = element_text(hjust = 0.5))
# ggsave("corrPlot_cytokine_chemokine_differential.pdf")

# max = 1.093168
# min = -0.8539036

# write.csv(diff_CTB_DCG, "Differential_CTB_DCG.csv")
# all
# features <- c("VEGF","MIP.1B","IL.8.HA.","IL.17C","IFN.Y","IL.10","IL.2","IL.6","IL.8","IL.17A.F","IL.17D","IL.1Ra","IL.3","IL.9","TSLP")
# Z3
features <- c("IL.8.HA.","IL.17C","IFN.Y","IL.10","IL.2","IL.6","IL.8","IL.17A.F","IL.17D","IL.1Ra","IL.3","IL.9","TSLP")
subCorrMSD <- as.data.frame(diff_CTB_DCG[, features])

IL.8.HA. <- subCorrMSD[order(- subCorrMSD$IL.8.HA.), ]
# VEGF <- subCorrMSD[order(- subCorrMSD$VEGF), ]
# MIP.1B <- subCorrMSD[order(- subCorrMSD$MIP.1B), ]
IL.17C <- subCorrMSD[order(- subCorrMSD$IL.17C), ]
IFN.Y <- subCorrMSD[order(- subCorrMSD$IFN.Y), ]
IL.10 <- subCorrMSD[order(- subCorrMSD$IL.10), ]
IL.2 <- subCorrMSD[order(- subCorrMSD$IL.2), ]
IL.6 <- subCorrMSD[order(- subCorrMSD$IL.6), ]
IL.8 <- subCorrMSD[order(- subCorrMSD$IL.8), ]
IL.17A.F <- subCorrMSD[order(- subCorrMSD$IL.17A.F), ]
IL.17D <- subCorrMSD[order(- subCorrMSD$IL.17D), ]
IL.1Ra <- subCorrMSD[order(- subCorrMSD$IL.1Ra), ]
IL.3 <- subCorrMSD[order(- subCorrMSD$IL.3), ]
IL.9 <- subCorrMSD[order(- subCorrMSD$IL.9), ]
TSLP <- subCorrMSD[order(- subCorrMSD$TSLP), ]


# write.csv(IL.8.HA., "Z3_IL.8.HA.csv")
# write.csv(IL.17C, "Z3_IL.17C.csv")
# write.csv(IFN.Y, "Z3_IFN.Y.csv")
# write.csv(IL.10, "Z3_IL.10.csv")
# write.csv(IL.2, "Z3_IL.2.csv")
# write.csv(IL.6, "Z3_IL.6.csv")
# write.csv(IL.8, "Z3_IL.8.csv")
# write.csv(IL.17A.F, "Z3_IL.17A.F.csv")
# write.csv(IL.17D, "Z3_IL.17D.csv")
# write.csv(IL.1Ra, "Z3_IL.1Ra.csv")
# write.csv(IL.3, "Z3_IL.3.csv")
# write.csv(IL.9, "Z3_IL.9.csv")
# write.csv(TSLP, "Z3_TSLP.csv")



