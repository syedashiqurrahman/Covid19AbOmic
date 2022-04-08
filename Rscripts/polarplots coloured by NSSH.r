
rm(list = ls())
cat("\014")

setwd("/Users/sar210/Box/covid19/FCprofiling/")

library(fmsb)
library(gplots)
library(corrplot)
library(tidyverse)
library(ggcorrplot)
library(readxl)
library("plot.matrix")
library(RColorBrewer)
library(plotrix)
library(dplyr)
library(NISTunits)

# setwd("C:/Users/preet/Documents")

#by antigen coloured by NS,S,H
#spike
t1 <- read_excel("polarplotexcel.xlsx", range = "ME85:ME130")
#spike RBD
t2 <- read_excel("polarplotexcel.xlsx", range = "MG85:MG130")
#Nucleocapsid
t3 <- read_excel("polarplotexcel.xlsx", range = "MI85:MI130")
#ORF3a
t4 <- read_excel("polarplotexcel.xlsx", range = "MK85:MK130")
#ORF8
t5 <- read_excel("polarplotexcel.xlsx", range = "MM85:MM130")
#nsp3
t6 <- read_excel("polarplotexcel.xlsx", range = "MO85:MO130")
#nsp13
t7 <- read_excel("polarplotexcel.xlsx", range = "MQ85:MQ130")
#M
t8 <- read_excel("polarplotexcel.xlsx", range = "MS85:MS130")

#OC43
t9 <- read_excel("polarplotexcel.xlsx", range = "MY85:MY136")
#NL63
t10 <- read_excel("polarplotexcel.xlsx", range = "NA85:NA136")
#Flu
t11 <- read_excel("polarplotexcel.xlsx", range = "NC85:NC136")

#only S & NS coloured
#spike
t12 <- read_excel("polarplotexcel.xlsx", range = "ME47:ME81")
#spike RBD
t13 <- read_excel("polarplotexcel.xlsx", range = "MG47:MG81")
#Nucelocapsid
t14 <- read_excel("polarplotexcel.xlsx", range = "MI47:MI81")

#canonical log2 values
tlog12 <- read_excel("polarplotexcel.xlsx", range = "MF47:MF81")
#spike RBD
tlog13 <- read_excel("polarplotexcel.xlsx", range = "Mh47:MH81")
#Nucelocapsid
tlog14 <- read_excel("polarplotexcel.xlsx", range = "MJ47:MJ81")


#Non canonical NS,S,H
# #orf3a
t15 <- read_excel("polarplotexcel.xlsx", range = "ML47:ML81")
#orf8
t16 <- read_excel("polarplotexcel.xlsx", range = "MN47:MN81")
#nsp3
t17 <- read_excel("polarplotexcel.xlsx", range = "MP47:MP81")
#nsp13
t18 <- read_excel("polarplotexcel.xlsx", range = "MR47:MR81")
#M protein
t19 <- read_excel("polarplotexcel.xlsx", range = "MT47:MT81")




#for NS,S,H without igg2&4
testpos1 <- read_excel("polarplotexcel.xlsx", range = "MB85:MB130")
#for OC43 only titer
testpos2 <- read_excel("polarplotexcel.xlsx", range = "MW85:MW136")

#only S&NS
testpos3 <- read_excel("polarplotexcel.xlsx", range = "MB47:MB81")

#palette(c("#FB3D9C","#554AFE")) #color palette adjustment
palette(c("#FB3D9C","#554AFE","#A9B1A9"))

par(mfrow=c(1,1), font= 2)

# #Spike
# polar.plot(t1,testpos1,main= "Spike", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","fcr1","fcr2a","fcr2b","fcr3a","fcr3b","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")

# # #Spike RBD
# polar.plot(t2,testpos1,main= "Spike RBD", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","fcr1","fcr2a","fcr2b","fcr3a","fcr3b","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")
# 
# # #Nucleocapsid
# polar.plot(t3,testpos1,main= "Nucleocapsid", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")
# 
# # #ORF3a
# polar.plot(t4,testpos1,main= "ORF 3a", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")
# 
# # #ORF8
# polar.plot(t5,testpos1,main= "ORF 8", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")
# 
# # #NSP3
# polar.plot(t6,testpos1,main= "NSP 3", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")
# 
# # #NSP13
# polar.plot(t7,testpos1,main= "NSP 13", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")
# 
# # #M
# polar.plot(t8,testpos1,main= "M protein", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG3","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(16,40,64,88,112,136,160,184,208,232,256,280,304,328,352),show.grid.labels = FALSE, grid.col = "gray80")


# # #OC43
# polar.plot(t9,testpos2,main= "HCoV OC43", radial.lim=c(0,60),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IgG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(14.12,35.3,56.48,77.66,98.84,120.02,141.2,162.38,183.56,204.74,225.92,247.1,268.28,289.46,310.64,331.82,353),show.grid.labels = FALSE, grid.col = "gray80")
# # # #NL63
# polar.plot(t10,testpos2,main= "HCoV NL63", radial.lim=c(0,60),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IgG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(14.12,35.3,56.48,77.66,98.84,120.02,141.2,162.38,183.56,204.74,225.92,247.1,268.28,289.46,310.64,331.82,353),show.grid.labels = FALSE, grid.col = "gray80")
# # # #Flu
polar.plot(t11,testpos2,main= "Flu", radial.lim=c(0,60),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IgG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(14.12,35.3,56.48,77.66,98.84,120.02,141.2,162.38,183.56,204.74,225.92,247.1,268.28,289.46,310.64,331.82,353),show.grid.labels = FALSE, grid.col = "gray80")

#only S&NS
# #Spike
# polar.plot(t12,testpos3,main= "Spike", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #Spike RBD
# polar.plot(t13,testpos3,main= "Spike RBD", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #Nucelocapsid
# polar.plot(t14,testpos3,main= "Nucelocapsid", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")

# #ORF3a
# polar.plot(t15,testpos3,main="ORF3a", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=3,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2a","FcR2b","FcR3a","FcR3b","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #orf8
# polar.plot(t16,testpos3,main="ORF8", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=3,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2a","FcR2b","FcR3a","FcR3b","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #nsp3
# polar.plot(t17,testpos3,main="NSP3", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=3,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2a","FcR2b","FcR3a","FcR3b","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #nsp13
# polar.plot(t18,testpos3,main="NSP 13", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=3,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2a","FcR2b","FcR3a","FcR3b","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #M
# polar.plot(t19,testpos3,main="M protein", radial.lim=c(0,80),start=120,clockwise=TRUE,lwd=3,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2a","FcR2b","FcR3a","FcR3b","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")

# #Spike RBD
# polar.plot(t12,testpos3,main="Spike", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=3,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","fcr1","fcr2a","fcr2b","fcr3a","fcr3b","C1q","Galactose","Sialic Acid"), label.pos = c(15.4,35.8,56.2,76.6,97,117.4,137.8,158.2,178.6,199,219.4,239.8,260.2,280.6,301,321.4,341.8),show.grid.labels = FALSE, grid.col = "gray80")
# #Spike RBD
# polar.plot(t12,testpos3,main="Spike", radial.lim=c(0,70),start=120,clockwise=TRUE,lwd=3,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","fcr1","fcr2a","fcr2b","fcr3a","fcr3b","C1q","Galactose","Sialic Acid"), label.pos = c(15.4,35.8,56.2,76.6,97,117.4,137.8,158.2,178.6,199,219.4,239.8,260.2,280.6,301,321.4,341.8),show.grid.labels = FALSE, grid.col = "gray80")


#Log2 values canonical
# #Spike
# polar.plot(tlog12,testpos3,main= "Spike", radial.lim=c(0,7),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #Spike RBD
# polar.plot(tlog13,testpos3,main= "Spike RBD", radial.lim=c(0,7),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")
# #Nucelocapsid
# polar.plot(tlog14,testpos3,main= "Nucelocapsid", radial.lim=c(0,7),start=120,clockwise=TRUE,lwd=4,line.col= c(1,4,7), labels = c("IgG","IgG1","IgG2","IgG3","IGG4","IgA","IgA1","IgA2","IgM","FcR1","FcR2A","FcR2B","FcR3A","FcR3B","C1q","Galactose","Sialic Acid"), label.pos = c(15.85,37.02,58.20,79.38,100.55,121.734,142.91,164.08,185.26,206.44,227.616,248.79,269.96,291.15,312.32,333.49,354.67),show.grid.labels = FALSE, grid.col = "gray80")

# 
