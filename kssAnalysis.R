library(Rmisc)
library(readr)
library(ggplot2)
library(readxl)
library(reshape2)
source("C:/Users/roohac/Documents/GitHub/ONR-Spectral-Sensitivity-Function-/kssAnalysisFunctions.R")


nighttime_KSS_normalized_time2 <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/KSS/nighttime_KSS_normalized_time2.xlsx")


nighttime_KSS_normalized_time2_long <- melt(nighttime_KSS_normalized_time2, id.vars = c("Subject_id", "session", "date", "color","condition","group","month"))



#1
kss <- nighttime_KSS_normalized_time2_long
analyzeKSS_normDim(kss, FALSE)

#2
kss <- nighttime_KSS_normalized_time2_long
analyzeKSS(kss, FALSE)

#3
red <- subset(nighttime_KSS_normalized_time2_long, color == "red")
analyzeKSS(red, FALSE)

#4
blue <- subset(nighttime_KSS_normalized_time2_long, color == "blue")
analyzeKSS(blue, FALSE)

#5
green <- subset(nighttime_KSS_normalized_time2_long, color == "green")
analyzeKSS(green, FALSE)

#6
amber<- subset(nighttime_KSS_normalized_time2_long, color == "amber")
analyzeKSS(amber, FALSE)

#7
cyan <- subset(nighttime_KSS_normalized_time2_long, color == "cyan")
analyzeKSS(cyan, FALSE)

