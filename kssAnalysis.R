library(Rmisc)
library(readr)
library(ggplot2)
library(reshape2)
source("C:/Users/roohac/Documents/GitHub/ONR-Spectral-Sensitivity-Function-/kssAnalysisFunctions.R")


nighttime_KSS_normalized_time2 <- read_csv("//root/projects/EEG-Nighttime-ONR-2017/EEG-nighttime-performance-analysis-allgroups/nighttime_KSS_normalized_time2.csv")


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

