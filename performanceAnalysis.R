library(Rmisc)
library(readr)
library(ggplot2)
source("C:/Users/roohac/Documents/GitHub/ONR-Spectral-Sensitivity-Function-/performanceAnalysisFunctions.R")

##Import data

performance <-read_csv("//root/projects/EEG-Nighttime-ONR-2017/EEG-nighttime-performance-analysis-allgroups/performance_data_01-30-18-ONR-nighttime.csv")

####Generate data sets for analysis
# 1. All spectra normalised to dim - to analyze between spectra
# 2. All spectra - to analyze between light levels
# 3. Red only
# 4. Blue only
# 5. Green only

# Muliplied by 3 (False positve, Hit rate, and repsonse time) = 15 data sets

#1
all_spectra_lightLevels <- performance
analyzeGNG_normDim(all_spectra_lightLevels, FALSE)

#2
all_spectra_lightLevels <- performance
analyzeGNG(all_spectra_lightLevels, FALSE)

#3
red <- subset(performance, color == "red")
analyzeGNG(red, FALSE)

#4
blue <- subset(performance, color == "blue")
analyzeGNG(blue, FALSE)

#5
green <- subset(performance, color == "green")
analyzeGNG(green, FALSE)




#