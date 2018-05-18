library(Rmisc)
library(readr)
library(ggplot2)
source("C:/Users/roohac/Documents/GitHub/ONR-Spectral-Sensitivity-Function-/performanceAnalysisFunctions.R")

##Import data

performance <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_ONR_processed_2018-05-09_14-48-36.csv")
performance$sub_cond <- paste(performance$Subject_id, performance$condition, sep = "_")
performance$sub_block <- paste(performance$Subject_id, performance$condition, performance$block, sep = "_")

performance1 <- subset(performance, Subject_id != "s132")
performance2 <- subset(performance, Subject_id != "s132" & sub_cond != "s129_high" & sub_block != "s176_medium_4"
                       & sub_block != "s157_high_4" & Subject_id != "s146" & sub_block != "s153_high_3" 
                       & sub_cond != "s153_low" & sub_cond != "s159_medium" & sub_block != "s161_high_3"
                       & sub_block != "s161_high_4" & sub_cond != "s161_medium" & sub_block != "s161_low_3"
                       & sub_block != "s106_high_3" & sub_block != "s106_high_4" & Subject_id != "s109" 
                       & sub_block != "s127_low_3" & sub_block != "s127_medium_3" & sub_block != "s127_medium_4" 
                       & sub_block != "s134_high_1")

performance3 <- subset(performance2, block != 4)
performance4 <- subset(performance1, block != 4)

if(FALSE){
  performance <- read_csv("//root/projects/EEG-Daytime-ONR-2017/Raw-data/GNG_data/processedData/GNG_Day-4-25-18.csv")
  
  performance <- subset(performance, uni2 != "s223-2018-03-29-GNG")
}

####Generate data sets for analysis
# 1. All spectra normalised to dim - to analyze between spectra
# 2. All spectra - to analyze between light levels
# 3. Red only
# 4. Blue only
# 5. Green only

# Muliplied by 3 (False positve, Hit rate, and repsonse time) = 15 data sets

runNight <- function(data00){
  
  #1
  all_spectra_lightLevels <- data00
  print("All data: between spectra")
  analyzeGNG(all_spectra_lightLevels, TRUE, "all", TRUE)
  
  #2
  all_spectra_lightLevels <- data00
  print("All data: between light level")
  analyzeGNG(all_spectra_lightLevels, TRUE, "all", FALSE)
  
  #3
  red <- subset(data00, color == "red")
  print("Red")
  analyzeGNG(red, TRUE, "red", FALSE)
  
  #4
  blue <- subset(data00, color == "blue")
  print("Blue")
  analyzeGNG(blue, TRUE, "blue", FALSE)
  
  #5
  green <- subset(data00, color == "green")
  print("Green")
  analyzeGNG(green, TRUE, "green", FALSE)
  
  #6
  amber <- subset(data00, color == "amber")
  print("Amber")
  analyzeGNG(amber, TRUE, "amber", FALSE)
  
  #7
  cyan <- subset(data00, color == "cyan")
  print("Cyan")
  analyzeGNG(cyan, TRUE, "cyan", FALSE)
  

  
  
}
runNight(performance1)


#