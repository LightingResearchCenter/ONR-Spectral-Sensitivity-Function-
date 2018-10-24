library(Rmisc)
library(readr)
library(ggplot2)

##Import data
if(FALSE){
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
  
  
  
  GNG_eegISIremoved <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegISIremoved_7_18_18.csv")
  
}

longToWide <- function(performanceData, dayNight){
  performanceData$daynight <- dayNight
  performanceData$Subject_id <- substr(performanceData$Subject_id, 2, 4)
  colnames(performanceData)[4] <- "subject"
  performanceData2 <- performanceData[c("subject", "daynight", "color", "condition", "block", "response_time", "display", "false_positive", "miss"  )]
  
  go_only <- subset(performanceData2, display == "go")
  no_only <- subset(performanceData2, display == "no_go")
  rt_gng <- subset(go_only, response_time >= .1)
  
  subject_fp_means <- aggregate(false_positive ~  subject + daynight + color + condition + block , data = no_only, FUN = mean)
  subject_miss_means <- aggregate(miss ~  subject + daynight + color + condition + block , data = go_only, FUN = mean)
  subject_rt_means <- aggregate(response_time ~  subject + daynight + color + condition + block , data = rt_gng, FUN = mean)
  
  
  subject_fp_means$variable <- "fp"
  subject_miss_means$variable <- "miss"
  subject_rt_means$variable <- "rt"
  
  colnames(subject_fp_means)[6] <- "value"
  colnames(subject_miss_means)[6] <- "value"
  colnames(subject_rt_means)[6] <- "value"
  
  summaryData <- rbind(subject_fp_means, subject_miss_means, subject_rt_means)
  
  summaryData$id_var <- paste(summaryData$variable, summaryData$condition, summaryData$block, sep = "_")
  summaryData$condition <- NULL
  summaryData$block <- NULL
  summaryData$variable <- NULL
  summaryData2 <- summaryData[c("subject", "daynight", "color", "id_var", "value"  )]
  summaryData3 <- spread(summaryData2, id_var,  value)
  
  
  return(summaryData3)
}
exportData <- longToWide(GNG_eegISIremoved, "night")