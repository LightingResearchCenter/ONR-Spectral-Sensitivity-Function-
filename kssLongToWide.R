library(Rmisc)
library(readr)
library(ggplot2)

if(FALSE){
  nighttime_KSS_normalized_time2 <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/KSS/nighttime_KSS_normalized_time2.xlsx")
  
  
  nighttime_KSS_normalized_time2_long <- melt(nighttime_KSS_normalized_time2, id.vars = c("Subject_id", "session", "date", "color","condition","group","month"))
  
  
  nighttime_KSS_normalized_time2_long2 <- subset(nighttime_KSS_normalized_time2_long, Subject_id != 122)
  
  wideKSS <- KSSlongTowide(nighttime_KSS_normalized_time2_long2)
}



KSSlongTowide <- function(data_kss, post_hoc){
  
  data_kss$session <- NULL
  data_kss$date <- NULL
  data_kss$group <- NULL
  
  data_kss$month <- NULL

  
  #colnames(data_kss)[5] <- "light_level"
  #colnames(data_kss)[9] <- "kss"
  
  #data_kss$light_level <- ifelse(data_kss$light_level =="dark", "dim", data_kss$light_level )
  
  data_kss2 <- subset(data_kss, !is.na(value) & variable != "k1" & variable != "k2")
  
  data_kss2$condition <- factor(data_kss2$condition, levels = c("dark" , "low", "medium", "high" ))
  data_kss2$id_var <- paste(data_kss2$variable, data_kss2$condition, sep = "_")
  
  data_kss2$condition <- NULL
  data_kss2$variable <- NULL
  #summaryData3 <- spread(data_kss2, id_var,  value)
  
  
  
  ###Norm to dim
  data_kssND <- data_kss  
  data_kssND$date <- NULL
  data_kssND$session <- NULL
  data_kssND$group <- NULL
  data_kssND$month <- NULL
  
  data_kssND <- subset(data_kssND, variable != "k1" & variable != "k2")
  data_kssND$condition <- factor(data_kssND$condition, levels = c("dark" , "low", "medium", "high" ))
  
  data_kssND$variable <- paste(data_kssND$variable, "D", sep = "" )
  
  data_kssND2 <- spread(data_kssND, condition,  value)
  data_kssND2$high <- data_kssND2$high/data_kssND2$dark 
  data_kssND2$medium <- data_kssND2$medium/data_kssND2$dark
  data_kssND2$low <- data_kssND2$low/data_kssND2$dark
  data_kssND2$dark <- NULL

  
  
  data_kssND4 <- melt(data_kssND2, id = c("Subject_id", "variable", "color"))
  
  colnames(data_kssND4)[4] <- "condition"
  data_kssND4$id_var <- paste(data_kssND4$variable, data_kssND4$condition, sep = "_")
  data_kssND4$condition <- NULL
  data_kssND4$variable <- NULL
  
  all_data <- rbind(data_kss2, data_kssND4)
  
  summaryData3 <- spread(all_data, id_var,  value)
  
  
  return(summaryData3)
 
}
