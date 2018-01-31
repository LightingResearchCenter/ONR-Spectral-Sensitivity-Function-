library(Rmisc)
library(readr)
library(ggplot2)
library(lsmeans)
library(nlme)
library(MuMIn)
library(tidyr)
library(reshape2)

print_output <- function(output_list){
  
  print(output_list[1])
  print(output_list[2])
  print(output_list[3])
  
  
}
analyzeKSS <- function(data_kss, post_hoc){
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons :light level------"
  text4 <- "Pairwise comparisons :time------"
  text5 <- "Pairwise comparisons : interaction (light level | time) ------"
  
  colnames(data_kss)[5] <- "light_level"
  colnames(data_kss)[8] <- "time"
  colnames(data_kss)[9] <- "kss"
  
  data_kss$light_level <- ifelse(data_kss$light_level =="dark", "dim", data_kss$light_level )
  
  
  
  data_kss$Subject_id <- as.factor(data_kss$Subject_id)
  data_kss$light_level <- as.factor(data_kss$light_level)
  data_kss$color <- as.factor(data_kss$color)
  data_kss$time <- as.factor(data_kss$time)
  
  data_kss2 <- subset(data_kss, !is.na(kss) & time != "k1" & time != "k2")

  
  kss_model <- lme(kss ~ light_level*time , random = ~1|Subject_id/light_level/time,
                  data=data_kss2)
  
  kss_model_r2 <- r.squaredGLMM(kss_model)
  
  kss_model_pVal <- anova(kss_model)
  
  if(post_hoc == TRUE){
    
    kss_model_postHoc1 <- lsmeans(kss_model, pairwise~light_level, adjust="tukey", data = data_kss2)
    kss_model_postHoc2 <- lsmeans(kss_model, pairwise~time, adjust="tukey", data = data_kss2)
    kss_model_postHoc3 <- lsmeans(kss_model, pairwise~light_level|time, adjust="tukey", data = data_kss2)
    
    output_list1 <- list(text1, kss_model_r2, text2,  kss_model_pVal, text3, kss_model_postHoc1, text4, kss_model_postHoc2, text5, kss_model_postHoc3)
  }else{
    output_list1 <- list(text1, kss_model_r2, text2,  kss_model_pVal)
    
  }
 
  print(output_list1)
}

analyzeKSS_normDim <- function(data_kss, post_hoc){
  
  
  
  colnames(data_kss)[5] <- "light_level"
  colnames(data_kss)[8] <- "time"
  colnames(data_kss)[9] <- "kss"
  data_kss$light_level <- ifelse(data_kss$light_level =="dark", "dim", data_kss$light_level )
  
  
  
  data_kss$Subject_id <- as.factor(data_kss$Subject_id)
  data_kss$light_level <- as.factor(data_kss$light_level)
  data_kss$color <- as.factor(data_kss$color)
  data_kss$time <- as.factor(data_kss$time)
  
  
  data_kss$date <- NULL
  data_kss$session <- NULL
  
  data_kss <- subset(data_kss, time != "k1" & time != "k2")
  data_kss2 <- spread(data_kss, light_level,  kss)
  data_kss2$high <- data_kss2$high/data_kss2$dim 
  data_kss2$medium <- data_kss2$medium/data_kss2$dim
  data_kss2$low <- data_kss2$low/data_kss2$dim
  data_kss2$dim <- NULL
  data_kss2$group <- NULL
  data_kss2$month <- NULL
  
  data_kss4 <- melt(data_kss2, id = c("Subject_id", "time", "color"))
  colnames(data_kss4)[4] <- "light_level"
  colnames(data_kss4)[5] <- "kss"
  data_kss5 <- subset(data_kss4, !is.na(kss) )
  
  
  kss_model <- lme(kss ~ color*light_level*time , random = ~1|Subject_id/light_level/time,
                  data=data_kss5)
  
  
  
  kss_model_r2 <- r.squaredGLMM(kss_model)
  
  kss_model_pVal <- anova(kss_model)
  
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons : color------"
  text4 <- "Pairwise comparisons : light level------"
  text5 <- "Pairwise comparisons : time------"
  text6 <- "Pairwise comparisons : interaction (light level | time) ------"
  text7 <- "Pairwise comparisons : interaction (color | time) ------"
  
  if(post_hoc == TRUE){
    
    kss_model_postHoc1 <- lsmeans(kss_model, pairwise~color, adjust="tukey", data = data_kss5)
    kss_model_postHoc2 <- lsmeans(kss_model, pairwise~light_level, adjust="tukey", data = data_kss5)
    kss_model_postHoc3 <- lsmeans(kss_model, pairwise~time, adjust="tukey", data = data_kss5)
    kss_model_postHoc4 <- lsmeans(kss_model, pairwise~color|time, adjust="tukey", data = data_kss5)
    kss_model_postHoc5 <- lsmeans(kss_model, pairwise~light_level|time, adjust="tukey", data = data_kss5)
    
    
    output_list1 <- list(text1, kss_model_r2, text2,  kss_model_pVal, text3, kss_model_postHoc1, text4, kss_model_postHoc2, text5, kss_model_postHoc3, text6, kss_model_postHoc4, text7, kss_model_postHoc5)
  }else{
    output_list1 <- list(text1, kss_model_r2, text2,  kss_model_pVal)
    
  }
  
  
  
  

  
  print(output_list1)
  
}


