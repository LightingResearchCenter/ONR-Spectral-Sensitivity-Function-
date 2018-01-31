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
analyzeGNG <- function(data_performance, post_hoc){
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons :light level------"
  text4 <- "Pairwise comparisons :block------"
  text5 <- "Pairwise comparisons : interaction (light level | block) ------"
  
  colnames(data_performance)[15] <- "light_level"
  data_performance$light_level <- ifelse(data_performance$light_level =="dark", "dim", data_performance$light_level )
  
  
  
  data_performance$Subject_id <- as.factor(data_performance$Subject_id)
  data_performance$light_level <- as.factor(data_performance$light_level)
  data_performance$color <- as.factor(data_performance$color)
  data_performance$block <- as.factor(data_performance$block)
  
  
  go_only <- subset(data_performance, display == "go")
  no_only <- subset(data_performance, display == "no_go")
  
  
  ###False postive rate
  print("False postive rate in progress")
  
  
  fp_text <- "----------False postive----------"
  
  subject_fp_means <- aggregate(false_positive ~  Subject_id + light_level + block, data = no_only, FUN = mean)
  
  
  fp_model <- lme(false_positive ~ light_level*block , random = ~1|Subject_id/light_level/block,
                  data=subject_fp_means)
  
  fp_model_r2 <- r.squaredGLMM(fp_model)

  fp_model_pVal <- anova(fp_model)
  
  if(post_hoc == TRUE){
    
    fp_model_postHoc1 <- lsmeans(fp_model, pairwise~light_level, adjust="tukey", data = subject_fp_means)
    fp_model_postHoc2 <- lsmeans(fp_model, pairwise~block, adjust="tukey", data = subject_fp_means)
    fp_model_postHoc3 <- lsmeans(fp_model, pairwise~light_level|block, adjust="tukey", data = subject_fp_means)
    
    output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal, text3, fp_model_postHoc1, text4, fp_model_postHoc2, text5, fp_model_postHoc3)
  }else{
    output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal)
    
  }
  ###Miss rate
  print("Miss rate in progress")
  
  miss_text <- "----------Miss rate----------"
  
  
  subject_miss_means <- aggregate(miss ~  Subject_id + light_level + block , data = go_only, FUN = mean)
  
  subject_miss_means2 <- subset(subject_miss_means, miss < .5)
  

  
  miss_model <- lme(miss ~ light_level*block , random = ~1|Subject_id/light_level/block,
                    data=subject_miss_means2)
  
  miss_model_r2 <- r.squaredGLMM(miss_model)
  
  miss_model_pVal <- anova(miss_model)

  if(post_hoc == TRUE){
    
    miss_model_postHoc1 <- lsmeans(miss_model, pairwise~light_level, adjust="tukey",  data = subject_miss_means2)
    miss_model_postHoc2 <- lsmeans(miss_model, pairwise~block, adjust="tukey",  data = subject_miss_means2)
    miss_model_postHoc3 <- lsmeans(miss_model, pairwise~light_level|block, adjust="tukey",  data = subject_miss_means2)
    
    output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal, text3,  miss_model_postHoc1, text4,  miss_model_postHoc2, text5,  miss_model_postHoc3)
  }else{
    output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal)
    
  }
  
  
  #Response time
  print("Response time in progress")
  
  rt_text <- "----------Response time----------"
  
  
  rt <- subset(data_performance, response_time >= .2 & display == "go" & miss_percent < .4 & fp_percent < 0.5242368 & response_time < 1)
  

  rt_model <- lme(response_time ~ light_level*block , random = ~1|Subject_id/light_level/block,
                  data=rt)
  
  rt_model_r2 <- r.squaredGLMM(rt_model)
  
  rt_model_pVal <- anova(rt_model)
  
  if(post_hoc == TRUE){
    rt_model_postHoc1 <- lsmeans(rt_model, pairwise~light_level, adjust="tukey", data = rt)
    rt_model_postHoc2 <- lsmeans(rt_model, pairwise~block, adjust="tukey", data = rt)
    rt_model_postHoc3 <- lsmeans(rt_model, pairwise~light_level|block, adjust="tukey", data = rt)  
    
    output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal, text3, rt_model_postHoc1, text4, rt_model_postHoc2, text5, rt_model_postHoc3)
    
  }else{
    output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal)
    
  }
  output_list <- list(output_list1, output_list2, output_list3)
  
  print_output(output_list)
  
}
  
analyzeGNG_normDim <- function(data_performance, post_hoc){
  

  
  colnames(data_performance)[15] <- "light_level"
  data_performance$light_level <- ifelse(data_performance$light_level =="dark", "dim", data_performance$light_level )
  
  
  
  data_performance$Subject_id <- as.factor(data_performance$Subject_id)
  data_performance$light_level <- as.factor(data_performance$light_level)
  data_performance$color <- as.factor(data_performance$color)
  data_performance$block <- as.factor(data_performance$block)
  
  
  go_only <- subset(data_performance, display == "go")
  no_only <- subset(data_performance, display == "no_go")
  
  
  ###False postive rate
  print("False postive rate in progress")
  
  
  fp_text <- "----------False postive----------"
  
  subject_fp_means <- aggregate(false_positive ~  Subject_id + light_level + block + color, data = no_only, FUN = mean)
  
  subject_fp_means2 <- spread(subject_fp_means, light_level,  false_positive)
  subject_fp_means2$high <- subject_fp_means2$high/subject_fp_means2$dim 
  subject_fp_means2$medium <- subject_fp_means2$medium/subject_fp_means2$dim
  subject_fp_means2$low <- subject_fp_means2$low/subject_fp_means2$dim
  subject_fp_means2$dim <- NULL
  subject_fp_means3 <- melt(subject_fp_means2, id = c("Subject_id", "block", "color"))
  colnames(subject_fp_means3)[4] <- "light_level"
  colnames(subject_fp_means3)[5] <- "false_positive"
  subject_fp_means4 <- subset(subject_fp_means3, !is.na(false_positive) & !is.infinite(false_positive))
  
  
  fp_model <- lme(false_positive ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                  data=subject_fp_means4)
  
  
  
  fp_model_r2 <- r.squaredGLMM(fp_model)
  
  fp_model_pVal <- anova(fp_model)
  
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons : color------"
  text4 <- "Pairwise comparisons : light level------"
  text5 <- "Pairwise comparisons : block------"
  text6 <- "Pairwise comparisons : interaction (light level | block) ------"
  text7 <- "Pairwise comparisons : interaction (color | block) ------"
  
  if(post_hoc == TRUE){
   
    fp_model_postHoc1 <- lsmeans(fp_model, pairwise~color, adjust="tukey", data = subject_fp_means4)
    fp_model_postHoc2 <- lsmeans(fp_model, pairwise~light_level, adjust="tukey", data = subject_fp_means4)
    fp_model_postHoc3 <- lsmeans(fp_model, pairwise~block, adjust="tukey", data = subject_fp_means4)
    fp_model_postHoc4 <- lsmeans(fp_model, pairwise~color|block, adjust="tukey", data = subject_fp_means4)
    fp_model_postHoc5 <- lsmeans(fp_model, pairwise~light_level|block, adjust="tukey", data = subject_fp_means4)
    
    
    output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal, text3, fp_model_postHoc1, text4, fp_model_postHoc2, text5, fp_model_postHoc3, text6, fp_model_postHoc4, text7, fp_model_postHoc5)
  }else{
    output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal)
    
    }
    
  
  ###Miss rate
  print("Miss rate in progress")
  
  miss_text <- "----------Miss rate----------"
  
  
  subject_miss_means <- aggregate(miss ~  Subject_id + color +light_level + block , data = go_only, FUN = mean)
  
  subject_miss_means2 <- spread(subject_miss_means, light_level,  miss)
  subject_miss_means2$high <- subject_miss_means2$high/subject_miss_means2$dim 
  subject_miss_means2$medium <- subject_miss_means2$medium/subject_miss_means2$dim
  subject_miss_means2$low <- subject_miss_means2$low/subject_miss_means2$dim
  subject_miss_means2$dim <- NULL
  subject_miss_means3 <- melt(subject_miss_means2, id = c("Subject_id", "block", "color"))
  colnames(subject_miss_means3)[4] <- "light_level"
  colnames(subject_miss_means3)[5] <- "miss"
  subject_miss_means4 <- subset(subject_miss_means3, !is.na(miss) & !is.infinite(miss) & miss < .5)
  
  
  

  
  
  miss_model <- lme(miss ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                    data=subject_miss_means4)
  
  miss_model_r2 <- r.squaredGLMM(miss_model)
  
  miss_model_pVal <- anova(miss_model)
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons : color------"
  text4 <- "Pairwise comparisons : light level------"
  text5 <- "Pairwise comparisons : block------"
  text6 <- "Pairwise comparisons : interaction (light level | block) ------"
  text7 <- "Pairwise comparisons : interaction (color | block) ------"
  
  if(post_hoc == TRUE){
    
    miss_model_postHoc1 <- lsmeans(miss_model, pairwise~color, adjust="tukey", data = subject_miss_means4)
    miss_model_postHoc2 <- lsmeans(miss_model, pairwise~light_level, adjust="tukey", data = subject_miss_means4)
    miss_model_postHoc3 <- lsmeans(miss_model, pairwise~block, adjust="tukey", data = subject_miss_means4)
    miss_model_postHoc4 <- lsmeans(miss_model, pairwise~color|block, adjust="tukey", data = subject_miss_means4)
    miss_model_postHoc5 <- lsmeans(miss_model, pairwise~light_level|block, adjust="tukey", data = subject_miss_means4)
    
    
    output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal, text3, miss_model_postHoc1, text4, miss_model_postHoc2, text5, miss_model_postHoc3, text6, miss_model_postHoc4, text7, miss_model_postHoc5)
  }else{
    output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal)
    
  }
  
  
  #Response time
  print("Response time in progress")
  
  rt_text <- "----------Response time----------"
  
  
  rt <- subset(data_performance, response_time >= .2 & display == "go" & miss_percent < .4 & fp_percent < 0.5242368 & response_time < 1)
  
  
  subject_rt_means <- aggregate(response_time ~  Subject_id + color +light_level + block , data = rt, FUN = mean)
  
  subject_rt_means2 <- spread(subject_rt_means, light_level,  response_time)
  subject_rt_means2$high <- subject_rt_means2$high/subject_rt_means2$dim 
  subject_rt_means2$medium <- subject_rt_means2$medium/subject_rt_means2$dim
  subject_rt_means2$low <- subject_rt_means2$low/subject_rt_means2$dim
  subject_rt_means2$dim <- NULL
  subject_rt_means3 <- melt(subject_rt_means2, id = c("Subject_id", "block", "color"))
  colnames(subject_rt_means3)[4] <- "light_level"
  colnames(subject_rt_means3)[5] <- "response_time"
  subject_rt_means4 <- subset(subject_rt_means3, !is.na(response_time) & !is.infinite(response_time))
  
  
  
  
  
  rt_model <- lme(response_time ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                  data=subject_rt_means4)
  
  rt_model_r2 <- r.squaredGLMM(rt_model)
  
  rt_model_pVal <- anova(rt_model)
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons : color------"
  text4 <- "Pairwise comparisons : light level------"
  text5 <- "Pairwise comparisons : block------"
  text6 <- "Pairwise comparisons : interaction (color | block) ------"
  text7 <- "Pairwise comparisons : interaction (light level | block) ------"
  text8 <- "Pairwise comparisons : interaction (light level | color) ------"
  


  if(post_hoc == TRUE){
    rt_model_postHoc1 <- lsmeans(rt_model, pairwise~color, adjust="tukey", data = subject_rt_means4)
    rt_model_postHoc2 <- lsmeans(rt_model, pairwise~light_level, adjust="tukey", data = subject_rt_means4)
    rt_model_postHoc3 <- lsmeans(rt_model, pairwise~block, adjust="tukey", data = subject_rt_means4)
    rt_model_postHoc4 <- lsmeans(rt_model, pairwise~color|block, adjust="tukey", data = subject_rt_means4)
    rt_model_postHoc5 <- lsmeans(rt_model, pairwise~light_level|block, adjust="tukey", data = subject_rt_means4)
    rt_model_postHoc6 <- lsmeans(rt_model, pairwise~light_level|color, adjust="tukey", data = subject_rt_means4)
    
    
    output_list3 <- list(rt_text, text1, rt_model_r2, text2,  rt_model_pVal, text3, rt_model_postHoc1, text4, rt_model_postHoc2, text5, rt_model_postHoc3, text6, rt_model_postHoc4, text7, rt_model_postHoc5, text8, rt_model_postHoc6)
    
  }else{
    output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal)
    
  }

  
  
  output_list <- list(output_list1, output_list2, output_list3)
  
  print_output(output_list)
  
}

###Add miss and false postive percentage
add_miss_fp <- function(data_performance){
  df <- data.frame()
  
  block_list <- unique(data_performance$uni1)
  
  for(i in 1:length(block_list)){
    print(paste0("Block: ", i, "of", length(block_list)))
    print(block_list[i])
    current_block <- subset(data_performance, uni1 == block_list[i] )
    
    current_block$miss_percent <- sum(current_block$miss)/224
    current_block$fp_percent <- sum(current_block$false_positive)/56
    
    df <- rbind(df, current_block)
    
  }
  return(df)
}
