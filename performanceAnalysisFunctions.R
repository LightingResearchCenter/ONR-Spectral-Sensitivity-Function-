library(Rmisc)
library(readr)
library(ggplot2)
library(lsmeans)
library(reshape2)
library(nlme)
library(ggsignif)
library(readxl)
library(lme4)
library(MuMIn)
library(tidyr)
library(data.table)
library(WordR)
library(magrittr)
library(ReporteRs)

print_output <- function(output_list){
  
  print(output_list[1])
  print(output_list[2])
  print(output_list[3])
  
  
}
options("ReporteRs-fontsize"=10)

analyzeGNG <- function(data_performance, post_hoc, color, normDim, doc){
  ctrl <- lmeControl(opt='optim');
  
  if(normDim == "multipleNorm"){
    baseCellProp = cellProperties( padding = 0 )
    doc <- addTitle( doc,normDim, level = 1 )
    
    
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
    subject_fp_means2$L4 <- subject_fp_means2$L4/subject_fp_means2$dim 
    subject_fp_means2$L3 <- subject_fp_means2$L3/subject_fp_means2$dim
    subject_fp_means2$L1 <- subject_fp_means2$L1/subject_fp_means2$dim
    #subject_fp_means2$fifteen <- subject_fp_means2$fifteen/subject_fp_means2$dim
    subject_fp_means2$dim <- NULL
    subject_fp_means3 <- melt(subject_fp_means2, id = c("Subject_id", "block", "color"))
    colnames(subject_fp_means3)[4] <- "light_level"
    colnames(subject_fp_means3)[5] <- "false_positive"
    subject_fp_means4 <- subset(subject_fp_means3, !is.na(false_positive) & !is.infinite(false_positive))
    
    
    fp_model <- lme(false_positive ~ color*light_level*block , random = ~1|Subject_id,
                    data=subject_fp_means4, control = ctrl)
    
    
    
    fp_model_r2 <- r.squaredGLMM(fp_model)
    
    fp_model_pVal <- anova(fp_model)
    
    df1 <-  setDT(fp_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]     
    colnames(df1)[4] <- "F"  
    colnames(df1)[5] <- "p"    
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)   
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, "FP - between light level", level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    text1 <- "R-squared ----------"
    text2 <- "P-values ----------"
    text3 <- "Pairwise comparisons : color------"
    text4 <- "Pairwise comparisons : light level------"
    text5 <- "Pairwise comparisons : block------"
    text6 <- "Pairwise comparisons : interaction (light level | block) ------"
    text7 <- "Pairwise comparisons : interaction (color | block) ------"
    
    if(post_hoc == TRUE){
      
      fp_model_postHoc1 <- lsmeans(fp_model, pairwise~color, adjust="bonferroni", data = subject_fp_means4)
      fp_model_postHoc2 <- lsmeans(fp_model, pairwise~light_level, adjust="bonferroni", data = subject_fp_means4)
      fp_model_postHoc3 <- lsmeans(fp_model, pairwise~block, adjust="bonferroni", data = subject_fp_means4)
      fp_model_postHoc4 <- lsmeans(fp_model, pairwise~color|block, adjust="bonferroni", data = subject_fp_means4)
      fp_model_postHoc5 <- lsmeans(fp_model, pairwise~light_level|block, adjust="bonferroni", data = subject_fp_means4)
      
      
      output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal, text3, fp_model_postHoc1, text4, fp_model_postHoc2, text5, fp_model_postHoc3, text6, fp_model_postHoc4, text7, fp_model_postHoc5)
    }else{
      output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal)
      
    }
    
    
    ###Miss rate
    print("Miss rate in progress")
    
    miss_text <- "----------Miss rate----------"
    
    
    subject_miss_means <- aggregate(miss ~  Subject_id + color +light_level + block , data = go_only, FUN = mean)
    
    subject_miss_means2 <- spread(subject_miss_means, light_level,  miss)
    subject_miss_means2$L4 <- subject_miss_means2$L4/subject_miss_means2$dim 
    subject_miss_means2$L3 <- subject_miss_means2$L3/subject_miss_means2$dim
    subject_miss_means2$L1 <- subject_miss_means2$L1/subject_miss_means2$dim
    subject_miss_means2$dim <- NULL
    subject_miss_means3 <- melt(subject_miss_means2, id = c("Subject_id", "block", "color"))
    colnames(subject_miss_means3)[4] <- "light_level"
    colnames(subject_miss_means3)[5] <- "miss"
    subject_miss_means4 <- subset(subject_miss_means3, !is.na(miss) & !is.infinite(miss) )
    
    
    
    
    
    
    miss_model <- lme(miss ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                      data=subject_miss_means4)
    
    miss_model_r2 <- r.squaredGLMM(miss_model)
    
    miss_model_pVal <- anova(miss_model)
    
    df1 <-  setDT(miss_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]   
    colnames(df1)[4] <- "F"   
    colnames(df1)[5] <- "p"    
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)  
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    
    doc <- addTitle( doc, "Miss - between spectra", level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    
    text1 <- "R-squared ----------"
    text2 <- "P-values ----------"
    text3 <- "Pairwise comparisons : color------"
    text4 <- "Pairwise comparisons : light level------"
    text5 <- "Pairwise comparisons : block------"
    text6 <- "Pairwise comparisons : interaction (light level | block) ------"
    text7 <- "Pairwise comparisons : interaction (color | block) ------"
    
    if(post_hoc == TRUE){
      
      miss_model_postHoc1 <- lsmeans(miss_model, pairwise~color, adjust="bonferroni", data = subject_miss_means4)
      miss_model_postHoc2 <- lsmeans(miss_model, pairwise~light_level, adjust="bonferroni", data = subject_miss_means4)
      miss_model_postHoc3 <- lsmeans(miss_model, pairwise~block, adjust="bonferroni", data = subject_miss_means4)
      miss_model_postHoc4 <- lsmeans(miss_model, pairwise~color|block, adjust="bonferroni", data = subject_miss_means4)
      miss_model_postHoc5 <- lsmeans(miss_model, pairwise~light_level|block, adjust="bonferroni", data = subject_miss_means4)
      
      
      output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal, text3, miss_model_postHoc1, text4, miss_model_postHoc2, text5, miss_model_postHoc3, text6, miss_model_postHoc4, text7, miss_model_postHoc5)
    }else{
      output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal)
      
    }
    
    
    #Response time
    print("Response time in progress")
    
    rt_text <- "----------Response time----------"
    
    
    rt <- subset(data_performance, response_time >= .1 & display == "go" )
    
    
    subject_rt_means <- aggregate(response_time ~  Subject_id + color +light_level + block , data = rt, FUN = mean)
    
    subject_rt_means2 <- spread(subject_rt_means, light_level,  response_time)
    subject_rt_means2$L4 <- subject_rt_means2$L4/subject_rt_means2$dim 
    subject_rt_means2$L3 <- subject_rt_means2$L3/subject_rt_means2$dim
    subject_rt_means2$L1 <- subject_rt_means2$L1/subject_rt_means2$dim
    subject_rt_means2$dim <- NULL
    subject_rt_means3 <- melt(subject_rt_means2, id = c("Subject_id", "block", "color"))
    colnames(subject_rt_means3)[4] <- "light_level"
    colnames(subject_rt_means3)[5] <- "response_time"
    subject_rt_means4 <- subset(subject_rt_means3, !is.na(response_time) & !is.infinite(response_time))
    
    
    
    
    
    rt_model <- lme(response_time ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                    data=subject_rt_means4)
    
    rt_model_r2 <- r.squaredGLMM(rt_model)
    
    rt_model_pVal <- anova(rt_model)
    
    df1 <-  setDT(rt_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]   
    colnames(df1)[4] <- "F" 
    colnames(df1)[5] <- "p"     
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)  
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, "RT - between spectra", level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    
    
    
    text1 <- "R-squared ----------"
    text2 <- "P-values ----------"
    text3 <- "Pairwise comparisons : color------"
    text4 <- "Pairwise comparisons : light level------"
    text5 <- "Pairwise comparisons : block------"
    text6 <- "Pairwise comparisons : interaction (color | block) ------"
    text7 <- "Pairwise comparisons : interaction (light level | block) ------"
    text8 <- "Pairwise comparisons : interaction (light level | color) ------"
    
    
    
    if(post_hoc == TRUE){
      rt_model_postHoc1 <- lsmeans(rt_model, pairwise~color, adjust="bonferroni", data = subject_rt_means4)
      rt_model_postHoc2 <- lsmeans(rt_model, pairwise~light_level, adjust="bonferroni", data = subject_rt_means4)
      rt_model_postHoc3 <- lsmeans(rt_model, pairwise~block, adjust="bonferroni", data = subject_rt_means4)
      rt_model_postHoc4 <- lsmeans(rt_model, pairwise~color|block, adjust="bonferroni", data = subject_rt_means4)
      rt_model_postHoc5 <- lsmeans(rt_model, pairwise~light_level|block, adjust="bonferroni", data = subject_rt_means4)
      rt_model_postHoc6 <- lsmeans(rt_model, pairwise~light_level|color, adjust="bonferroni", data = subject_rt_means4)
      
      
      output_list3 <- list(rt_text, text1, rt_model_r2, text2,  rt_model_pVal, text3, rt_model_postHoc1, text4, rt_model_postHoc2, text5, rt_model_postHoc3, text6, rt_model_postHoc4, text7, rt_model_postHoc5, text8, rt_model_postHoc6)
      
    }else{
      output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal)
      
    }
    
    
    
    
    output_list <- list(output_list1, output_list2, output_list3)
    
    print_output(output_list)
    
  }
  
  
  if(normDim == "single"){
    baseCellProp = cellProperties( padding = 0 )
    
    
    ctrl <- lmeControl(opt='optim');
    
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
                    data=subject_fp_means, control = ctrl)
    
    fp_model_r2 <- r.squaredGLMM(fp_model)
    
    fp_model_pVal <- anova(fp_model)
    
    df1 <-  setDT(fp_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]  
    colnames(df1)[4] <- "F"   
    colnames(df1)[5] <- "p"      
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p) 
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, paste("Fp - between light level", color), level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    if(post_hoc == TRUE){
      
      fp_model_postHoc1 <- lsmeans(fp_model, pairwise~light_level, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc2 <- lsmeans(fp_model, pairwise~block, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc3 <- lsmeans(fp_model, pairwise~light_level|block, adjust="bonferroni", data = subject_fp_means)
      
      output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal, text3, fp_model_postHoc1, text4, fp_model_postHoc2, text5, fp_model_postHoc3)
    }else{
      output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal)
      
    }
    ###Miss rate
    print("Miss rate in progress")
    
    miss_text <- "----------Miss rate----------"
    
    
    subject_miss_means <- aggregate(miss ~  Subject_id + light_level + block , data = go_only, FUN = mean)
    
    subject_miss_means2 <- subject_miss_means
    
    
    
    miss_model <- lme(miss ~ light_level*block , random = ~1|Subject_id/light_level/block,
                      data=subject_miss_means2)
    
    miss_model_r2 <- r.squaredGLMM(miss_model)
    
    miss_model_pVal <- anova(miss_model)
    
    df1 <-  setDT(miss_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]    
    colnames(df1)[4] <- "F"  
    colnames(df1)[5] <- "p"      
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, paste("Miss - between light level", color), level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    if(post_hoc == TRUE){
      
      miss_model_postHoc1 <- lsmeans(miss_model, pairwise~light_level, adjust="bonferroni",  data = subject_miss_means2)
      miss_model_postHoc2 <- lsmeans(miss_model, pairwise~block, adjust="bonferroni",  data = subject_miss_means2)
      miss_model_postHoc3 <- lsmeans(miss_model, pairwise~light_level|block, adjust="bonferroni",  data = subject_miss_means2)
      
      output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal, text3,  miss_model_postHoc1, text4,  miss_model_postHoc2, text5,  miss_model_postHoc3)
    }else{
      output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal)
      
    }
    
    
    #Response time
    print("Response time in progress")
    
    rt_text <- "----------Response time----------"
    
    
    rt <- subset(data_performance, response_time >= .1 & display == "go" )
    
    
    rt_model <- lme(response_time ~ light_level*block , random = ~1|Subject_id/light_level/block,control=ctrl, 
                    data=rt)
    
    rt_model_r2 <- r.squaredGLMM(rt_model)
    
    rt_model_pVal <- anova(rt_model)
    
    df1 <-  setDT(rt_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]     
    colnames(df1)[4] <- "F"   
    colnames(df1)[5] <- "p"      
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)  
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, paste("RT - between light level - ", color), level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    
    if(post_hoc == TRUE){
      rt_model_postHoc1 <- lsmeans(rt_model, pairwise~light_level, adjust="bonferroni", data = rt)
      rt_model_postHoc2 <- lsmeans(rt_model, pairwise~block, adjust="bonferroni", data = rt)
      rt_model_postHoc3 <- lsmeans(rt_model, pairwise~light_level|block, adjust="bonferroni", data = rt)  
      
      output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal, text3, rt_model_postHoc1, text4, rt_model_postHoc2, text5, rt_model_postHoc3)
      
    }else{
      output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal)
      
    }
    
    
    
    output_list <- list(output_list1, output_list2, output_list3)
    
    print_output(output_list)
  }
  
 
  if(normDim == "multiple"){
    baseCellProp = cellProperties( padding = 0 )
    doc <- addTitle( doc,normDim, level = 1 )
    
    
    ctrl <- lmeControl(opt='optim');
    
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
    
    subject_fp_means <- aggregate(false_positive ~  Subject_id + color + light_level + block, data = no_only, FUN = mean)
    
    
    fp_model <- lme(false_positive ~ light_level*block*color , random = ~1|Subject_id/light_level/block,
                    data=subject_fp_means)
    
    fp_model_r2 <- r.squaredGLMM(fp_model)
    
    fp_model_pVal <- anova(fp_model)
    
    df1 <-  setDT(fp_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]  
    colnames(df1)[4] <- "F"   
    colnames(df1)[5] <- "p"      
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p) 
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, paste("Fp - between light level", color), level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    if(post_hoc == TRUE){
      fp_model_postHoc1 <- lsmeans(fp_model, pairwise~color, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc2 <- lsmeans(fp_model, pairwise~light_level, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc3 <- lsmeans(fp_model, pairwise~block, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc4 <- lsmeans(fp_model, pairwise~color|block, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc5 <- lsmeans(fp_model, pairwise~light_level|block, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc6 <- lsmeans(fp_model, pairwise~light_level|color, adjust="bonferroni", data = subject_fp_means)
      
      
      output_list3 <- list(rt_text, text1, rt_model_r2, text2,  rt_model_pVal, text3, rt_model_postHoc1, text4, rt_model_postHoc2)
    }else{
      output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal)
      
    }
    ###Miss rate
    print("Miss rate in progress")
    
    miss_text <- "----------Miss rate----------"
    
    
    subject_miss_means <- aggregate(miss ~  Subject_id + color + light_level + block , data = go_only, FUN = mean)
    
    subject_miss_means2 <- subject_miss_means
    
    
    
    miss_model <- lme(miss ~ light_level*block*color , random = ~1|Subject_id/light_level/block,
                      data=subject_miss_means2)
    
    miss_model_r2 <- r.squaredGLMM(miss_model)
    
    miss_model_pVal <- anova(miss_model)
    
    df1 <-  setDT(miss_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]    
    colnames(df1)[4] <- "F"  
    colnames(df1)[5] <- "p"      
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, paste("Miss - between light level", color), level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    if(post_hoc == TRUE){
      
      fp_model_postHoc1 <- lsmeans(fp_model, pairwise~color, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc2 <- lsmeans(fp_model, pairwise~light_level, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc3 <- lsmeans(fp_model, pairwise~block, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc4 <- lsmeans(fp_model, pairwise~color|block, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc5 <- lsmeans(fp_model, pairwise~light_level|block, adjust="bonferroni", data = subject_fp_means)
      fp_model_postHoc6 <- lsmeans(fp_model, pairwise~light_level|color, adjust="bonferroni", data = subject_fp_means)
      
      output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal, text3,  miss_model_postHoc1, text4,  miss_model_postHoc2, text5,  miss_model_postHoc3)
    }else{
      output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal)
      
    }
    
    
    #Response time
    print("Response time in progress")
    
    rt_text <- "----------Response time----------"
    
    
    rt <- subset(data_performance, response_time >= .1 & display == "go" )
    
    
    rt_model <- lme(response_time ~ light_level*block*color , random = ~1|Subject_id/light_level/block,control=ctrl, 
                    data=rt)
    
    rt_model_r2 <- r.squaredGLMM(rt_model)
    
    rt_model_pVal <- anova(rt_model)
    
    df1 <-  setDT(rt_model_pVal, keep.rownames = TRUE)[]
    
    df1 <- df1[-1,]     
    colnames(df1)[4] <- "F"   
    colnames(df1)[5] <- "p"      
    df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)  
    df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
    
    MyFTable = vanilla.table( data = df1 )
    
    MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
    
    doc <- addTitle( doc, paste("RT - between light level - ", color), level = 1 )
    
    doc = addFlexTable(doc, MyFTable)
    
    
    
    if(post_hoc == TRUE){
      rt_model_postHoc1 <- lsmeans(rt_model, pairwise~light_level, adjust="bonferroni", data = rt)
      rt_model_postHoc2 <- lsmeans(rt_model, pairwise~block, adjust="bonferroni", data = rt)
      rt_model_postHoc3 <- lsmeans(rt_model, pairwise~light_level|block, adjust="bonferroni", data = rt)  
      
      output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal, text3, rt_model_postHoc1, text4, rt_model_postHoc2, text5, rt_model_postHoc3)
      
    }else{
      output_list3 <- list(rt_text, text1, rt_model_r2, text2, rt_model_pVal)
      
    }
    
    
    
    output_list <- list(output_list1, output_list2, output_list3)
    
    print_output(output_list)
  }
  
  
  
return(doc)
}
  
analyzeGNG_normDim <- function(data_performance, post_hoc){
  baseCellProp = cellProperties( padding = 0 )
  
  
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
  subject_fp_means2$L4 <- subject_fp_means2$L4/subject_fp_means2$dim 
  subject_fp_means2$L3 <- subject_fp_means2$L3/subject_fp_means2$dim
  subject_fp_means2$L1 <- subject_fp_means2$L1/subject_fp_means2$dim
  subject_fp_means2$dim <- NULL
  subject_fp_means3 <- melt(subject_fp_means2, id = c("Subject_id", "block", "color"))
  colnames(subject_fp_means3)[4] <- "light_level"
  colnames(subject_fp_means3)[5] <- "false_positive"
  subject_fp_means4 <- subset(subject_fp_means3, !is.na(false_positive) & !is.infinite(false_positive))
  
  
  fp_model <- lme(false_positive ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                  data=subject_fp_means4)
  
  
  
  fp_model_r2 <- r.squaredGLMM(fp_model)
  
  fp_model_pVal <- anova(fp_model)
  
  df1 <-  setDT(fp_model_pVal, keep.rownames = TRUE)[]
  
  df1 <- df1[-1,]     
  colnames(df1)[4] <- "F"  
  colnames(df1)[5] <- "p"    
  df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)   
  df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
  
  MyFTable = vanilla.table( data = df1 )
  
  MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
  
  doc <- addTitle( doc, "FP - between light level", level = 1 )
  
  doc = addFlexTable(doc, MyFTable)
  
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons : color------"
  text4 <- "Pairwise comparisons : light level------"
  text5 <- "Pairwise comparisons : block------"
  text6 <- "Pairwise comparisons : interaction (light level | block) ------"
  text7 <- "Pairwise comparisons : interaction (color | block) ------"
  
  if(post_hoc == TRUE){
   
    fp_model_postHoc1 <- lsmeans(fp_model, pairwise~color, adjust="bonferroni", data = subject_fp_means4)
    fp_model_postHoc2 <- lsmeans(fp_model, pairwise~light_level, adjust="bonferroni", data = subject_fp_means4)
    fp_model_postHoc3 <- lsmeans(fp_model, pairwise~block, adjust="bonferroni", data = subject_fp_means4)
    fp_model_postHoc4 <- lsmeans(fp_model, pairwise~color|block, adjust="bonferroni", data = subject_fp_means4)
    fp_model_postHoc5 <- lsmeans(fp_model, pairwise~light_level|block, adjust="bonferroni", data = subject_fp_means4)
    
    
    output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal, text3, fp_model_postHoc1, text4, fp_model_postHoc2, text5, fp_model_postHoc3, text6, fp_model_postHoc4, text7, fp_model_postHoc5)
  }else{
    output_list1 <- list(fp_text, text1, fp_model_r2, text2,  fp_model_pVal)
    
    }
    
  
  ###Miss rate
  print("Miss rate in progress")
  
  miss_text <- "----------Miss rate----------"
  
  
  subject_miss_means <- aggregate(miss ~  Subject_id + color +light_level + block , data = go_only, FUN = mean)
  
  subject_miss_means2 <- spread(subject_miss_means, light_level,  miss)
  subject_miss_means2$L4 <- subject_miss_means2$L4/subject_miss_means2$dim 
  subject_miss_means2$L3 <- subject_miss_means2$L3/subject_miss_means2$dim
  subject_miss_means2$L1 <- subject_miss_means2$L1/subject_miss_means2$dim
  subject_miss_means2$dim <- NULL
  subject_miss_means3 <- melt(subject_miss_means2, id = c("Subject_id", "block", "color"))
  colnames(subject_miss_means3)[4] <- "light_level"
  colnames(subject_miss_means3)[5] <- "miss"
  subject_miss_means4 <- subset(subject_miss_means3, !is.na(miss) & !is.infinite(miss) )
  
  
  

  
  
  miss_model <- lme(miss ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                    data=subject_miss_means4)
  
  miss_model_r2 <- r.squaredGLMM(miss_model)
  
  miss_model_pVal <- anova(miss_model)
  
  df1 <-  setDT(miss_model_pVal, keep.rownames = TRUE)[]
  
  df1 <- df1[-1,]   
  colnames(df1)[4] <- "F"   
  colnames(df1)[5] <- "p"    
  df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)  
  df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
  
  MyFTable = vanilla.table( data = df1 )
  
  MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
  
  
  doc <- addTitle( doc, "Miss - between spectra", level = 1 )
  
  doc = addFlexTable(doc, MyFTable)
  
  
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons : color------"
  text4 <- "Pairwise comparisons : light level------"
  text5 <- "Pairwise comparisons : block------"
  text6 <- "Pairwise comparisons : interaction (light level | block) ------"
  text7 <- "Pairwise comparisons : interaction (color | block) ------"
  
  if(post_hoc == TRUE){
    
    miss_model_postHoc1 <- lsmeans(miss_model, pairwise~color, adjust="bonferroni", data = subject_miss_means4)
    miss_model_postHoc2 <- lsmeans(miss_model, pairwise~light_level, adjust="bonferroni", data = subject_miss_means4)
    miss_model_postHoc3 <- lsmeans(miss_model, pairwise~block, adjust="bonferroni", data = subject_miss_means4)
    miss_model_postHoc4 <- lsmeans(miss_model, pairwise~color|block, adjust="bonferroni", data = subject_miss_means4)
    miss_model_postHoc5 <- lsmeans(miss_model, pairwise~light_level|block, adjust="bonferroni", data = subject_miss_means4)
    
    
    output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal, text3, miss_model_postHoc1, text4, miss_model_postHoc2, text5, miss_model_postHoc3, text6, miss_model_postHoc4, text7, miss_model_postHoc5)
  }else{
    output_list2 <- list(miss_text, text1, miss_model_r2, text2,  miss_model_pVal)
    
  }
  
  
  #Response time
  print("Response time in progress")
  
  rt_text <- "----------Response time----------"
  
  
  rt <- subset(data_performance, response_time >= .1 & display == "go" )
  
  
  subject_rt_means <- aggregate(response_time ~  Subject_id + color +light_level + block , data = rt, FUN = mean)
  
  subject_rt_means2 <- spread(subject_rt_means, light_level,  response_time)
  subject_rt_means2$L4 <- subject_rt_means2$L4/subject_rt_means2$dim 
  subject_rt_means2$L3 <- subject_rt_means2$L3/subject_rt_means2$dim
  subject_rt_means2$L1 <- subject_rt_means2$L1/subject_rt_means2$dim
  subject_rt_means2$dim <- NULL
  subject_rt_means3 <- melt(subject_rt_means2, id = c("Subject_id", "block", "color"))
  colnames(subject_rt_means3)[4] <- "light_level"
  colnames(subject_rt_means3)[5] <- "response_time"
  subject_rt_means4 <- subset(subject_rt_means3, !is.na(response_time) & !is.infinite(response_time))
  
  
  
  
  
  rt_model <- lme(response_time ~ color*light_level*block , random = ~1|Subject_id/light_level/block,
                  data=subject_rt_means4)
  
  rt_model_r2 <- r.squaredGLMM(rt_model)
  
  rt_model_pVal <- anova(rt_model)
  
  df1 <-  setDT(rt_model_pVal, keep.rownames = TRUE)[]
  
  df1 <- df1[-1,]   
  colnames(df1)[4] <- "F" 
  colnames(df1)[5] <- "p"     
  df1$p <-  ifelse(!is.na(df1$p), substr(as.character(sprintf("%.3f", round(df1$p, digits = 3))), 2, 5), df1$p)  
  df1$F <-  ifelse(!is.na(df1$F), as.character(round(df1$F, digits = 3)), df1$F)
  
  MyFTable = vanilla.table( data = df1 )
  
  MyFTable[as.numeric(df1$p) < .05, 5] = chprop( baseCellProp, background.color = "green")
  
  doc <- addTitle( doc, "RT - between spectra", level = 1 )
  
  doc = addFlexTable(doc, MyFTable)
  
  
  
  
  
  text1 <- "R-squared ----------"
  text2 <- "P-values ----------"
  text3 <- "Pairwise comparisons : color------"
  text4 <- "Pairwise comparisons : light level------"
  text5 <- "Pairwise comparisons : block------"
  text6 <- "Pairwise comparisons : interaction (color | block) ------"
  text7 <- "Pairwise comparisons : interaction (light level | block) ------"
  text8 <- "Pairwise comparisons : interaction (light level | color) ------"
  


  if(post_hoc == TRUE){
    rt_model_postHoc1 <- lsmeans(rt_model, pairwise~color, adjust="bonferroni", data = subject_rt_means4)
    rt_model_postHoc2 <- lsmeans(rt_model, pairwise~light_level, adjust="bonferroni", data = subject_rt_means4)
    rt_model_postHoc3 <- lsmeans(rt_model, pairwise~block, adjust="bonferroni", data = subject_rt_means4)
    rt_model_postHoc4 <- lsmeans(rt_model, pairwise~color|block, adjust="bonferroni", data = subject_rt_means4)
    rt_model_postHoc5 <- lsmeans(rt_model, pairwise~light_level|block, adjust="bonferroni", data = subject_rt_means4)
    rt_model_postHoc6 <- lsmeans(rt_model, pairwise~light_level|color, adjust="bonferroni", data = subject_rt_means4)
    
    
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


