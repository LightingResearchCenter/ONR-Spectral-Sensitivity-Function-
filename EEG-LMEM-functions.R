library(reshape2)
library(MuMIn)
library(nlme)
library(tibble)
library(ReporteRs)
library(magrittr)
library(readr)
library(ggplot2)
library(lsmeans)
library(ggsignif)
library(readxl)
library(lme4)
library(MuMIn)
library(tidyr)
library(reshape2)
library(data.table)
library(ReporteRs)
library(magrittr)
library(WordR)


output_mixed_model1 <- function(data, time, doc, Save, UseCompleteOnly){
  title1 <- paste(time, "All data", sep = " ")
  data <- subset(data, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1)
  
  if(UseCompleteOnly){
    numSubs <- data.frame(table(data$Subject))
    numSubs <- numSubs[numSubs$Freq == 48,]
    
    data <- data[data$Subject %in% numSubs$Var1, ]
  }
  
  numSubs2 <- data.frame(table(data$subject, data$color,  data$light_level))
  
  numSubs000 <- data.frame(table(data$subject, data$color,  data$light_level, data$trial))
  
  
  numSubs2$Freq <- ifelse(numSubs2$Freq == 12, "COMPLETED", 
                          ifelse(numSubs2$Freq == 0, "MISSING", numSubs2$Freq))
  
  
  
  numSubs000 <- data.frame(table(data$subject, data$color))
  
  
  numSubs000$Freq <- ifelse(numSubs000$Freq == 48, "COMPLETED",  "MISSING")
  
  
  
  
  
  if(FALSE){
    
    
    MyFTable000 = vanilla.table( data = numSubs000 )
    
    textProp <- textProperties()
    
    MyFTable22 = vanilla.table( data = numSubs2 )
    
    textProp <- textProperties()
    
    doc = addFlexTable(doc, MyFTable000)
    doc = addFlexTable(doc, MyFTable22)
    
    
  }
  
  
  
  
  if(time == "both"){

    theta <- subset(data, Channel == "theta" & trial != 1 )
    atheta <- subset(data, Channel == "atheta" & trial != 1 )
    alpha <- subset(data, Channel == "alpha" & trial != 1 )
    halpha <- subset(data, Channel == "halpha" & trial != 1)
    hbeta <- subset(data, Channel == "hbeta" & trial != 1 )
    beta <- subset(data, Channel == "beta" & trial != 1)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1 ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1 ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1 ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1 ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1 ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1 ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){

    theta <- subset(data, Channel == "theta" & daynight == "day" & trial != 1)
    atheta <- subset(data, Channel == "atheta"  & daynight == "day" & trial != 1 )
    alpha <- subset(data, Channel == "alpha"   & daynight == "day" & trial != 1)
    halpha <- subset(data, Channel == "halpha"  & daynight == "day" & trial != 1)
    hbeta <- subset(data, Channel == "hbeta"  & daynight == "day" & trial != 1)
    beta <- subset(data, Channel == "beta" & daynight == "day" & trial != 1)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){

    theta <- subset(data, Channel == "theta" & daynight == "n" & trial != 1)
    atheta <- subset(data, Channel == "atheta"  & daynight == "n" & trial != 1)
    alpha <- subset(data, Channel == "alpha"   & daynight == "n" & trial != 1)
    halpha <- subset(data, Channel == "halpha"  & daynight == "n"& trial != 1)
    hbeta <- subset(data, Channel == "hbeta"  & daynight == "n"& trial != 1)
    beta <- subset(data, Channel == "beta" & daynight == "n"& trial != 1)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  aov_theta <- anova(theta_model)
  aov_atheta <- anova(atheta_model)
  aov_alpha <- anova(alpha_model)
  aov_halpha <- anova(halpha_model)
  aov_beta <- anova(beta_model)
  aov_hbeta <- anova(hbeta_model)

  
  
  rsq_theta <- r.squaredGLMM(theta_model)
  rsq_atheta <- r.squaredGLMM(atheta_model)
  rsq_alpha <- r.squaredGLMM(alpha_model)
  rsq_halpha <- r.squaredGLMM(halpha_model)
  rsq_beta <- r.squaredGLMM(beta_model)
  rsq_hbeta <- r.squaredGLMM(hbeta_model)
  
  output_list <- list(rsq_theta, aov_theta, rsq_atheta, aov_atheta, rsq_alpha, aov_alpha, rsq_halpha, aov_halpha, rsq_beta, aov_beta, rsq_hbeta, aov_hbeta )
  
  if(Save){

    aov_theta2 <- addheader(aov_theta, "Theta")
    aov_atheta2 <- addheader(aov_atheta, "Alpha-theta")
    aov_alpha2 <- addheader(aov_alpha, "Alpha")
    aov_halpha2 <- addheader(aov_halpha, "High alpha")
    aov_beta2 <- addheader(aov_beta, "Beta")
    aov_hbeta2 <- addheader(aov_hbeta, "High beta")
    

    
    doc = addTitle( doc, title1, level = 1 )
    
    final_df <- rbind(aov_theta2, aov_atheta2, aov_alpha2, aov_halpha2, aov_beta2, aov_hbeta2)
    
    
    MyFTable = vanilla.table( data = final_df )
    
    textProp <- textProperties()
    
    MyFTable[as.numeric(substr(final_df$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
    
    doc = addFlexTable(doc, MyFTable)
    
    
  }
  
  
  return(output_list)
}

addheader <- function(df, name){
  
  df2 <-  data.frame(df)
  
  df2 <- rownames_to_column(df2, "Dependent measures")
  df2 <- df2[2:length(df2$p.value),]
  
  df2$Bin <- ""
  df2$p.value <- ifelse(round(df2$p.value, digits = 3) == 1, "1", ifelse(round(df2$p.value, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(df2$p.value, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(df2$p.value, digits = 3))), 2, 5) ) )
  
  
  df2$F.value <-  as.character(round(df2$F.value, digits = 3))
  
  header1 <- setNames(data.frame(matrix(ncol = 6, nrow = 1)), colnames(df2))
  header1$Bin[1] <- name
 
  header1$`Dependent measures` <- ""
  header1$numDF <- ""
  header1$denDF <- ""
  header1$F.value <- ""
  header1$p.value <- ""
  
  currdf20 <- rbind(header1, df2)
  
  colnames(currdf20)[2] <- "df"
  colnames(currdf20)[3] <- "Error"
  colnames(currdf20)[4] <- "F"
  colnames(currdf20)[5] <- "p"
  

  currdf20 <- currdf20[c("Bin", "Dependent measures", "df", "Error", "F", "p")]
  
  return(currdf20)
}

norm_to_Dim <- function(data){
  
  #Normalize to dim light
  
  library(tidyr)
  trial2 <- subset(data, trial == 2)
  trial2$date <- NULL
  trial2$session <- NULL
  trial2$Value <- NULL
  trial2$percent_used <- NULL
  trial2$sub_trial_id <- NULL
  trial2$sub_session_id <- NULL
  trial2$UsedDataCount <- NULL
  trial2$TotalDataCount <- NULL
  trial2.2 <- spread(trial2, key = c("light_level"), value = "ValueNorm1")
  
  trial2.2$high <- trial2.2$high/trial2.2$dim
  trial2.2$medium <- trial2.2$medium/trial2.2$dim
  trial2.2$low <- trial2.2$low/trial2.2$dim
  trial2.2$dim <- NULL
  
  trial2.3 <- melt(trial2.2, id = c("subject", "color", "group","Channel", "daynight", "trial"))
  
  #Normalize to dim light
  
  trial3 <- subset(data, trial == 3)
  trial3$date <- NULL
  trial3$session <- NULL
  trial3$Value <- NULL
  trial3$percent_used <- NULL
  trial3$sub_trial_id <- NULL
  trial3$sub_session_id <- NULL
  trial3$UsedDataCount <- NULL
  trial3$TotalDataCount <- NULL
  trial3.2 <- spread(trial3, key = c("light_level"), value = "ValueNorm1")
  
  trial3.2$high <- trial3.2$high/trial3.2$dim
  trial3.2$medium <- trial3.2$medium/trial3.2$dim
  trial3.2$low <- trial3.2$low/trial3.2$dim
  trial3.2$dim <- NULL
  
  trial3.3 <- melt(trial3.2, id = c("subject", "color", "group","Channel", "daynight", "trial"))
  
  ValueNorm1_norm_dim <- rbind(trial2.3, trial3.3)
  colnames(ValueNorm1_norm_dim)[7] <- "light_level"
  colnames(ValueNorm1_norm_dim)[8] <- "ValueNorm1"
  return(ValueNorm1_norm_dim)
}

norm_to_Dim2 <- function(data){
  
  #ValueNorm1alize to dim light
  
  library(tidyr)
  trial2 <- subset(data, trial == 2)
  trial2$date <- NULL
  trial2$session <- NULL
  trial2$Value <- NULL
  trial2$percent_used <- NULL
  trial2$sub_trial_id <- NULL
  trial2$sub_session_id <- NULL
  trial2$UsedDataCount <- NULL
  trial2$TotalDataCount <- NULL
  trial2$`%UsedData` <- NULL
  trial2$month <- NULL
  
  trial2.2 <- spread(trial2, key = c("light_level"), value = "ValueNorm1")
  
  trial2.2$h <- trial2.2$h/trial2.2$d
  trial2.2$m <- trial2.2$m/trial2.2$d
  trial2.2$l <- trial2.2$l/trial2.2$d
  trial2.2$d <- NULL
  
  trial2.3 <- melt(trial2.2, id = c("subject", "color", "group","Channel", "daynight", "trial"))
  
  #ValueNorm1alize to dim light
  
  trial3 <- subset(data, trial == 3)
  trial3$date <- NULL
  trial3$session <- NULL
  trial3$Value <- NULL
  trial3$percent_used <- NULL
  trial3$sub_trial_id <- NULL
  trial3$sub_session_id <- NULL
  trial3$UsedDataCount <- NULL
  trial3$TotalDataCount <- NULL
  trial3$`%UsedData` <- NULL
  trial3$month <- NULL
  trial3.2 <- spread(trial3, key = c("light_level"), value = "ValueNorm1")
  
  trial3.2$h <- trial3.2$h/trial3.2$d
  trial3.2$m <- trial3.2$m/trial3.2$d
  trial3.2$l <- trial3.2$l/trial3.2$d
  trial3.2$d <- NULL
  
  trial3.3 <- melt(trial3.2, id = c("subject", "color", "group","Channel", "daynight", "trial"))
  
  ValueNorm1_ValueNorm1_dim <- rbind(trial2.3, trial3.3)
  colnames(ValueNorm1_ValueNorm1_dim)[7] <- "light_level"
  colnames(ValueNorm1_ValueNorm1_dim)[8] <- "ValueNorm1"
  return(ValueNorm1_ValueNorm1_dim)
}
output_mixed_model2 <- function(data, time, doc, Save, UseCompleteOnly){
  title1 <- paste(time, "All data normalized to dim", sep = " ")
  data$ValueNorm1NormD <- as.numeric(data$ValueNorm1NormD )
  data <- subset(data, !is.na(ValueNorm1NormD) & is.finite(ValueNorm1NormD) & trial != 1 & light_level != "d") 
  
  if(UseCompleteOnly){
    numSubs <- data.frame(table(data$Subject))
    numSubs <- numSubs[numSubs$Freq == 48,]
    
    data <- data[data$Subject %in% numSubs$Var1, ]
  }
  
  
  numSubs2 <- data.frame(table(data$subject, data$color,  data$light_level))
  
  numSubs000 <- data.frame(table(data$subject, data$color,  data$light_level, data$trial))
  
  
  numSubs2$Freq <- ifelse(numSubs2$Freq == 12, "COMPLETED", 
                          ifelse(numSubs2$Freq == 0, "MISSING", numSubs2$Freq))
  
  
  
  numSubs000 <- data.frame(table(data$subject, data$color))
  
  
  numSubs000$Freq <- ifelse(numSubs000$Freq == 48, "COMPLETED",  "MISSING")
  
  
  
  
  
  if(FALSE){
    
    
    MyFTable000 = vanilla.table( data = numSubs000 )
    
    textProp <- textProperties()
    
    MyFTable22 = vanilla.table( data = numSubs2 )
    
    textProp <- textProperties()
    
    doc = addFlexTable(doc, MyFTable000)
    doc = addFlexTable(doc, MyFTable22)
    
    
  }
  
  
  
  if(time == "both"){
    #data <- norm_to_Dim(data)

    theta <- subset(data, Channel == "theta" )
    atheta <- subset(data, Channel == "atheta" )
    alpha <- subset(data, Channel == "alpha" )
    halpha <- subset(data, Channel == "halpha")
    hbeta <- subset(data, Channel == "hbeta" )
    beta <- subset(data, Channel == "beta")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1NormD ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1NormD ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1NormD ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1NormD ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1NormD ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1NormD ~ color*light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){
    #data <- norm_to_Dim(data)

    
    data <- subset(data, !is.na(ValueNorm1NormD) & is.finite(ValueNorm1NormD))
    theta <- subset(data, Channel == "theta" & daynight == "day" & trial != 1)
    atheta <- subset(data, Channel == "atheta"  & daynight == "day" & trial != 1 )
    alpha <- subset(data, Channel == "alpha"   & daynight == "day" & trial != 1)
    halpha <- subset(data, Channel == "halpha"  & daynight == "day" & trial != 1)
    hbeta <- subset(data, Channel == "hbeta"  & daynight == "day" & trial != 1)
    beta <- subset(data, Channel == "beta" & daynight == "day" & trial != 1)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){
   # data <- norm_to_Dim(data)

    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & daynight == "n" & trial != 1)
    atheta <- subset(data, Channel == "atheta"  & daynight == "n"  & trial != 1)
    alpha <- subset(data, Channel == "alpha"   & daynight == "n" & trial != 1)
    halpha <- subset(data, Channel == "halpha"  & daynight == "n" & trial != 1)
    hbeta <- subset(data, Channel == "hbeta"  & daynight == "n" & trial != 1)
    beta <- subset(data, Channel == "beta" & daynight == "n" & trial != 1)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1NormD ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  aov_theta <- anova(theta_model)
  aov_atheta <- anova(atheta_model)
  aov_alpha <- anova(alpha_model)
  aov_halpha <- anova(halpha_model)
  aov_beta <- anova(beta_model)
  aov_hbeta <- anova(hbeta_model)
  
  rsq_theta <- r.squaredGLMM(theta_model)
  rsq_atheta <- r.squaredGLMM(atheta_model)
  rsq_alpha <- r.squaredGLMM(alpha_model)
  rsq_halpha <- r.squaredGLMM(halpha_model)
  rsq_beta <- r.squaredGLMM(beta_model)
  rsq_hbeta <- r.squaredGLMM(hbeta_model)
  
  
  if(Save){
    
    aov_theta2 <- addheader(aov_theta, "Theta")
    aov_atheta2 <- addheader(aov_atheta, "Alpha-theta")
    aov_alpha2 <- addheader(aov_alpha, "Alpha")
    aov_halpha2 <- addheader(aov_halpha, "High alpha")
    aov_beta2 <- addheader(aov_beta, "Beta")
    aov_hbeta2 <- addheader(aov_hbeta, "High beta")
    
    
    
    doc = addTitle( doc, title1, level = 1 )
    
    final_df <- rbind(aov_theta2, aov_atheta2, aov_alpha2, aov_halpha2, aov_beta2, aov_hbeta2)
    
    
    MyFTable = vanilla.table( data = final_df )
    
    textProp <- textProperties()
    
    MyFTable[as.numeric(substr(final_df$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
    
    doc = addFlexTable(doc, MyFTable)
    
    
  }
  
  output_list <- list(rsq_theta, aov_theta, rsq_atheta, aov_atheta, rsq_alpha, aov_alpha, rsq_halpha, aov_halpha, rsq_beta, aov_beta, rsq_hbeta, aov_hbeta )
  
  return(output_list)
}


output_mixed_model_single_spectrum <- function(data, time, spectrum, doc, Save, UseCompleteOnly){
  title1 <- paste(time, spectrum, sep = " ")
  data <- subset(data, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1 & color == spectrum)
  
  numSubs <- data.frame(table(data$subject))
  
  if(UseCompleteOnly){
    numSubs <- numSubs[numSubs$Freq == 48,]
    
    data <- data[data$subject %in% numSubs$Var1, ]
  }
  
  numSubs$Standing <- ifelse(numSubs$Freq == 48, "COMPLETE", "MISSING")
  numSubs$Freq <- NULL
  colnames(numSubs)[1] <- "Subject"
  
  
  data$subject <- factor(data$subject)
  numSubs2 <- data.frame(table(data$subject, data$color,  data$light_level))
  
  numSubs000 <- data.frame(table(data$subject, data$color,  data$light_level, data$trial))
  
  
  numSubs2$Freq <- ifelse(numSubs2$Freq == 12, "COMPLETED", 
                          ifelse(numSubs2$Freq == 0, "MISSING", numSubs2$Freq))
  
  
  
  numSubs000 <- data.frame(table(data$subject, data$color))
  
  
  numSubs000$Freq <- ifelse(numSubs000$Freq == 48, "COMPLETED",  "MISSING")
  
  
  
  
  


  
  if(time == "both"){

    theta <- subset(data, Channel == "theta" & color == spectrum)
    atheta <- subset(data, Channel == "atheta" & color == spectrum )
    alpha <- subset(data, Channel == "alpha" & color == spectrum)
    halpha <- subset(data, Channel == "halpha"& color == spectrum)
    hbeta <- subset(data, Channel == "hbeta" & color == spectrum)
    beta <- subset(data, Channel == "beta"& color == spectrum)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1 ~ light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1 ~ light_level*trial + daynight , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1 ~ light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1 ~ light_level*trial + daynight , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1 ~ light_level*trial + daynight , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1 ~ light_level*trial + daynight , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){

    
    theta <- subset(data, Channel == "theta" & daynight == "day" & color == spectrum & trial != 1)
    atheta <- subset(data, Channel == "atheta"  & daynight == "day" & color == spectrum & trial != 1)
    alpha <- subset(data, Channel == "alpha"   & daynight == "day" & color == spectrum & trial != 1)
    halpha <- subset(data, Channel == "halpha"  & daynight == "day" & color == spectrum & trial != 1)
    hbeta <- subset(data, Channel == "hbeta"  & daynight == "day" & color == spectrum & trial != 1)
    beta <- subset(data, Channel == "beta" & daynight == "day" & color == spectrum & trial != 1)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){

    
    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & daynight == "n" & color == spectrum & trial != 1)
    atheta <- subset(data, Channel == "atheta"  & daynight == "n" & color == spectrum & trial != 1 )
    alpha <- subset(data, Channel == "alpha"   & daynight == "n" & color == spectrum & trial != 1)
    halpha <- subset(data, Channel == "halpha"  & daynight == "n" & color == spectrum & trial != 1)
    hbeta <- subset(data, Channel == "hbeta"  & daynight == "n" & color == spectrum & trial != 1)
    beta <- subset(data, Channel == "beta" & daynight == "n" & color == spectrum & trial != 1)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(ValueNorm1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  aov_theta <- anova(theta_model)
  aov_atheta <- anova(atheta_model)
  aov_alpha <- anova(alpha_model)
  aov_halpha <- anova(halpha_model)
  aov_beta <- anova(beta_model)
  aov_hbeta <- anova(hbeta_model)
  
  rsq_theta <- r.squaredGLMM(theta_model)
  rsq_atheta <- r.squaredGLMM(atheta_model)
  rsq_alpha <- r.squaredGLMM(alpha_model)
  rsq_halpha <- r.squaredGLMM(halpha_model)
  rsq_beta <- r.squaredGLMM(beta_model)
  rsq_hbeta <- r.squaredGLMM(hbeta_model)
  
  output_list <- list(rsq_theta, aov_theta, rsq_atheta, aov_atheta, rsq_alpha, aov_alpha, rsq_halpha, aov_halpha, rsq_beta, aov_beta, rsq_hbeta, aov_hbeta )
  
  if(Save){
    
    aov_theta2 <- addheader(aov_theta, "Theta")
    aov_atheta2 <- addheader(aov_atheta, "Alpha-theta")
    aov_alpha2 <- addheader(aov_alpha, "Alpha")
    aov_halpha2 <- addheader(aov_halpha, "High alpha")
    aov_beta2 <- addheader(aov_beta, "Beta")
    aov_hbeta2 <- addheader(aov_hbeta, "High beta")
    
    
    
    doc = addTitle( doc, title1, level = 1 )
    
    
    
    MyFTable000 = vanilla.table( data = numSubs000 )
    
    textProp <- textProperties()
    
    MyFTable22 = vanilla.table( data = numSubs2 )
    
    textProp <- textProperties()
    
    #doc = addFlexTable(doc, MyFTable000)
    #doc = addFlexTable(doc, MyFTable22)
    
    
    final_df <- rbind(aov_theta2, aov_atheta2, aov_alpha2, aov_halpha2, aov_beta2, aov_hbeta2)
    
    
    MyFTable = vanilla.table( data = final_df )
    
    textProp <- textProperties()
    
    MyFTable[as.numeric(substr(final_df$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
    
    doc = addFlexTable(doc, MyFTable)
    
    
  }
  
  
  return(output_list)
}
print_mixed_model <- function(model_list){
  
  print(model_list[1])
  print("Theta----------------")
  print(model_list[2])
  print(anova(model_list[3]))
  
  print("Alpha-theta----------")
  print(model_list[4])
  print(anova(model_list[5]))
  
  print("Alpha----------------")
  print(model_list[6])
  print(anova(model_list[7]))
  
  print("High Alpha-----------")
  print(model_list[8])
  print(anova(model_list[9]))
  
  print("Beta-----------------")
  print(model_list[10])
  print(anova(model_list[11]))
  
  print("High Beta------------")
  print(model_list[12])
  print(anova(model_list[13]))
  
}


performTtests <- function(Data, doc0, x_str, Correction){
  
  library(stringr)
  
  
  Data$condition <- as.factor(Data$condition)
  ComparedGroups <- c()
  df <- c()
  t <- c()
  p <- c()
  
  colnames(Data)[2] <- "condition"
  if(x_str == "Light level"){
    
    Data$condition <- factor(Data$condition, levels = c("high" , "medium", "low", "dim" ))
    
  }
  #get comparisons
  testModel<- lme(value ~ condition, random = ~1|subject,
                  data=Data)
  
  df5 <- lsmeans(testModel, pairwise~ condition, adjust=Correction, data = Data)
  lsmeansComparisons3 <- data.frame(summary(df5)[2])
  comparistonList <- strsplit(as.character(lsmeansComparisons3$contrasts.contrast), " - ")
  ###
  
  for(i in 1:length(comparistonList)){
    orignialData <- Data[Data$condition == comparistonList[[i]][1] | Data$condition == comparistonList[[i]][2] ,]
    
    subList001 <- intersect(Data[Data$condition == comparistonList[[i]][1],]$subject , Data[Data$condition == comparistonList[[i]][2],]$subject)
    cleanedData <- orignialData[orignialData$subject %in% subList001, ]
    
    curr_Ttest <- t.test(value ~ condition, data = cleanedData, paired =  TRUE)
    ComparedGroups <- c(ComparedGroups, paste(comparistonList[[i]][1], comparistonList[[i]][2], sep = " - "  ))
    df <- c(df, as.numeric(curr_Ttest$parameter))
    t <- c(t, as.numeric(curr_Ttest$statistic))
    p <- c(p, curr_Ttest$p.value)
    
  }
  t_test_table <- data.frame(ComparedGroups, df, t, p )
  t_test_table$p <- as.numeric(as.character(t_test_table$p))
  t_test_table2 <- t_test_table
  ###add p-value-correction here
  ###
  t_test_table$t <- as.numeric(as.character(t_test_table$t))
  t_test_table$p <- ifelse(round(t_test_table$p, digits = 3) == 1, "1", ifelse(round(t_test_table$p, digits = 3) < .05, paste(substr(as.character(sprintf("%.3f", round(t_test_table$p, digits = 3))), 2, 5), "*", sep = " ") ,substr(as.character(sprintf("%.3f", round(t_test_table$p, digits = 3))), 2, 5) ) )
  
  t_test_table$t <-  ifelse(!is.na(t_test_table$t), as.character(round(t_test_table$t, digits = 3)), t_test_table$t)
  
  removeDim <- FALSE
  if(removeDim){
    
    t_test_table$number<- str_count(t_test_table$ComparedGroups, "Dim")
    
    t_test_table <- subset(t_test_table, number < 2)
    t_test_table$number <- NULL
  }
  
  
  print(t_test_table2)
  
  textProp <- textProperties()
  
  sigFTable2 = vanilla.table( data = t_test_table )
  sigFTable2[as.numeric(substr(t_test_table$p, 1, 4)) < .05] = chprop( textProp, font.weight = "bold") 
  
  
  #doc = addParagraph( doc0, "comparison method: t.test", stylename = "BulletList" )
  
  doc0 = addFlexTable(doc0, sigFTable2)
  
  
  ##Summarize data and graph
  
  summarized_data <- summarySE(measurevar = "value", groupvars = "condition", data = Data)
  
  gg <- ggplot(summarized_data, aes(x = condition, y = value))+
    geom_bar(position=position_dodge(), stat="identity", colour =  "black") +
    geom_errorbar(aes(ymin=value-se, ymax=value+se),
                  width=.2,                    
                  position=position_dodge(.9))+
    labs(x=x_str) +
    theme(axis.title.y=element_text(vjust=2), legend.title=element_blank(), legend.position="bottom") +
    theme(legend.title=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+ 
    theme(legend.position="none")
  
  sig_start <- max(summarized_data$value) 
  sig_inter <- max(summarized_data$se) + (.5*max(summarized_data$se)) 
  
  sig_nifComp <- t_test_table2[t_test_table2$p < .05,]
  if(length(sig_nifComp$ComparedGroups) > 0){
    for(i in 1:length(sig_nifComp$ComparedGroups)){
      comparison_group <- strsplit(as.character(sig_nifComp$ComparedGroups[i]), " - ")[[1]]
      
      gg <- gg + geom_signif(comparisons = list(c(comparison_group[1], comparison_group[2])), annotations=substr(as.character(sprintf("%.3f", round(sig_nifComp$p[i], digits = 3))), 2, 5), y_position = sig_start + (i*sig_inter))
      
    }
  }
  
  if(x_str == "Light"){
    
    gg <- gg +  scale_fill_manual(values=c("red4", "deepskyblue4",  "gray80" )) 
    
  }
  if(x_str == "Caffeine"){
    gg <- gg +  scale_fill_manual(values=c( "gray100",  "gray40")) 
    
  }
  #doc0 = addParagraph( doc0, "Athethmetic means", stylename = "BulletList" )
  doc0 <- addPlot(doc = doc0, fun = print, x = gg, vector.graphic = TRUE, width = 4, height = 3)
  
  
}
