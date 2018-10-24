source("C:/Users/roohac/Documents/GitHub/ONR-Spectral-Sensitivity-Function-/EEG-LMEM-functions.R")
library(ggsignif)
library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(ReporteRs)
library(magrittr)

UpdatedEEG_5_10_18<- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/total-months/180sec_threshold_100/longFormatTableNorm1NormD_2018_05_10_16_34.xlsx")
colnames(UpdatedEEG_5_10_18)[6] <- "light_level"
UpdatedEEG_5_10_18$light_level <- ifelse(UpdatedEEG_5_10_18$light_level =="d", "dim", ifelse(UpdatedEEG_5_10_18$light_level =="m", "medium", ifelse(UpdatedEEG_5_10_18$light_level =="l", "low","high" )) )
UpdatedEEG_5_10_18$sub_t_id <- paste(UpdatedEEG_5_10_18$subject, UpdatedEEG_5_10_18$light_level, UpdatedEEG_5_10_18$trial, sep = "_")
UpdatedEEG_5_10_18$sub_c <- paste(UpdatedEEG_5_10_18$subject, UpdatedEEG_5_10_18$light_level, sep = "_")
UpdatedEEG_5_10_18$sub_t <- paste(UpdatedEEG_5_10_18$subject, UpdatedEEG_5_10_18$session, sep = "_")

UpdatedEEG_5_10_18_cropped <- subset(UpdatedEEG_5_10_18, subject != 102 & subject != 108 & subject != 125 
                                     & subject != 137 & subject != 111 & subject != 115 & subject != 118 & subject != 126 
                                     & subject != 122 & subject != 117 & subject != 140 & subject != 142 & subject != 170
                                     & subject != 171 & subject != 174 & subject != 162 & subject != 168 & subject != 119 & subject != 156
                                     & sub_c != "134_high" & sub_c != "169_low" & sub_c != "155_medium" & sub_t_id != "167_high_3" & sub_t_id != "144_low_3" 
                                     & sub_t != "138_1" & sub_t != "143_1" & sub_t != "145_1" & sub_t != "147_1" & sub_t != "148_1" & sub_t != "151_1" 
                                     & sub_t != "152_1")

UpdatedEEG_5_10_18_cropped <- subset(UpdatedEEG_5_10_18, subject != 102 & subject != 108 & subject != 125 
                                     & subject != 137 & subject != 111 & subject != 115 & subject != 118 & subject != 126 
                                     & subject != 122 & subject != 117 & subject != 140 & subject != 142 & subject != 170
                                     & subject != 171 & subject != 174 & subject != 162 & subject != 168 & subject != 119 & subject != 156)



UpdatedEEG_5_10_18_cropped <- subset(UpdatedEEG_5_10_18, subject != 102 & subject != 108 & subject != 125 
                                     & subject != 137 & subject != 111 & subject != 115 & subject != 118 & subject != 126 
                                     & subject != 122 & subject != 117 & subject != 140 & subject != 142 & subject != 170
                                     & subject != 171 & subject != 174 & subject != 162 & subject != 168 & subject != 119 & subject != 156
                                     & sub_c != "134_high" & sub_c != "169_low" & sub_c != "155_medium" & sub_t_id != "167_high_3" & sub_t_id != "144_low_3"
                                     & sub_t_id != "103_medium_3" & sub_t_id != "103_high_2" & sub_t_id != "127_dim_3")


UpdatedEEG_5_10_18_cropped <- subset(UpdatedEEG_5_10_18_cropped, trial != 1)
if(FALSE){
  missingTrials <- setdiff(UpdatedEEG_5_10_18_cropped$sub_t_id, EEG_dataCropped_1_29_18$sub_t_id)
  subs001 <- substr(missingTrials, 1, 3)
  cond001 <- substr(missingTrials, 5, 5)
  
  missINgsUbs <- data.frame(subs001, cond001, missingTrials)
  
  missINgsUbs2 <- missINgsUbs[1:12,]
  
}




###Create doc 
doc = docx()


UsedDate <- UpdatedEEG_5_10_18_cropped
#colnames(UpdatedEEG_3_11_18)[9] <- "ValueNorm1"
#colnames(UpdatedEEG_3_11_18)[16] <- "daynight"

model_list1 <- output_mixed_model1(UsedDate, "night", doc, TRUE, FALSE)
model_list2 <- output_mixed_model2(UsedDate, "night", doc, TRUE, FALSE)









##Looking at each indivudual spectra
model_list5 <- output_mixed_model_single_spectrum(UsedDate, "night", "r", doc, TRUE, FALSE)
model_list7 <- output_mixed_model_single_spectrum(UsedDate, "night", "b", doc, TRUE, FALSE)
model_list6 <- output_mixed_model_single_spectrum(UsedDate, "night", "g", doc, TRUE, FALSE)

model_list8 <- output_mixed_model_single_spectrum(UsedDate, "night", "c", doc, TRUE, FALSE)
model_list9 <- output_mixed_model_single_spectrum(UsedDate, "night", "a", doc, TRUE, FALSE)



###Save doc
dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/STATS-OUTPUT/"
filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "ONR-SSF-EEG.docx")
writeDoc( doc, file = filename )




#posthoc tests




###Create doc 
doc2 = docx()



#main model not normalized to Dim

doc2 <- addTitle( doc2, "Main model not normalized to dim", level = 1 )

UsedDate

eeg_001 <- subset(UsedDate, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1)

  
  #a-theta
  atheta_001 <- subset(eeg_001, Channel == "atheta" & trial != 1 )
  
  atheta_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = atheta_001, FUN = mean)
  
  colnames(atheta_001_subjects)[2] <- "condition"
  colnames(atheta_001_subjects)[3] <- "value"
  
  
  doc = addParagraph( doc2, "alpha-theta - light level", stylename = "Normal" )
  
  performTtests(atheta_001_subjects, doc2, "Light level", "None")
  
  
  
  
  
  atheta_002_subjects <- aggregate(ValueNorm1 ~ subject + trial, data = atheta_001, FUN = mean)
  
  colnames(atheta_002_subjects)[2] <- "condition"
  colnames(atheta_002_subjects)[3] <- "value"
  
  
  doc2 = addParagraph( doc2, "alpha-theta - trial", stylename = "Normal" )
  
  performTtests(atheta_002_subjects, doc2, " trial", "None")

  
  #Alpha
  alpha_001 <- subset(eeg_001, Channel == "alpha" & trial != 1 )


  if(FALSE){
    test001_model <- lme(ValueNorm1 ~ light_level * trial, random = ~trial|subject/light_level, 
                           data = Rtheta_001)
    
    anova(test001_model)
    
    ezANOVA(data=Rtheta_001, 
            dv=ValueNorm1, 
            wid=.(subject), 
            within=.(light_level, trial)) 
    
    summary(test002_model)
    
    Rtheta_001_subjects <- aggregate(ValueNorm1 ~ subject + trial, data = Rtheta_001, FUN = mean)
    
    
    t.test( ValueNorm1 ~ trial, data = Rtheta_001_subjects, paired = TRUE)
    
    
    
    
    #test 2
    
    test003_model <- aov(ValueNorm1 ~ light_level * trial + Error(subject/(light_level * trial)), data=alpha_002)
    summary(test003_model)
    
    library(ez) 
    
    ezANOVA(data=alpha_002, 
                      dv=ValueNorm1, 
                      wid=.(subject), 
                      within=.(light_level, trial)) 
    
    
    test004_model <- lme(ValueNorm1 ~ light_level * trial, random = ~1|subject/light_level/trial, 
                         data = alpha_002)
    
    anova(test004_model)
    
  }

  
  
  
  alpha_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = alpha_001, FUN = mean)
  colnames(alpha_001_subjects)[2] <- "condition"
  colnames(alpha_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "alpha - light level", stylename = "Normal" )
  
  performTtests(alpha_001_subjects, doc2, "Light level", "None")
  
  
  
  
  numSubs <- data.frame(table(alpha_001$subject))
  numSubs <- numSubs[numSubs$Freq == 8,]
  
  alpha_002 <- alpha_001[alpha_001$subject %in% numSubs$Var1, ]
  
  
  alpha_002_subjects <- aggregate(ValueNorm1 ~ subject + trial, data = alpha_002, FUN = mean)
  
  colnames(alpha_002_subjects)[2] <- "condition"
  colnames(alpha_002_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "alpha - trial", stylename = "Normal" )
  

  
  performTtests(alpha_002_subjects, doc2, "Trial", "None")
  
  
  
  #high alpha 
  halpha_001 <- subset(eeg_001, Channel == "halpha" & trial != 1 )
  
  halpha_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = halpha_001, FUN = mean)
  
  colnames(halpha_001_subjects)[2] <- "condition"
  colnames(halpha_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "High alpha - light level", stylename = "Normal" )
  
  performTtests(halpha_001_subjects, doc2, "Light level", "None")
  
  




#main model for Red

doc2 <- addTitle( doc2, "Red", level = 1 )

Red_eeg_001 <- subset(UsedDate, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1 & color == "r")


  ###red theta
  Rtheta_001 <- subset(Red_eeg_001, Channel == "theta" & trial != 1 )
  
  numSubs <- data.frame(table(Rtheta_001$subject))
  numSubs <- numSubs[numSubs$Freq == 8,]
  
  Rtheta_002 <- Rtheta_001[Rtheta_001$subject %in% numSubs$Var1, ]
  

  Rtheta_001_subjects <- aggregate(ValueNorm1 ~ subject + trial, data = Rtheta_002, FUN = mean)

  colnames(Rtheta_001_subjects)[2] <- "condition"
  colnames(Rtheta_001_subjects)[3] <- "value"

  doc2 = addParagraph( doc2, "Theta - trial", stylename = "Normal" )

  performTtests(Rtheta_001_subjects, doc2, "Trial", "None")


  ###red A-theta
  Ratheta_001 <- subset(Red_eeg_001, Channel == "atheta" & trial != 1 )
  
  numSubs <- data.frame(table(Ratheta_001$subject))
  numSubs <- numSubs[numSubs$Freq == 8,]
  
  Ratheta_002 <- Ratheta_001[Ratheta_001$subject %in% numSubs$Var1, ]
  
  
  Ratheta_001_subjects <- aggregate(ValueNorm1 ~ subject + trial, data = Ratheta_002, FUN = mean)
  
  colnames(Ratheta_001_subjects)[2] <- "condition"
  colnames(Ratheta_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "Alpha-Theta - trial", stylename = "Normal" )
  
  performTtests(Ratheta_001_subjects, doc2, "Trial", "None")
  
  
  
  ###red alpha
  Ralpha_001 <- subset(Red_eeg_001, Channel == "alpha" & trial != 1 )
  
  Ralpha_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = Ralpha_001, FUN = mean)
  
  colnames(Ralpha_001_subjects)[2] <- "condition"
  colnames(Ralpha_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "Alpha - light level", stylename = "Normal" )
  
  performTtests(Ralpha_001_subjects, doc2, "Light level", "None")
  
  
  
  ###red high alpha
  Rhalpha_001 <- subset(Red_eeg_001, Channel == "halpha" & trial != 1 )
  
  Rhalpha_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = Rhalpha_001, FUN = mean)
  
  colnames(Rhalpha_001_subjects)[2] <- "condition"
  colnames(Rhalpha_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "High-Alpha - light level", stylename = "Normal" )
  
  performTtests(Rhalpha_001_subjects, doc2, "Light level", "None")

  
  
  
  




#main model for Blue

doc2 <- addTitle( doc2, "Blue", level = 1 )

Blue_eeg_001 <- subset(UsedDate, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1 & color == "b")


  ###Blue alpha
  Balpha_001 <- subset(Blue_eeg_001, Channel == "alpha" & trial != 1 )
  
  Balpha_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = Balpha_001, FUN = mean)
  
  colnames(Balpha_001_subjects)[2] <- "condition"
  colnames(Balpha_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "Alpha - light level", stylename = "Normal" )
  
  performTtests(Balpha_001_subjects, doc2, "Light level", "None")
  
  
  
  ###Blue high alpha
  Bhalpha_001 <- subset(Blue_eeg_001, Channel == "halpha" & trial != 1 )
  
  Bhalpha_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = Bhalpha_001, FUN = mean)
  
  colnames(Bhalpha_001_subjects)[2] <- "condition"
  colnames(Bhalpha_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "High-Alpha - light level", stylename = "Normal" )
  
  performTtests(Bhalpha_001_subjects, doc2, "Light level", "None")





#main model for Green

doc2 <- addTitle( doc2, "Green", level = 1 )

Green_eeg_001 <- subset(UsedDate, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1 & color == "g")



  ###Green alpha-theta
  G_atheta_001 <- subset(Green_eeg_001, Channel == "atheta" & trial != 1 )
  
  g_atheta_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = G_atheta_001, FUN = mean)
  
  colnames(g_atheta_001_subjects)[2] <- "condition"
  colnames(g_atheta_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "Alpha-theta - light level", stylename = "Normal" )
  
  performTtests(g_atheta_001_subjects, doc2, "Light level", "None")
  
  
  
  ###Green alpha
  G_alpha_001 <- subset(Green_eeg_001, Channel == "alpha" & trial != 1 )
  
  g_alpha_001_subjects <- aggregate(ValueNorm1 ~ subject + light_level, data = G_alpha_001, FUN = mean)
  
  colnames(g_alpha_001_subjects)[2] <- "condition"
  colnames(g_alpha_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "Alpha - light level", stylename = "Normal" )
  
  performTtests(g_alpha_001_subjects, doc2, "Light level", "None")




#main model for Cyan

doc2 <- addTitle( doc2, "Cyan", level = 1 )

Cyan_eeg_001 <- subset(UsedDate, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1 & color == "c")


  ###Cyan alpha-theta
  C_atheta_001 <- subset(Cyan_eeg_001, Channel == "atheta" & trial != 1 )
  
  
  numSubs <- data.frame(table(C_atheta_001$subject))
  numSubs <- numSubs[numSubs$Freq == 8,]
  
  C_atheta_002 <- C_atheta_001[C_atheta_001$subject %in% numSubs$Var1, ]
  
  C_atheta_001_subjects <- aggregate(ValueNorm1 ~ subject + trial, data = C_atheta_002, FUN = mean)
  
  colnames(C_atheta_001_subjects)[2] <- "condition"
  colnames(C_atheta_001_subjects)[3] <- "value"
  
  doc2 = addParagraph( doc2, "Alpha-theta - trial", stylename = "Normal" )
  
  performTtests(C_atheta_001_subjects, doc2, "trial", "None")


#main model for Amber

doc2 <- addTitle( doc2, "Amber", level = 1 )

Amber_eeg_001 <- subset(UsedDate, !is.na(ValueNorm1) & is.finite(ValueNorm1) & trial != 1 & color == "a")







###Save doc
dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/STATS-OUTPUT/"
filename <- paste0(dir,format(Sys.time(), "%Y-%m-%d_%H%M%S_"), "ONR-SSF-EEG-PostHoc.docx")
writeDoc( doc2, file = filename )





####
###Daytime 
if(FALSE){
  
  daytime_eeg <- read_excel("//root/projects/EEG-Daytime-ONR-2017/processed-data/month1&2/cropped-longFormatTable_2018_04_11_14_53.xlsx", 
                            col_types = c("text", "text", "numeric", 
                                          "text", "text", "numeric", "date", 
                                          "text", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))
  daytime_eeg$TimeOfDay <- "day"
  
  
  daytime_eeg$light_level <- factor(daytime_eeg$light_level, levels = c("h", "m", "l", "d"))
  daytime_eeg$Channel <- factor(daytime_eeg$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta"))
  daytime_eeg$subject <- as.factor(daytime_eeg$subject)
  daytime_eeg$color <- as.factor(daytime_eeg$color)
  daytime_eeg$Channel <- as.factor(daytime_eeg$Channel)
  daytime_eeg$trial <- as.factor(daytime_eeg$trial)
  
  
  colnames(daytime_eeg)[10] <- "ValueNorm1"
  
  daytime_eeg <- daytime_eeg[daytime_eeg$trial != 1,]
  
  model_list10 <- output_mixed_model1(daytime_eeg, "day")
  model_list11 <- output_mixed_model2(daytime_eeg, "day")
  
  
  
  print("All data: daytime, specifed to look at light level")
  print_mixed_model(model_list10)
  
  print("All data: daytime, specifed to look at spectra")
  print_mixed_model(model_list11)
  
  
  
  
  ##Looking at each indivudual spectra
  model_list12 <- output_mixed_model_single_spectrum(daytime_eeg, "day", "red")
  model_list13 <- output_mixed_model_single_spectrum(daytime_eeg, "day", "blue")
  
  
  print("Red: daytime 3min")
  print_mixed_model(model_list12)
  print("Blue: daytime 3min")
  print_mixed_model(model_list13)
  
  
}













wordTableGraphGenerator(model_list1, model_list2, model_list3, model_list4, model_list5, model_list6, model_list7, model_list8, model_list9)






if(FALSE){
  EEG_dataCropped_1_29_18 <- read_csv("//root/projects/EEG-Nighttime-ONR-2017/data-sets-EEGpower/EEG-dataCropped-1-29-18.csv")
  
  
  month5_EEG_data <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/month5/Month5normalised.xlsx")
  month5_EEG_data$norm_t1 <- as.numeric(month5_EEG_data$norm_t1 )
  colnames(month5_EEG_data)[4] <- "light_level"
  month5_EEG_data$light_level <- ifelse(month5_EEG_data$light_level =="d", "dim", ifelse(month5_EEG_data$light_level =="m", "medium", ifelse(month5_EEG_data$light_level =="l", "low","high" )) )
  month5_EEG_data$color <- as.factor(month5_EEG_data$color)
  month5_EEG_data$sub_trial_id <- paste(month5_EEG_data$subject, month5_EEG_data$light_level, month5_EEG_data$trial, sep = "_")
  month5_EEG_data$sub_session_id <- paste(month5_EEG_data$subject, month5_EEG_data$light_level, sep = "_")
  month5_EEG_data$TimeOfDay <- "night"
  
  UpdatedEEG_3_11_18 <- rbind(EEG_dataCropped_1_29_18, month5_EEG_data[month5_EEG_data$trial != 1,])
  
  UpdatedEEG_3_11_18$daynight <- "n"
  
  UpdatedEEG_3_11_18 <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/month5/UpdatedEEG_3_11_18.xlsx")
  
  UpdatedEEG_3_11_18 <- subset(UpdatedEEG_3_11_18, percent_used >= .3 & subject != 158 & sub_session_id != "169_medium" )
  UpdatedEEG_3_11_18$color <- ifelse(UpdatedEEG_3_11_18$color =="red", "r", ifelse(UpdatedEEG_3_11_18$color =="blue", "b", ifelse(UpdatedEEG_3_11_18$color =="green", "g",ifelse(UpdatedEEG_3_11_18$color =="amber", "a",ifelse(UpdatedEEG_3_11_18$color =="cyan", "c","N?A" )) ) )) 
  
  
  UpdatedEEG_3_11_18$light_level <- factor(UpdatedEEG_3_11_18$light_level, levels = c("high", "medium", "low", "dim"))
  UpdatedEEG_3_11_18$Channel <- factor(UpdatedEEG_3_11_18$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta"))
  UpdatedEEG_3_11_18$subject <- as.factor(UpdatedEEG_3_11_18$subject)
  UpdatedEEG_3_11_18$color <- as.factor(UpdatedEEG_3_11_18$color)
  UpdatedEEG_3_11_18$Channel <- as.factor(UpdatedEEG_3_11_18$Channel)
  UpdatedEEG_3_11_18$trial <- as.factor(UpdatedEEG_3_11_18$trial)
  UpdatedEEG_3_11_18$daynight <- as.factor("n")
  
  
}



##5/23/18 This is the data that is significant


if(FALSE){
  EEG_dataCropped_1_29_18 <- read_csv("//root/projects/EEG-Nighttime-ONR-2017/data-sets-EEGpower/EEG-dataCropped-1-29-18.csv")
  
  
  month5_EEG_data <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/month5/Month5normalised.xlsx")
  month5_EEG_data$norm_t1 <- as.numeric(month5_EEG_data$norm_t1 )
  colnames(month5_EEG_data)[4] <- "light_level"
  month5_EEG_data$light_level <- ifelse(month5_EEG_data$light_level =="d", "dim", ifelse(month5_EEG_data$light_level =="m", "medium", ifelse(month5_EEG_data$light_level =="l", "low","high" )) )
  month5_EEG_data$color <- as.factor(month5_EEG_data$color)
  month5_EEG_data$sub_trial_id <- paste(month5_EEG_data$subject, month5_EEG_data$light_level, month5_EEG_data$trial, sep = "_")
  month5_EEG_data$sub_session_id <- paste(month5_EEG_data$subject, month5_EEG_data$light_level, sep = "_")
  month5_EEG_data$TimeOfDay <- "night"
  
  UpdatedEEG_3_11_18 <- rbind(EEG_dataCropped_1_29_18, month5_EEG_data[month5_EEG_data$trial != 1,])
  
  UpdatedEEG_3_11_18$daynight <- "n"
  

  UpdatedEEG_3_11_18$color <- ifelse(UpdatedEEG_3_11_18$color =="red", "r", ifelse(UpdatedEEG_3_11_18$color =="blue", "b", ifelse(UpdatedEEG_3_11_18$color =="green", "g",ifelse(UpdatedEEG_3_11_18$color =="amber", "a",ifelse(UpdatedEEG_3_11_18$color =="cyan", "c","N?A" )) ) )) 
  
  
  UpdatedEEG_3_11_18$light_level <- factor(UpdatedEEG_3_11_18$light_level, levels = c("high", "medium", "low", "dim"))
  UpdatedEEG_3_11_18$Channel <- factor(UpdatedEEG_3_11_18$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta"))
  UpdatedEEG_3_11_18$subject <- as.factor(UpdatedEEG_3_11_18$subject)
  UpdatedEEG_3_11_18$color <- as.factor(UpdatedEEG_3_11_18$color)
  UpdatedEEG_3_11_18$Channel <- as.factor(UpdatedEEG_3_11_18$Channel)
  UpdatedEEG_3_11_18$trial <- as.factor(UpdatedEEG_3_11_18$trial)
  UpdatedEEG_3_11_18$daynight <- as.factor("n")
  
  
  
  blue1 <- subset(UpdatedEEG_3_11_18, color == "b")
  blue2 <- subset(UpdatedEEG_5_10_18_cropped, color == "b" & trial != 1)
  
  
  setdiff(blue2$sub_t_id, blue1$sub_trial_id)
}



