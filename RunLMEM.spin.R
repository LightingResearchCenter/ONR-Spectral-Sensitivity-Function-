source("C:/Users/roohac/Documents/GitHub/ONR-Spectral-Sensitivity-Function-/EEG-LMEM-functions.R")
library(ggsignif)
library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
if(FALSE){
  EEG_dataCropped_1_29_18 <- read_csv("//root/projects/EEG-Nighttime-ONR-2017/data-sets-EEGpower/EEG-dataCropped-1-29-18.csv")
  
  
  month5_EEG_data <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/Month5normalised.xlsx")
  month5_EEG_data$norm_t1 <- as.numeric(month5_EEG_data$norm_t1 )
  colnames(month5_EEG_data)[4] <- "light_level"
  month5_EEG_data$light_level <- ifelse(month5_EEG_data$light_level =="d", "dim", ifelse(month5_EEG_data$light_level =="m", "medium", ifelse(month5_EEG_data$light_level =="l", "low","high" )) )
  month5_EEG_data$color <- as.factor(month5_EEG_data$color)
  month5_EEG_data$sub_trial_id <- paste(month5_EEG_data$subject, month5_EEG_data$light_level, month5_EEG_data$trial, sep = "_")
  month5_EEG_data$sub_session_id <- paste(month5_EEG_data$subject, month5_EEG_data$light_level, sep = "_")
  month5_EEG_data$TimeOfDay <- "night"
  
  UpdatedEEG_3_11_18 <- rbind(EEG_dataCropped_1_29_18, month5_EEG_data[month5_EEG_data$trial != 1,])
  
}

UpdatedEEG_3_11_18 <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/month5/UpdatedEEG_3_11_18.xlsx")



UpdatedEEG_3_11_18$light_level <- factor(UpdatedEEG_3_11_18$light_level, levels = c("high", "medium", "low", "dim"))
UpdatedEEG_3_11_18$Channel <- factor(UpdatedEEG_3_11_18$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta"))
UpdatedEEG_3_11_18$subject <- as.factor(UpdatedEEG_3_11_18$subject)
UpdatedEEG_3_11_18$color <- as.factor(UpdatedEEG_3_11_18$color)
UpdatedEEG_3_11_18$Channel <- as.factor(UpdatedEEG_3_11_18$Channel)
UpdatedEEG_3_11_18$trial <- as.factor(UpdatedEEG_3_11_18$trial)
UpdatedEEG_3_11_18$TimeOfDay <- as.factor("night")




model_list1 <- output_mixed_model1(UpdatedEEG_3_11_18, "night")
model_list2 <- output_mixed_model2(UpdatedEEG_3_11_18, "night")



print("All data: nighttime, specifed to look at light level")
print_mixed_model(model_list1)

print("All data: nighttime, specifed to look at spectra")
print_mixed_model(model_list2)




##Looking at each indivudual spectra
model_list5 <- output_mixed_model_single_spectrum(UpdatedEEG_3_11_18, "night", "red")
model_list6 <- output_mixed_model_single_spectrum(UpdatedEEG_3_11_18, "night", "green")
model_list7 <- output_mixed_model_single_spectrum(UpdatedEEG_3_11_18, "night", "blue")

model_list8 <- output_mixed_model_single_spectrum(UpdatedEEG_3_11_18, "night", "cyan")
model_list9 <- output_mixed_model_single_spectrum(UpdatedEEG_3_11_18, "night", "amber")

print("Red: nighttime 3min")
print_mixed_model(model_list5)
print("Green: nighttime 3min")
print_mixed_model(model_list6)
print("Blue nighttime: 3min")
print_mixed_model(model_list7)
print("Cyan nighttime: 3min")
print_mixed_model(model_list8)
print("Amber nighttime: 3min")
print_mixed_model(model_list9)
