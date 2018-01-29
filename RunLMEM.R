source("EEG-LMEM-functions.R")
library(ggsignif)
library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(reshape2)


EEG_dataCropped_1_29_18 <- read_csv("//root/projects/EEG-Nighttime-ONR-2017/data-sets-EEGpower/EEG-dataCropped-1-29-18.csv")

EEG_dataCropped_1_29_18$light_level <- factor(EEG_dataCropped_1_29_18$light_level, levels = c("high", "medium", "low", "dim"))
EEG_dataCropped_1_29_18$Channel <- factor(EEG_dataCropped_1_29_18$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta"))
EEG_dataCropped_1_29_18$subject <- as.factor(EEG_dataCropped_1_29_18$subject)
EEG_dataCropped_1_29_18$color <- as.factor(EEG_dataCropped_1_29_18$color)
EEG_dataCropped_1_29_18$Channel <- as.factor(EEG_dataCropped_1_29_18$Channel)
EEG_dataCropped_1_29_18$trial <- as.factor(EEG_dataCropped_1_29_18$trial)
EEG_dataCropped_1_29_18$TimeOfDay <- as.factor("night")




model_list1 <- output_mixed_model1(EEG_dataCropped_1_29_18, "night")
model_list2 <- output_mixed_model2(EEG_dataCropped_1_29_18, "night")



print("All data: nighttime, specifed to look at light level")
print_mixed_model(model_list1)

print("All data: nighttime, specifed to look at spectra")
print_mixed_model(model_list2)




##Looking at each indivudual spectra
model_list5 <- output_mixed_model_single_spectrum(EEG_dataCropped_1_29_18, "night", "red")
model_list6 <- output_mixed_model_single_spectrum(EEG_dataCropped_1_29_18, "night", "green")
model_list7 <- output_mixed_model_single_spectrum(EEG_dataCropped_1_29_18, "night", "blue")



print("Red: nighttime 3min")
print_mixed_model(model_list5)
print("Green: nighttime 3min")
print_mixed_model(model_list6)
print("Blue nighttime: 3min")
print_mixed_model(model_list7)
