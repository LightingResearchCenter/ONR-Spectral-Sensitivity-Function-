library(readr)
library(Rmisc)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)

month5_EEG_data <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/month5/longFormatTable_month5_USE-THIS.xlsx")

month5_EEG_data$norm_t1 <- as.numeric(month5_EEG_data$norm_t1 )
colnames(month5_EEG_data)[5] <- "light_level"

month5_EEG_data$light_level <- ifelse(month5_EEG_data$light_level =="d", "dim", ifelse(month5_EEG_data$light_level =="m", "medium", ifelse(month5_EEG_data$light_level =="l", "low","high" )) )


month5_EEG_data$color <- as.factor(month5_EEG_data$color)

###################################################################################
###Testing the reliabilty of data


month5_EEG_data$percent_used <- month5_EEG_data$UsedDataCount/month5_EEG_data$TotalDataCount

colnames(month5_EEG_data)[12] <- "percent_used"


month5_EEG_data2 <- month5_EEG_data
sum_data_percent <- aggregate(percent_used ~ subject + color + session + light_level + trial + session + date  , data = month5_EEG_data2, FUN = mean)

sum_data_percent <- sum_data_percent %>% 
  group_by(subject) %>% 
  mutate(sub_percent_mean = mean(percent_used))


sum_data_percent$light_level <- factor(sum_data_percent$light_level, levels = c("high", "medium", "low", "dim"))
sum_data_percent_cyan <- subset(sum_data_percent, color == "cyan")
sum_data_percent_amber <- subset(sum_data_percent, color == "amber")



sum_data_percent4 <- subset(sum_data_percent, percent_used >= 30)
sum_data_percent_cyan3 <- subset(sum_data_percent4, color == "cyan")
sum_data_percent_amber3 <- subset(sum_data_percent4, color == "amber")

sum_data_percent4$sub_char <- paste(sum_data_percent4$subject, sum_data_percent4$light_level, sep = "_")
missing_after_filter <-  data.frame(table(sum_data_percent4$sub_char))

cyan_outllier_limit <- mean(sum_data_percent_cyan$percent_used) - sd(sum_data_percent_cyan$percent_used)*2
amber_outllier_limit <- mean(sum_data_percent_amber$percent_used) - sd(sum_data_percent_amber$percent_used)*2

sub_data_percent_cyan <- aggregate(percent_used ~ subject, data = sum_data_percent_cyan, FUN = mean)
sub_data_percent_amber <- aggregate(percent_used ~ subject, data = sum_data_percent_amber, FUN = mean)
##Subject 117 was the only subject whose mean percent_used was below the outlier limit
##Therefore was excluded from analysis

gg <- ggplot(sum_data_percent_cyan, aes(x =trial, y= percent_used, fill = light_level, colour = light_level))+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 30) + 
  #geom_hline(yintercept = cyan_outllier_limit) + 
  facet_grid(. ~ subject)+
  theme(legend.position="none") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_colour_manual(values=c("cyan1", "cyan2", "cyan4", "#999999"))+
  labs(y = "Percentage of unfilted data (%)")

ggsave("//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/cyanReliability.png", dpi = 250, width = 12, height = 4.5, units = "in")

gg1 <- ggplot(sum_data_percent_amber, aes(x =trial, y= percent_used, fill = light_level, colour = light_level))+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 30) + 
  #geom_hline(yintercept = amber_outllier_limit) + 
  facet_grid(. ~ subject)+
  theme(legend.position="none") +
  coord_cartesian(ylim=c(0, 100)) +
  scale_colour_manual(values=c("brown1", "brown4", "red2", "#999999")) +
  labs(y = "Percentage of unfilted data (%)")


ggsave("//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/amberReliability.png", dpi = 250, width = 12, height = 4.5, units = "in")




gg5 <- ggplot(sum_data_percent_cyan3, aes(x =trial, y= percent_used, fill = light_level, colour = light_level))+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 30) + 
  geom_hline(yintercept = cyan_outllier_limit) + 
  facet_grid(. ~ subject)+
  coord_cartesian(ylim=c(0, 100)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("cyan1", "cyan2", "cyan4", "#999999")) 

gg6 <- ggplot(sum_data_percent_amber3, aes(x =trial, y= percent_used, fill = light_level, colour = light_level))+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 30) + 
  geom_hline(yintercept = amber_outllier_limit) + 
  facet_grid(. ~ subject)+
  coord_cartesian(ylim=c(0, 100)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("brown1", "brown4", "red2", "#999999")) 


#gg <- ggplotly(gg)
#gg1 <- ggplotly(gg1)

#gg3 <- ggplotly(gg3)
#gg4 <- ggplotly(gg4)

#gg5 <- ggplotly(gg5)
#gg6 <- ggplotly(gg6)


multiplot(gg, gg5, gg1, gg6, cols=2)



norm_raw <- subset(month5_EEG_data, trial != 1 & !is.na(Value))


norm_raw$light_level <- factor(norm_raw$light_level, levels = c("high", "medium", "low", "dim"))


norm_raw$Channel <- factor(norm_raw$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta"))

norm_raw$subject <- as.factor(norm_raw$subject)
norm_raw$color <- as.factor(norm_raw$color)
norm_raw$Channel <- as.factor(norm_raw$Channel)

norm_raw$sub_trial_id <- paste(norm_raw$subject, norm_raw$light_level, norm_raw$trial, sep = "_")
norm_raw$sub_session_id <- paste(norm_raw$subject, norm_raw$light_level, sep = "_")

## Removal of Data
# 1. Removing subject 117 based on the subject mean percent used was less than 2 sd from population mean
# 2. 
norm_raw2 <- subset(norm_raw, sub_trial_id != "212_low_3" & sub_trial_id != "208_medium_2" & sub_trial_id != "202_low_2" & sub_trial_id != "212_dim_3")

