
library(ggsignif)
library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(dplyr)
library(tibble)

##This script plots EEG data fro each subjects

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

upperLim <- function(x) {
  return(quantile(x, 0.75) + 1.5 * IQR(x))
}

lowerLim <- function(x) {
  return(quantile(x, 0.25) - 1.5 * IQR(x) )
}


plot_Summary <- function(plotData, color, filtered){
  
  
  if(filtered){
    plotData <- plotData[plotData$color == color & plotData$percent_used >= .3 & plotData$trial != 1 & !is.na(plotData$ValueNorm1),]
    
  }else{
    plotData <- plotData[plotData$color == color & plotData$trial != 1 & !is.na(plotData$ValueNorm1),]
    
  }
  plotData$channel_condition <- paste(plotData$Channel, plotData$light_level, sep = "_")
  
  plotData2 <- aggregate(ValueNorm1 ~ light_level + subject + Channel + channel_condition + color, data = plotData, FUN = mean)
  
  #apply this to our data
  

  
  plotData2 <- plotData2  %>% group_by(channel_condition) %>% mutate(upperLim=upperLim(ValueNorm1))
  plotData2 <- plotData2  %>% group_by(channel_condition) %>% mutate(lowerLim=lowerLim(ValueNorm1))
  
  plotData2 <- plotData2 %>% group_by(channel_condition) %>% mutate(is_outlier=ifelse(is_outlier(ValueNorm1), subject, as.numeric(NA)))
  
  plotData2$outlier <- plotData2$ValueNorm1 < plotData2$lowerLim | plotData2$ValueNorm1 > plotData2$upperLim 

  yUpper <- max(plotData2$ValueNorm1) + (max(plotData2$ValueNorm1) * .10)
  meanDuration <- round(mean(  plotData2$ValueNorm1), digits = 2) 
  medianDuration <- round(median(  plotData2$ValueNorm1), digits = 2) 
  sd2_Duration_upper <- sd(  plotData2$ValueNorm1)*2 + meanDuration
  sd2_Duration_lower <- meanDuration - sd(  plotData2$ValueNorm1)*2 
  
  
  
  figSpecsList <- c(yUpper, meanDuration, medianDuration, sd2_Duration_upper, sd2_Duration_lower)
  
  
  plotData2$Channel <- factor( plotData2$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta")) 
  
  unitString <- paste( "Normalised"," power", sep = " ")
  
  fontSize <- 3
  
  titleText <- simpleCap(plotData2$color[1])
  

  

  
  dodge <- position_dodge(width = 0.7)
  
  
  subPlot1 <- ggplot(plotData2, aes_string(x="light_level", y="ValueNorm1", colour = "light_level")) +
    geom_boxplot()+
    labs(y = unitString)+
    coord_cartesian(ylim=c(0,figSpecsList[1]))+
    geom_text(aes(label=is_outlier),na.rm=TRUE,position = dodge, vjust = -0.3) +   
    theme(axis.title.x=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5))+
    ggtitle(titleText)+
    
    facet_wrap(~ Channel,  ncol=3, scales = "free")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          plot.title = element_text(size = 30), strip.text = element_text(size=11))
  
  
  
  
  
  if(color == "blue"){
    subPlot1 <- subPlot1 +   scale_colour_manual(values=c("deepskyblue2", "deepskyblue4", "dodgerblue2", "#999999")) 
    
    
  }
  if(color == "red"){
    subPlot1 <- subPlot1 +   scale_colour_manual(values=c("red1", "red4", "rosybrown", "#999999")) 
    
    
  }
  if(color == "green"){
    subPlot1 <- subPlot1 +   scale_colour_manual(values=c("green2", "green4", "palegreen4", "#999999")) 
    
    
  }
  
  if(color == "cyan"){
    subPlot1 <- subPlot1 + scale_colour_manual(values=c("cyan1", "cyan2", "cyan4", "#999999")) 
  }
  
  if(color == "amber"){
    subPlot1 <- subPlot1 + scale_colour_manual(values=c("brown1", "brown4", "red2", "#999999")) 
    
  }
  
  return(subPlot1)
  
  
}


nsubsReturn <- function(plotData, color){
  
  return(length(unique(plotData[plotData$color == color,]$subject)))
}


UpdatedEEG_3_11_18 <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/month5/UpdatedEEG_3_11_18.xlsx")
UpdatedEEG_3_11_18$percent_used <- UpdatedEEG_3_11_18$UsedDataCount/UpdatedEEG_3_11_18$TotalDataCount




filtered_eeg <- subset(UpdatedEEG_4_30_18, percent_used >= .3)
#filtered_eeg <- UpdatedEEG_3_11_18
nsubsReturn(UpdatedEEG_4_30_18, "amber")


UpdatedEEG_3_11_18$light_level <- factor(UpdatedEEG_3_11_18$light_level, levels = c("high", "medium", "low", "dim"))
UpdatedEEG_3_11_18$Channel <- factor(UpdatedEEG_3_11_18$Channel, levels = c("theta", "atheta", "alpha", "halpha", "beta", "hbeta"))
UpdatedEEG_3_11_18$subject <- as.factor(UpdatedEEG_3_11_18$subject)
UpdatedEEG_3_11_18$color <- as.factor(UpdatedEEG_3_11_18$color)
UpdatedEEG_3_11_18$Channel <- as.factor(UpdatedEEG_3_11_18$Channel)
UpdatedEEG_3_11_18$trial <- as.factor(UpdatedEEG_3_11_18$trial)
UpdatedEEG_3_11_18$TimeOfDay <- as.factor("night")



filtered_eeg <- subset(UpdatedEEG_3_11_18, percent_used >= .3 & subject != 158 & sub_session_id != "169_medium" )




##daytime
library(readxl)
library(readr)
library(Rmisc)
library(ggplot2)
library(plotly)
library(dplyr)
library(reshape2)
library(nlme)
library(lsmeans)
library(multcomp)
library(ggsignif)

ctrl <- lmeControl(opt='optim')



###Daytime 
library(readxl)


daytime_eeg <- read_excel("//root/projects/EEG-Daytime-ONR-2017/processed-data/completed/longFormatTableNorm1NormD_2018_04_25_14_24.xlsx")

daytime_eeg$color <- ifelse(daytime_eeg$color =="r", "red", "blue")
colnames(daytime_eeg)[6] <- "light_level"

daytime_eeg$light_level <- ifelse(daytime_eeg$light_level =="d", "dim", ifelse(daytime_eeg$light_level =="m", "med", ifelse(daytime_eeg$light_level =="l", "low","high" )) )
daytime_eeg$light_level <- factor(daytime_eeg$light_level, levels = c("high", "med", "low", "dim"))



UpdatedEEG_4_30_18 <- read_excel("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/EEG/total-months/longFormatTableNorm1NormD_2018_04_30_16_18.xlsx")

colnames(UpdatedEEG_4_30_18)[6] <- "light_level"

UpdatedEEG_4_30_18$light_level <- ifelse(UpdatedEEG_4_30_18$light_level =="d", "dim", ifelse(UpdatedEEG_4_30_18$light_level =="m", "med", ifelse(UpdatedEEG_4_30_18$light_level =="l", "low","high" )) )
UpdatedEEG_4_30_18$light_level <- factor(UpdatedEEG_4_30_18$light_level, levels = c("high", "med", "low", "dim"))

UpdatedEEG_4_30_18$color <- ifelse(UpdatedEEG_4_30_18$color =="r", "red", ifelse(UpdatedEEG_4_30_18$color =="b", "blue", ifelse(UpdatedEEG_4_30_18$color =="g", "green", ifelse(UpdatedEEG_4_30_18$color =="c", "cyan", ifelse(UpdatedEEG_4_30_18$color =="a", "amber", "N/A")))))




if(FALSE){
  
  thetaRed_plot <- plot_Summary(UpdatedEEG_4_30_18, 'theta', "red", FALSE )
  alphacyan_plot1 <- plot_Summary(UpdatedEEG_4_30_18, 'alpha', "cyan", FALSE )
  alphacyan_plot2 <- plot_Summary(UpdatedEEG_4_30_18, 'alpha', "cyan", TRUE )
  
  
  alphaRed_d_plot <- plot_Summary(daytime_eeg, 'alpha', "red", FALSE )
  
  alphaRed_d_plot <- plot_Summary(daytime_eeg, 'theta', "blue", FALSE )
  alphaRed_d_plot <- plot_Summary(daytime_eeg, 'atheta', "blue", FALSE )
  alphaRed_d_plot <- plot_Summary(daytime_eeg, 'alpha', "blue", FALSE )
  
  
  ggsave("//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/ThetaRed.pdf", dpi = 800, width = 17, height = 22, units = "in")
  
  
}

createNames <- function(colorList){
  
  
  newList <- c()
  for(i in 1:length(colorList)){
      newList <- c(newList, paste(colorList[i], sep = "_"))
  }
  return(newList)
}




saveGraph <- function(dayNight, colorList, PlotData ){
  
  figure_list <- createNames(colorList)
  
  for(i in 1:length(figure_list)){

    

    currPlot <- plot_Summary(PlotData, figure_list[i], FALSE)
    nSubs <- nsubsReturn(PlotData[PlotData$color == figure_list[i], ], figure_list[i])
    
    if(tolower(dayNight) == "day"){
      dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/SUMMARY/DAY/"
      pdfFileName <- paste(figure_list[i], "_day.pdf")
    }
    if(tolower(dayNight) == "night"){
      dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/SUMMARY/NIGHT/"
      pdfFileName <- paste(figure_list[i], "_night.pdf")
    }
    
    ggsave(paste(dir, pdfFileName, sep = ""), dpi = 250, width = 14.5, height = (ceiling(nSubs/3))*3, units = "in")
    
  }
}

colorList <- c("blue", "cyan", "green", "amber", "red")
colorList <- c( "cyan", "amber")
colorList <- c( "red", "blue")

if(FALSE){
  colorList <- c( "red", "blue")
  saveGraph("Day", colorList, daytime_eeg)
  
  
  colorList <- c("blue", "cyan", "green", "amber", "red")
  saveGraph("night", colorList, UpdatedEEG_4_30_18)
}