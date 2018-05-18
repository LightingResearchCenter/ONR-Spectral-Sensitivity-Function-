
library(ggsignif)
library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)
library(reshape2)


##This script plots EEG data fro each subjects

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

plot_allSubjects <- function(plotData, outcomeMeasure, color, filtered){
  
  if(filtered){
    plotData <- plotData[plotData$Channel == outcomeMeasure & plotData$color == color & plotData$percent_used >= .3 & plotData$trial != 1,]
    
  }else{
    plotData <- plotData[plotData$Channel == outcomeMeasure & plotData$color == color & plotData$trial != 1,]
    
  }

  meanplotData <-  summarySE(data = plotData, measurevar = "norm_t1", groupvars = c("subject", "Channel", "color", "light_level"))
  

  meanplotData$se <- ifelse(is.na(meanplotData$se), 0, meanplotData$se)
  meanplotData$sd <- ifelse(is.na(meanplotData$sd), 0, meanplotData$sd)
  meanplotData$ci <- ifelse(is.na(meanplotData$ci), 0, meanplotData$ci)
  
  meanplotData$norm_t1 <- round(meanplotData$norm_t1, digits = 2)
  
  yUpper <- max(plotData$norm_t1) + max(  plotData$se)+ (max(  plotData$se)* .10)
  meanDuration <- round(mean(  plotData$norm_t1), digits = 2) 
  medianDuration <- round(median(  plotData$norm_t1), digits = 2) 
  sd2_Duration_upper <- sd(  plotData$norm_t1)*2 + meanDuration
  sd2_Duration_lower <- meanDuration - sd(  plotData$norm_t1)*2 
  
  
  
  figSpecsList <- c(yUpper, meanDuration, medianDuration, sd2_Duration_upper, sd2_Duration_lower)
  
  
  
  unitString <- paste( "Normalised", outcomeMeasure, " power", sep = " ")
  
  fontSize <- 3
  
  titleText <- paste(simpleCap(color), simpleCap(outcomeMeasure), "Power", sep = " " )
  
  subPlot1 <- ggplot(meanplotData, aes_string(x="light_level", y="norm_t1", colour = "light_level", group = "subject")) +
    geom_line(colour =  "black")+
    geom_errorbar(aes(ymin=norm_t1-se, ymax=norm_t1+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9), colour = "black")+
    geom_point(aes(size = .5))+
    labs(y = unitString)+
    coord_cartesian(ylim=c(0,figSpecsList[1]))+
    geom_text(aes_string(label = "norm_t1", vjust=2)) +
    geom_segment(aes(x = 0,xend=.2,y=figSpecsList[2],yend=figSpecsList[2]), color = "red", linetype = "dashed",  size = .4) + 
    annotate("text", x = .3, y = figSpecsList[2]+(figSpecsList[2]*.07), label = paste("mu : ", figSpecsList[2], sep = ""), parse = TRUE, color = "red", size = fontSize) +
    
    geom_segment(aes(x = 4.8,xend=5,y=figSpecsList[3],yend=figSpecsList[3]), color = "red", linetype = "dashed", size = .4) + 
    annotate("text", x = 4.7, y=figSpecsList[3]+(figSpecsList[3]*.07), label = paste("M : ", figSpecsList[3], sep = ""), parse = TRUE, color = "red", size = fontSize) +
    
    
    theme(axis.title.x=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5))+
    ggtitle(titleText)+
    #scale_x_continuous(expand = c(0, 0), limits = c(0,5), breaks = c( 1, 2, 3, 4),label = c( "W1 - B", paste("W2 - ", plotData[plotData$period2 == 2,]$CS_levels_2[1]), "W3 - B", paste("W4 - ", plotData[plotData$period2 == 4,]$CS_levels_2[1]))
    #)+
    #scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
    facet_wrap(~ subject,  ncol=3, scales = "free")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          plot.title = element_text(size = 30), strip.text = element_text(size=11))
  
  
  if(figSpecsList[5] > 0){
    subPlot1 <- subPlot1 + geom_hline(aes(yintercept = figSpecsList[5]), colour = "red", size = .4, linetype = "dashed") + 
      annotate("text", x = 2.5, y=figSpecsList[5]+(yUpper*.03), label = "2 SD from mean", colour = "red", size = fontSize) 
  }
  
  subPlot1 <- subPlot1 + geom_hline(aes(yintercept = figSpecsList[4]), colour = "red", size =  .4, linetype="dashed") + 
  annotate("text", x = 2.5, y=figSpecsList[4]+(yUpper*.03), label = "2 SD from mean", colour = "red", size = fontSize) 
  
  
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

filtered_eeg <- subset(UpdatedEEG_3_11_18, percent_used >= .3)
#filtered_eeg <- UpdatedEEG_3_11_18
nsubsReturn(UpdatedEEG_3_11_18, "amber")


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
daytime_eeg <- read_excel("//root/projects/EEG-Daytime-ONR-2017/processed-data/month1&2/cropped-longFormatTable_2018_04_11_14_53.xlsx", 
                          col_types = c("text", "text", "numeric", 
                                        "text", "text", "numeric", "date", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))
daytime_eeg$TimeOfDay <- "day"

daytime_eeg$light_level <- ifelse(daytime_eeg$light_level =="d", "dim", ifelse(daytime_eeg$light_level =="m", "medium", ifelse(daytime_eeg$light_level =="l", "low","high" )) )
daytime_eeg$light_level <- factor(daytime_eeg$light_level, levels = c("high", "medium", "low", "dim"))



if(FALSE){
  
  thetaRed_plot <- plot_allSubjects(UpdatedEEG_3_11_18, 'theta', "red" )
  alphacyan_plot1 <- plot_allSubjects(UpdatedEEG_3_11_18, 'alpha', "cyan", FALSE )
  alphacyan_plot2 <- plot_allSubjects(UpdatedEEG_3_11_18, 'alpha', "cyan", TRUE )
  
  
  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'alpha', "red", FALSE )

  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'theta', "blue", FALSE )
  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'atheta', "blue", FALSE )
  alphaRed_d_plot <- plot_allSubjects(daytime_eeg, 'alpha', "blue", FALSE )
  
  
    ggsave("//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/ThetaRed.pdf", dpi = 800, width = 17, height = 22, units = "in")
  
  
}

if(FALSE){
  
  createNames <- function(){
    #colorList <- c("blue", "cyan", "green", "amber", "red")
    colorList <- c( "cyan", "amber")
    
    powerBins <- c("theta", "atheta", "alpha", "halpha", "beta")
    newList <- c()
    for(i in 1:length(colorList)){
      for(j in 1:length(powerBins)){
        print(paste(colorList[i], powerBins[j], sep = "_"))
        newList <- c(newList, paste(colorList[i], powerBins[j], sep = "_"))
      }
    }
    return(newList)
  }
  
  figure_list <- createNames()
  
  for(i in 1:length(figure_list)){
    curr <- strsplit(figure_list[i], "_")
    print(curr[[1]][1])
    print(curr[[1]][2])
    currPlot <- plot_allSubjects(filtered_eeg, curr[[1]][2], curr[[1]][1], TRUE)
    nSubs <- nsubsReturn(filtered_eeg[filtered_eeg$color == "cyan" | filtered_eeg$color == "amber", ], curr[[1]][1])
    
    dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/"
    pdfFileName <- paste(figure_list[i], "_filterd.png")
    ggsave(paste(dir, pdfFileName, sep = ""), dpi = 250, width = 9.5, height = (ceiling(nSubs/3))*3, units = "in")
    
  }
  
}
