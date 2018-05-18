
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

plot_allSubjects <- function(plotData, outcomeMeasure, color){
  
  plotData$condition <- factor(plotData$condition, c("high", "medium", "low", "dark"))
  plotData <- plotData[plotData$color == color,]
  if(outcomeMeasure == "fp"){
    no_only <- subset(plotData, display == "no_go")
    
    meanplotData <-  summarySE(data = no_only, measurevar = "false_positive", groupvars = c("Subject_id", "color", "condition"))
    
    meanplotData$false_positive <- round(meanplotData$false_positive, digits = 2)
    
    yUpper <- max(meanplotData$false_positive) + max(  meanplotData$se)+ (max(  meanplotData$se)* .10)
    meanDuration <- round(mean(  meanplotData$false_positive), digits = 2) 
    medianDuration <- round(median(  meanplotData$false_positive), digits = 2) 
    sd2_Duration_upper <- sd(  meanplotData$false_positive)*2 + meanDuration
    sd2_Duration_lower <- meanDuration - sd(  meanplotData$false_positive)*2 
    meanplotData$se <- ifelse(is.na(meanplotData$se), 0, meanplotData$se)
    meanplotData$sd <- ifelse(is.na(meanplotData$sd), 0, meanplotData$sd)
    meanplotData$ci <- ifelse(is.na(meanplotData$ci), 0, meanplotData$ci)
    
    
    
    
    figSpecsList <- c(yUpper, meanDuration, medianDuration, sd2_Duration_upper, sd2_Duration_lower)
    
    
    
    unitString <- "False positive"
    
    fontSize <- 3
    
    titleText <- paste(simpleCap(color), unitString, sep = " " )
    
    subPlot1 <- ggplot(meanplotData, aes_string(x="condition", y="false_positive", colour = "condition", group = "Subject_id")) +
      geom_line(colour =  "black")+
      geom_errorbar(aes(ymin=false_positive-se, ymax=false_positive+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9), colour = "black")+
      geom_point(aes(size = .5))+
      labs(y = unitString)+
      coord_cartesian(ylim=c(-.2,figSpecsList[1]))+
      geom_text(aes_string(label = "false_positive", vjust=2)) +
      geom_segment(aes(x = 0,xend=.2,y=figSpecsList[2],yend=figSpecsList[2]), color = "red", linetype = "dashed",  size = .4) + 
      annotate("text", x = .3, y = figSpecsList[2]+(figSpecsList[2]*.07), label = paste("mu : ", figSpecsList[2], sep = ""), parse = TRUE, color = "red", size = fontSize) +
      
      geom_segment(aes(x = 4.8,xend=5,y=figSpecsList[3],yend=figSpecsList[3]), color = "red", linetype = "dashed", size = .4) + 
      annotate("text", x = 4.7, y=figSpecsList[3]+(figSpecsList[3]*.07), label = paste("M : ", figSpecsList[3], sep = ""), parse = TRUE, color = "red", size = fontSize) +
      
      
      theme(axis.title.x=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5))+
      ggtitle(titleText)+
      #scale_x_continuous(expand = c(0, 0), limits = c(0,5), breaks = c( 1, 2, 3, 4),label = c( "W1 - B", paste("W2 - ", plotData[plotData$period2 == 2,]$CS_levels_2[1]), "W3 - B", paste("W4 - ", plotData[plotData$period2 == 4,]$CS_levels_2[1]))
      #)+
      #scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
      facet_wrap(~ Subject_id,  ncol=3, scales = "free")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            plot.title = element_text(size = 30), strip.text = element_text(size=11))
  }
  if(outcomeMeasure == "miss"){
    go_only <- subset(plotData, display == "go")
    
    meanplotData <-  summarySE(data = go_only, measurevar = "miss", groupvars = c("Subject_id", "color", "condition"))
    
    meanplotData$miss <- round(meanplotData$miss, digits = 2)
    
    yUpper <- max(meanplotData$miss) + max(  meanplotData$se)+ (max(  meanplotData$se)* .10)
    meanDuration <- round(mean(  meanplotData$miss), digits = 2) 
    medianDuration <- round(median(  meanplotData$miss), digits = 2) 
    sd2_Duration_upper <- sd(  meanplotData$miss)*2 + meanDuration
    sd2_Duration_lower <- meanDuration - sd(  meanplotData$miss)*2 
    meanplotData$se <- ifelse(is.na(meanplotData$se), 0, meanplotData$se)
    meanplotData$sd <- ifelse(is.na(meanplotData$sd), 0, meanplotData$sd)
    meanplotData$ci <- ifelse(is.na(meanplotData$ci), 0, meanplotData$ci)
    
    
    
    
    figSpecsList <- c(yUpper, meanDuration, medianDuration, sd2_Duration_upper, sd2_Duration_lower)
    
    
    
    unitString <- "Miss"
    
    fontSize <- 3
    
    titleText <- paste(simpleCap(color), unitString,  sep = " " )
    
    subPlot1 <- ggplot(meanplotData, aes_string(x="condition", y="miss", colour = "condition", group = "Subject_id")) +
      geom_line(colour =  "black")+
      geom_errorbar(aes(ymin=miss-se, ymax=miss+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9), colour = "black")+
      geom_point(aes(size = .5))+
      labs(y = unitString)+
      coord_cartesian(ylim=c(-.2,figSpecsList[1]))+
      geom_text(aes_string(label = "miss", vjust=2)) +
      geom_segment(aes(x = 0,xend=.2,y=figSpecsList[2],yend=figSpecsList[2]), color = "red", linetype = "dashed",  size = .4) + 
      annotate("text", x = .3, y = figSpecsList[2]+(figSpecsList[2]*.07), label = paste("mu : ", figSpecsList[2], sep = ""), parse = TRUE, color = "red", size = fontSize) +
      
      geom_segment(aes(x = 4.8,xend=5,y=figSpecsList[3],yend=figSpecsList[3]), color = "red", linetype = "dashed", size = .4) + 
      annotate("text", x = 4.7, y=figSpecsList[3]+(figSpecsList[3]*.07), label = paste("M : ", figSpecsList[3], sep = ""), parse = TRUE, color = "red", size = fontSize) +
      
      
      theme(axis.title.x=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5))+
      ggtitle(titleText)+
      #scale_x_continuous(expand = c(0, 0), limits = c(0,5), breaks = c( 1, 2, 3, 4),label = c( "W1 - B", paste("W2 - ", plotData[plotData$period2 == 2,]$CS_levels_2[1]), "W3 - B", paste("W4 - ", plotData[plotData$period2 == 4,]$CS_levels_2[1]))
      #)+
      #scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
      facet_wrap(~ Subject_id,  ncol=3, scales = "free")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            plot.title = element_text(size = 30), strip.text = element_text(size=11))
    
    
  }
  

  if(outcomeMeasure == "rt"){
    rt_gng <- subset(plotData, display == "go" & response_time > .1)
    
    meanplotData <-  summarySE(data = rt_gng, measurevar = "response_time", groupvars = c("Subject_id", "color", "condition"))
    
    meanplotData$response_time <- round(meanplotData$response_time, digits = 3)
    
    yUpper <- max(meanplotData$response_time) + max(  meanplotData$se)+ (max(  meanplotData$se)* .10)
    meanDuration <- round(mean(  meanplotData$response_time), digits = 2) 
    medianDuration <- round(median(  meanplotData$response_time), digits = 2) 
    sd2_Duration_upper <- sd(  meanplotData$response_time)*2 + meanDuration
    sd2_Duration_lower <- meanDuration - sd(  meanplotData$response_time)*2 
    meanplotData$se <- ifelse(is.na(meanplotData$se), 0, meanplotData$se)
    meanplotData$sd <- ifelse(is.na(meanplotData$sd), 0, meanplotData$sd)
    meanplotData$ci <- ifelse(is.na(meanplotData$ci), 0, meanplotData$ci)
    
    
    
    
    figSpecsList <- c(yUpper, meanDuration, medianDuration, sd2_Duration_upper, sd2_Duration_lower)
    
    
    
    unitString <- "Response time (s)"
    
    fontSize <- 3
    
    titleText <- paste(simpleCap(color), unitString,  sep = " " )
    
    subPlot1 <- ggplot(meanplotData, aes_string(x="condition", y="response_time", colour = "condition", group = "Subject_id")) +
      geom_line(colour =  "black")+
      geom_errorbar(aes(ymin=response_time-se, ymax=response_time+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9), colour = "black")+
      geom_point(aes(size = .5))+
      labs(y = unitString)+
      coord_cartesian(ylim=c(-.2,figSpecsList[1]))+
      geom_text(aes_string(label = "response_time", vjust=2)) +
      geom_segment(aes(x = 0,xend=.2,y=figSpecsList[2],yend=figSpecsList[2]), color = "red", linetype = "dashed",  size = .4) + 
      annotate("text", x = .3, y = figSpecsList[2]+(figSpecsList[2]*.07), label = paste("mu : ", figSpecsList[2], sep = ""), parse = TRUE, color = "red", size = fontSize) +
      
      geom_segment(aes(x = 4.8,xend=5,y=figSpecsList[3],yend=figSpecsList[3]), color = "red", linetype = "dashed", size = .4) + 
      annotate("text", x = 4.7, y=figSpecsList[3]+(figSpecsList[3]*.07), label = paste("M : ", figSpecsList[3], sep = ""), parse = TRUE, color = "red", size = fontSize) +
      
      
      theme(axis.title.x=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5))+
      ggtitle(titleText)+
      #scale_x_continuous(expand = c(0, 0), limits = c(0,5), breaks = c( 1, 2, 3, 4),label = c( "W1 - B", paste("W2 - ", plotData[plotData$period2 == 2,]$CS_levels_2[1]), "W3 - B", paste("W4 - ", plotData[plotData$period2 == 4,]$CS_levels_2[1]))
      #)+
      #scale_y_continuous(expand = c(0, 0), limits = c(0,15))+
      facet_wrap(~ Subject_id,  ncol=3, scales = "free")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), 
            plot.title = element_text(size = 30), strip.text = element_text(size=11))
    
    
  }
  
  
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
  
  return(length(unique(plotData[plotData$color == color,]$Subject_id)))
}




if(FALSE){
  
  
  performance <- read_csv("//root/projects/EEG-Daytime-ONR-2017/Raw-data/GNG_data/processedData/GNG_Day-4-25-18.csv")
  
  performance <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_ONR_processed_2018-05-09_14-48-36.csv")
  
  
  
  performance <- subset(performance, uni2 != "s223-2018-03-29-GNG")
  
  
  
  fp_red <- plot_allSubjects(performance, 'fp', "red" )
  miss_red <- plot_allSubjects(performance, 'miss', "red" )
  rt_red <- plot_allSubjects(performance, 'rt', "red" )
  
  miss_red <- plot_allSubjects(performance, 'miss', "blue" )
  
  ggsave("//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/ThetaRed.pdf", dpi = 800, width = 17, height = 22, units = "in")
  
  
}
createNames <- function(colorList){
  #colorList <- c("blue", "cyan", "green", "amber", "red")
  #colorList <- c( "cyan", "amber")
  
  powerBins <- c("miss", "fp", "rt")
  newList <- c()
  for(i in 1:length(colorList)){
    for(j in 1:length(powerBins)){
      print(paste(colorList[i], powerBins[j], sep = "_"))
      newList <- c(newList, paste(colorList[i], powerBins[j], sep = "_"))
    }
  }
  return(newList)
}

saveGraph <- function(dayNight, colorList, PlotData ){
  
  figure_list <- createNames(colorList)
  
  for(i in 1:length(figure_list)){
    curr <- strsplit(figure_list[i], "_")
    print(curr[[1]][1])
    print(curr[[1]][2])
    currPlot <- plot_allSubjects(PlotData, curr[[1]][2], curr[[1]][1])
    nSubs <- nsubsReturn(PlotData[PlotData$color == curr[[1]][1], ], curr[[1]][1])
    
    if(tolower(dayNight) == "day"){
      dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/SUBJECT/DAY/GNG/"
      pdfFileName <- paste(figure_list[i], "1_day.pdf")
    }
    if(tolower(dayNight) == "night"){
      dir <- "//root/projects/ONR-EEG-BAA16_001/REPORTS/GRAPHS/SUBJECT/NIGHT/GNG/"
      pdfFileName <- paste(figure_list[i], "1_night.pdf")
    }
    
    ggsave(paste(dir, pdfFileName, sep = ""), dpi = 250, width = 14.5, height = (ceiling(nSubs/3))*3, units = "in")
    
  }
}


if(FALSE){
  colorList <- c( "red", "blue")
  saveGraph("Day", colorList, performance)
  
  
  colorList <- c("blue", "cyan", "green", "amber", "red")
  saveGraph("night", colorList, performance)
}

