

#1. Previous ISI
#2. Next ISI
#3. fast_response_time < 200 ms
#4. false_start_after_miss
#5. fast_rt_after_miss
#6. 




addISIfactors <- function(PerformData, task){
  
  PerformData$sub_cond_block <- paste(PerformData$Subject_id, PerformData$condition, PerformData$block, sep = "_")
  
  
  PerformData$next_time <- NA
  
  
  PerformData$previous_isi <- NA
  PerformData$next_isi <- NA
  
  
  
  blockList <- unique(PerformData$sub_cond_block)
  
  for(i in 1:length(blockList)){
    
    
    print(blockList[i])
    
    blockLength <- length(PerformData[PerformData$sub_cond_block == blockList[i],]$date)
    
    PerformData[PerformData$sub_cond_block == blockList[i],]$next_time[1:(blockLength - 1)] <-
      PerformData[PerformData$sub_cond_block == blockList[i],]$time[2:blockLength]
    
    
  }
  
  PerformData$next_isi <- PerformData$next_time - PerformData$time
  
  for(i in 1:length(blockList)){
    
    
    print(blockList[i])
    
    blockLength <- length(PerformData[PerformData$sub_cond_block == blockList[i],]$date)
    
    PerformData[PerformData$sub_cond_block == blockList[i],]$previous_isi[2:(blockLength)] <-
      PerformData[PerformData$sub_cond_block == blockList[i],]$next_isi[1:blockLength]
    
    
  }
  
  
  
  PerformData$previous_isi <- as.numeric(PerformData$previous_isi)
  PerformData$next_isi <- as.numeric(PerformData$next_isi)
  PerformData$response_time <- as.numeric(PerformData$response_time)
  
  PerformData$fast_response_time <- ifelse(PerformData$response_time < .2, 1, 0)
  
  
  

  
  
  ###adding misses and fast rt after misses
  
  PerformData$false_start_after_miss <- 0
  PerformData$fast_rt_after_miss <- 0
  
  
  
  if(task == "GNG"){
    PerformData$false_start <- ifelse(PerformData$response_time < .1, 1, 0)
    PerformData$false_start <- ifelse(is.na(PerformData$false_start), 0, PerformData$false_start)
    
    PerformData$lapse <- ifelse(as.numeric(PerformData$response_time) > .5 & PerformData$display == "go" , 1, 0)
    PerformData$lapse <- ifelse(is.na(PerformData$response_time), 0, PerformData$lapse)
    
    PerformData$lapse400 <- ifelse(as.numeric(PerformData$response_time) > .4 & PerformData$display == "go", 1, 0)
    PerformData$lapse400 <- ifelse(is.na(PerformData$response_time), 0, PerformData$lapse400)
    
    PerformData$false_positive <- ifelse((as.numeric(PerformData$response_time) > 0 & PerformData$display == "no_go") & PerformData$display == "no_go" , 1, 0)
    PerformData$false_positive <- ifelse(is.na(PerformData$false_positive), 0, PerformData$false_positive)
    
    PerformData$miss <- ifelse((is.na(PerformData$response_time)) & PerformData$display == "go", 1, 0)
    
    
  }
  if(task == "N-back"){
    PerformData$false_start <- ifelse(PerformData$response_time < .1, 1, 0)
    PerformData$false_start <- ifelse(is.na(PerformData$false_start), 0, PerformData$false_start)
    PerformData$miss <- ifelse(is.na(PerformData$response_time) & PerformData$trial_num != 1, 1, 0)
  }
  PerformData$previous_miss <- NA
  
  for(i in 1:length(blockList)){
    
    
    print(blockList[i])
    
    
    blockLength <- length(PerformData[PerformData$sub_cond_block == blockList[i],]$date)
    
    PerformData[PerformData$sub_cond_block == blockList[i],]$previous_miss[2:(blockLength)] <-
      PerformData[PerformData$sub_cond_block == blockList[i],]$miss[1:blockLength]
    
  }
  
  PerformData$false_start_after_miss <- ifelse(PerformData$previous_miss == 1 & PerformData$false_start == 1, 1, 0)
  PerformData$fast_rt_after_miss <- ifelse(PerformData$previous_miss ==1 & PerformData$fast_response_time == 1, 1, 0)
  
  if(task == "GNG"){
  PerformData$false_positive <- ifelse(PerformData$display == "no_go" & !is.na(PerformData$response_time), 1, 0)
  }
  if(task == "N-back"){
    PerformData$false_positive <- ifelse(PerformData$accuracy == 0, 1, 0)
  }
  return(PerformData)
}


plotScenerios <- function(PlotData){
  
  previous_isi_data <- subset(PlotData, !is.na(previous_isi) )
  next_isi_data <- subset(PlotData, !is.na(next_isi) )
  
  ########################
  
  ###Specific scenerios
  library(ggplot2)
  
  ## Relationship: previous ISI * false positive
  previous_isi_data$isi_rounded <- as.numeric(substr(as.character(previous_isi_data$previous_isi) , 1, 3))
  prev_isi_falsePos <- data.frame(table(previous_isi_data$isi_rounded , previous_isi_data$false_positive))
  prev_isi_falsePosTRUE <- subset(prev_isi_falsePos, Var2 == 1)
  gg <- ggplot(prev_isi_falsePosTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of false positives")
  
  print(gg)
  
  ## Relationship: previous ISI * false start
  prev_isi_falseStart <-data.frame(table(previous_isi_data$isi_rounded , previous_isi_data$false_start))
  prev_isi_falseStartTRUE <- subset(prev_isi_falseStart, Var2 == 1)
  gg <- ggplot(prev_isi_falseStartTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of false starts")
  
  
  print(gg)
  
  ## Relationship: previous ISI * false start after miss
  falseStart_afterMiss <-data.frame(table(previous_isi_data$isi_rounded, previous_isi_data$false_start_after_miss))
  falseStartaftermissTRUE <- subset(falseStart_afterMiss, Var2 == 1)
  gg <- ggplot(falseStartaftermissTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of false starts after miss")
  
  
  print(gg)
  
  ## Relationship: previous ISI * fast RT ( < 200ms) after miss
  fastRt_afterMiss <- data.frame(table(previous_isi_data$isi_rounded, previous_isi_data$fast_rt_after_miss))
  fastRt_afterMissTRUE <- subset(fastRt_afterMiss, Var2 == 1)
  gg <- ggplot(fastRt_afterMissTRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Previous ISI", y = "Number of fast RT ( < 200ms) after miss")
  
  
  print(gg)
  
  ## Relationship: Next ISI * miss
  next_isi_data$isi_rounded <- as.numeric(substr(as.character(next_isi_data$next_isi) , 1, 3))
  miss_afterISI <- data.frame(table(next_isi_data$isi_rounded , next_isi_data$miss))
  miss_afterISI_TRUE <- subset(miss_afterISI, Var2 == 1)
  gg <- ggplot(miss_afterISI_TRUE, aes(x=Var1, y=Freq)) +
    geom_point()+
    labs(x="Next ISI", y = "Misses")
  print(gg)
  
  gg <- ggplot(PlotData, aes(x=next_isi, y=response_time)) +
    geom_point()+
    labs(x="Next ISI", y = "RT")
  
  
  print(gg)
}

if(FALSE){
  
  
  library(readr)
  
  
 # performance <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_ONR_processed_2018-05-09_14-48-36.csv")
  #performance <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_ONR_processed_2018-12-13_11-19-11.csv")
  performance <-read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_ONR_processed_2019-05-02_09-07-10.csv")
  performance$id_num1 <- paste(performance$Subject_id, performance$condition, performance$session, sep="_")
  
  performance$sub_cond <- paste(performance$Subject_id, performance$condition, sep = "_")
  performance$sub_block <- paste(performance$Subject_id, performance$condition, performance$block, sep = "_")
  
  performance1 <- subset(performance, Subject_id != "s132")
  
  test <- subset(performance, (Subject_id == "s214" | Subject_id == "s201")  & condition == "dark")
  
  test <- addISIfactors(test, "GNG")
  

  GNG_eeg <- addISIfactors(GNG_eeg, "GNG")
  
  
  next_isi_NA <- subset(GNG_eeg, is.na(next_isi))
  previous_isi_NA <- subset(GNG_eeg, is.na(previous_isi))
  
  next_isi_NEGATIVE <- subset(GNG_eeg, next_isi < 0)
  previous_isi_NEGATIVE <- subset(GNG_eeg, previous_isi < 0)
  
  GNG_eeg$next_isi <-ifelse(GNG_eeg$next_isi < 0, NA, GNG_eeg$next_isi)
  
  write.csv(GNG_eeg, "//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegISIadded_5_13_19.csv", row.names = FALSE)
  
  GNG_eeg$next_isi <- as.numeric(GNG_eeg$next_isi )
  GNG_isi_removed <- subset(GNG_eeg, next_isi > 1 )
  
  write.csv(GNG_isi_removed, "//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegISIremoved_5_13_19.csv", row.names = FALSE)
  
  
  
  
  

  
  #GNG_eegISIadded <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegISIadded_7_18_18.csv")

  #GNG_eegISIremoved <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegISIremoved_7_18_18.csv")
  GNG_eegISIadded <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegISIadded_5_13_19.csv")
  GNG_eegISIadded <- GNG_eegISIadded[GNG_eegISIadded$condition != "twenty-five",]
  subNumbers <- data.frame(table(GNG_eegISIadded$Subject_id))
  subNumbers$Conds <- subNumbers$Freq/1120
  
  GNG_eegISIremoved <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegISIremoved_5_13_19.csv")
  
  GNG_eegISIremoved <- GNG_eegISIremoved[GNG_eegISIremoved$condition != "twenty-five",]
  GNG_eegISIremoved$condition <- ifelse(GNG_eegISIremoved$condition == "dark", "dim", 
                                        ifelse(GNG_eegISIremoved$condition == "low", "L1", 
                                        ifelse(GNG_eegISIremoved$condition == "fifteen", "L2", 
                                        ifelse(GNG_eegISIremoved$condition == "medium", "L3", "L4"))))
  
  GNG_eegISIremoved$sub_cond <- paste(GNG_eegISIremoved$Subject_id, GNG_eegISIremoved$condition, sep = "_")
  GNG_eegISIremoved$id_num1 <- paste(GNG_eegISIremoved$Subject_id, GNG_eegISIremoved$condition, GNG_eegISIremoved$block, sep = "_")
  
  
  write.csv(GNG_eegISIremoved, "//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eeg_5_13_19.csv", row.names = FALSE)
  
  plotScenerios(GNG_eegISIadded)
  plotScenerios(GNG_eegISIremoved)
  
  GNG_eegISIremoved <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eeg_5_13_19.csv")
  
  GNG_eegISIremoved2 <- GNG_eegISIremoved[GNG_eegISIremoved$Subject_id != "s122" & GNG_eegISIremoved$Subject_id != "s181" & GNG_eegISIremoved$Subject_id != "s106",]
 # write.csv(GNG_eegISIremoved2, "//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegCropped_12_14_18.csv", row.names = FALSE)
  write.csv(GNG_eegISIremoved2, "//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_eegCropped_5_14_19.csv", row.names = FALSE)
  
  ##************************^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    
  #quality check
  missRate <- aggregate(miss ~ condition * color * Subject_id * block, data = GNG_eegISIremoved2, FUN = mean)
  missRate2 <- aggregate(miss ~ Subject_id * color, data = missRate, FUN = length)
  missRate3 <- aggregate(miss ~ Subject_id, data = GNG_eegISIremoved, FUN = sd)
  
  fpRate <- aggregate(false_positive ~ condition * color * Subject_id, data = GNG_eegISIremoved, FUN = mean)
  
  
  
  
}



Daytime <- FALSE
if(Daytime){
  
  
  library(readr)
  
  performance <- GNG
  performance$id_num1 <- paste(performance$Subject_id, performance$condition, performance$session, sep="_")
  
  performance$sub_cond <- paste(performance$Subject_id, performance$condition, sep = "_")
  performance$sub_block <- paste(performance$Subject_id, performance$condition, performance$block, sep = "_")
  

  
  
  GNG_eeg <- addISIfactors(performance, "GNG")
  
  next_isi_NA <- subset(GNG_eeg, is.na(next_isi))
  previous_isi_NA <- subset(GNG_eeg, is.na(previous_isi))
  
  
  GNG_eeg$next_isi <- as.numeric(GNG_eeg$next_isi )
  GNG_isi_removed <- subset(GNG_eeg, next_isi > 1 )
  write.csv(GNG_eeg, "//root/projects/EEG-Daytime-ONR-2017/Raw-data/GNG_data/processedData/GNG_eegDaytime_1_18_19.csv", row.names = FALSE)
  
  write.csv(GNG_isi_removed[GNG_isi_removed$sub_cond != "s223_high",], "//root/projects/EEG-Daytime-ONR-2017/Raw-data/GNG_data/processedData/GNG_eegDaytime_ISIremoved_1_18_19.csv", row.names = FALSE)
  
  
  
  
  
  
  

  plotScenerios(GNG_eeg)
  plotScenerios(GNG_isi_removed)
  

  
  
  
  
}


