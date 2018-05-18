library(Rmisc)
library(readr)
library(ggplot2)


performance <- read_csv("//root/projects/ONR-EEG-BAA16_001/PROCESSED_DATA/PERFORMANCE/GNG_ONR_processed_2018-05-09_14-48-36.csv")

performance$id_num1 <- paste(performance$Subject_id, performance$condition, performance$session, sep="_")

performance$sub_cond <- paste(performance$Subject_id, performance$condition, sep = "_")
performance$sub_block <- paste(performance$Subject_id, performance$condition, performance$block, sep = "_")

performance1 <- subset(performance, Subject_id != "s132")
performance2 <- subset(performance, Subject_id != "s132" & sub_cond != "s129_high" & sub_block != "s176_medium_4"
                       & sub_block != "s157_high_4" & Subject_id != "s146" & sub_block != "s153_high_3" 
                       & sub_cond != "s153_low" & sub_cond != "s159_medium" & sub_block != "s161_high_3"
                       & sub_block != "s161_high_4" & sub_cond != "s161_medium" & sub_block != "s161_low_3"
                       & sub_block != "s106_high_3" & sub_block != "s106_high_4" & Subject_id != "s109" 
                       & sub_block != "s127_low_3" & sub_block != "s127_medium_3" & sub_block != "s127_medium_4" 
                       & sub_block != "s134_high_1")

performance3 <- subset(performance2, block != 4)
performance4 <- subset(performance1, block != 4)

#Set the performance to main data
performanceXYZ <- performance1


colnames(performanceXYZ)[15] <- "light_level"
performanceXYZ$light_level <- ifelse(performanceXYZ$light_level =="dark", "dim", performanceXYZ$light_level )



performanceXYZ$Subject_id <- as.factor(performanceXYZ$Subject_id)
performanceXYZ$light_level <- as.factor(performanceXYZ$light_level)
performanceXYZ$color <- as.factor(performanceXYZ$color)
performanceXYZ$block <- as.factor(performanceXYZ$block)


#Red response time
ctrl <- lmeControl(opt='optim');



rt <- subset(performanceXYZ, response_time >= .1 & display == "go" & color == "red")


rt_model <- lme(response_time ~ light_level*block , random = ~1|Subject_id/light_level/block,control=ctrl, 
                data=rt)

rt_model_r2 <- r.squaredGLMM(rt_model)

rt_model_pVal <- anova(rt_model)

lsmeans(rt_model, pairwise~light_level, adjust="bonferroni", data = rt)


###Performance data copping #1 seems most conservative and yeidls the best results. We lose a lot of the strange 
# and negative significance that was found in the red light response time as well as the models testing between 
# spectra

