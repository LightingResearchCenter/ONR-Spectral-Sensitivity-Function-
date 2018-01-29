library(reshape2)
library(MuMIn)
library(nlme)
output_mixed_model1 <- function(data, time){
  if(time == "both"){
    theta <- subset(data, Channel == "theta" )
    atheta <- subset(data, Channel == "atheta" )
    alpha <- subset(data, Channel == "alpha" )
    halpha <- subset(data, Channel == "halpha")
    hbeta <- subset(data, Channel == "hbeta" )
    beta <- subset(data, Channel == "beta")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){
    theta <- subset(data, Channel == "theta" & TimeOfDay == "day")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "day" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "day")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "day")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "day")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "day")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){
    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & TimeOfDay == "night")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "night" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "night")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "night")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "night")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "night")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
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
  
  return(output_list)
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
  trial2.2 <- spread(trial2, key = c("light_level"), value = "norm_t1")
  
  trial2.2$high <- trial2.2$high/trial2.2$dim
  trial2.2$medium <- trial2.2$medium/trial2.2$dim
  trial2.2$low <- trial2.2$low/trial2.2$dim
  trial2.2$dim <- NULL
  
  trial2.3 <- melt(trial2.2, id = c("subject", "color", "group","Channel", "TimeOfDay", "trial"))
  
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
  trial3.2 <- spread(trial3, key = c("light_level"), value = "norm_t1")
  
  trial3.2$high <- trial3.2$high/trial3.2$dim
  trial3.2$medium <- trial3.2$medium/trial3.2$dim
  trial3.2$low <- trial3.2$low/trial3.2$dim
  trial3.2$dim <- NULL
  
  trial3.3 <- melt(trial3.2, id = c("subject", "color", "group","Channel", "TimeOfDay", "trial"))
  
  norm_t1_norm_dim <- rbind(trial2.3, trial3.3)
  colnames(norm_t1_norm_dim)[7] <- "light_level"
  colnames(norm_t1_norm_dim)[8] <- "norm_t1"
  return(norm_t1_norm_dim)
}

output_mixed_model2 <- function(data, time){
  if(time == "both"){
    data <- norm_to_Dim(data)
    data$norm_t1 <- as.numeric(data$norm_t1 )
    data <- subset(data, !is.na(norm_t1) & is.finite(norm_t1))
    theta <- subset(data, Channel == "theta" )
    atheta <- subset(data, Channel == "atheta" )
    alpha <- subset(data, Channel == "alpha" )
    halpha <- subset(data, Channel == "halpha")
    hbeta <- subset(data, Channel == "hbeta" )
    beta <- subset(data, Channel == "beta")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){
    data <- norm_to_Dim(data)
    data$norm_t1 <- as.numeric(data$norm_t1 )
    data <- subset(data, !is.na(norm_t1) & is.finite(norm_t1))
    theta <- subset(data, Channel == "theta" & TimeOfDay == "day")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "day" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "day")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "day")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "day")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "day")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){
    data <- norm_to_Dim(data)
    data$norm_t1 <- as.numeric(data$norm_t1 )
    data <- subset(data, !is.na(norm_t1) & is.finite(norm_t1))
    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & TimeOfDay == "night")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "night" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "night")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "night")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "night")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "night")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~1|subject/light_level/trial,
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
  
  return(output_list)
}


output_mixed_model_single_spectrum <- function(data, time, spectrum){
  if(time == "both"){
    theta <- subset(data, Channel == "theta" & color == spectrum)
    atheta <- subset(data, Channel == "atheta" & color == spectrum )
    alpha <- subset(data, Channel == "alpha" & color == spectrum)
    halpha <- subset(data, Channel == "halpha"& color == spectrum)
    hbeta <- subset(data, Channel == "hbeta" & color == spectrum)
    beta <- subset(data, Channel == "beta"& color == spectrum)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){
    theta <- subset(data, Channel == "theta" & TimeOfDay == "day" & color == spectrum)
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "day" & color == spectrum )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "day" & color == spectrum)
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "day" & color == spectrum)
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "day" & color == spectrum)
    beta <- subset(data, Channel == "beta" & TimeOfDay == "day" & color == spectrum)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){
    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & TimeOfDay == "night" & color == spectrum)
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "night" & color == spectrum )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "night" & color == spectrum)
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "night" & color == spectrum)
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "night" & color == spectrum)
    beta <- subset(data, Channel == "beta" & TimeOfDay == "night" & color == spectrum)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~1|subject/light_level/trial,
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

  return(output_list)
}
print_mixed_model <- function(model_list){
  
  print("Theta----------------")
  print(model_list[1])
  print(model_list[2])
  
  print("Alpha-theta----------")
  print(model_list[3])
  print(model_list[4])
  
  print("Alpha----------------")
  print(model_list[5])
  print(model_list[6])
  
  print("High Alpha-----------")
  print(model_list[7])
  print(model_list[8])
  
  print("Beta-----------------")
  print(model_list[9])
  print(model_list[10])
  
  print("High Beta------------")
  print(model_list[11])
  print(model_list[12])
  
}

output_mixed_model3 <- function(data, time){
  if(time == "both"){
    theta <- subset(data, Channel == "theta" )
    atheta <- subset(data, Channel == "atheta" )
    alpha <- subset(data, Channel == "alpha" )
    halpha <- subset(data, Channel == "halpha")
    hbeta <- subset(data, Channel == "hbeta" )
    beta <- subset(data, Channel == "beta")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){
    theta <- subset(data, Channel == "theta" & TimeOfDay == "day")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "day" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "day")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "day")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "day")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "day")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){
    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & TimeOfDay == "night")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "night" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "night")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "night")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "night")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "night")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
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
  
  return(output_list)
}
output_mixed_model4 <- function(data, time){
  if(time == "both"){
    data <- norm_to_Dim(data)
    data$norm_t1 <- as.numeric(data$norm_t1 )
    data <- subset(data, !is.na(norm_t1) & is.finite(norm_t1))
    theta <- subset(data, Channel == "theta" )
    atheta <- subset(data, Channel == "atheta" )
    alpha <- subset(data, Channel == "alpha" )
    halpha <- subset(data, Channel == "halpha")
    hbeta <- subset(data, Channel == "hbeta" )
    beta <- subset(data, Channel == "beta")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ color*light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){
    data <- norm_to_Dim(data)
    data$norm_t1 <- as.numeric(data$norm_t1 )
    data <- subset(data, !is.na(norm_t1) & is.finite(norm_t1))
    theta <- subset(data, Channel == "theta" & TimeOfDay == "day")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "day" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "day")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "day")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "day")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "day")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){
    data <- norm_to_Dim(data)
    data$norm_t1 <- as.numeric(data$norm_t1 )
    data <- subset(data, !is.na(norm_t1) & is.finite(norm_t1))
    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & TimeOfDay == "night")
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "night" )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "night")
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "night")
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "night")
    beta <- subset(data, Channel == "beta" & TimeOfDay == "night")
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ color*light_level*trial , random = ~trial|subject/light_level,
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
  
  return(output_list)
}


output_mixed_model_single_spectrum2 <- function(data, time, spectrum){
  if(time == "both"){
    theta <- subset(data, Channel == "theta" & color == spectrum)
    atheta <- subset(data, Channel == "atheta" & color == spectrum )
    alpha <- subset(data, Channel == "alpha" & color == spectrum)
    halpha <- subset(data, Channel == "halpha"& color == spectrum)
    hbeta <- subset(data, Channel == "hbeta" & color == spectrum)
    beta <- subset(data, Channel == "beta"& color == spectrum)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial + TimeOfDay , random = ~trial|subject/light_level,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "day"){
    theta <- subset(data, Channel == "theta" & TimeOfDay == "day" & color == spectrum)
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "day" & color == spectrum )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "day" & color == spectrum)
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "day" & color == spectrum)
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "day" & color == spectrum)
    beta <- subset(data, Channel == "beta" & TimeOfDay == "day" & color == spectrum)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=hbeta, control = ctrl)
    
  }
  if(time == "night"){
    print("All data: nighttime")
    theta <- subset(data, Channel == "theta" & TimeOfDay == "night" & color == spectrum)
    atheta <- subset(data, Channel == "atheta"  & TimeOfDay == "night" & color == spectrum )
    alpha <- subset(data, Channel == "alpha"   & TimeOfDay == "night" & color == spectrum)
    halpha <- subset(data, Channel == "halpha"  & TimeOfDay == "night" & color == spectrum)
    hbeta <- subset(data, Channel == "hbeta"  & TimeOfDay == "night" & color == spectrum)
    beta <- subset(data, Channel == "beta" & TimeOfDay == "night" & color == spectrum)
    
    
    ctrl <- lmeControl(opt='optim')
    theta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=theta, control = ctrl)
    
    atheta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=atheta, control = ctrl)
    
    alpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                       data=alpha, control = ctrl)
    
    halpha_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                        data=halpha, control = ctrl)
    
    beta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
                      data=beta, control = ctrl)
    
    hbeta_model <- lme(norm_t1 ~ light_level*trial , random = ~trial|subject/light_level,
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
  
  return(output_list)
}


