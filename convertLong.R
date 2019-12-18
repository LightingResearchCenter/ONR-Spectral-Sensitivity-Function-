

library(readxl)
CSDDdata <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/DATA/long-term/questionaire/CSDD.xlsx")
CMAI <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/DATA/long-term/questionaire/CMAI.xlsx")
MDS_ADL <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/DATA/long-term/questionaire/MDS_ADL.xlsx")
PSQI <- read_excel("//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/DATA/long-term/questionaire/PSQI.xlsx")

convertLongFormat <- function(variable){
  dir <- "//root/projects/NIH Alzheimers/NIH Alzheimers Phase two study/DATA/long-term/questionaire/"
  filename <- paste(dir, variable, ".xlsx", sep = "")
  
  sheetB <- read_excel(filename, sheet = "B")
  sheet1 <- read_excel(filename, sheet = "1")
  sheet2 <- read_excel(filename, sheet = "2")
  sheet3 <- read_excel(filename, sheet = "3")
  sheet4 <- read_excel(filename, sheet = "4")
  
  sheetB$period <- "B"
  sheet1$period <- "1"
  sheet2$period <- "2"
  sheet3$period <- "3"
  sheet4$period <- "4"
  
  sheetBLong <- melt(sheetB, id.vars = ("period"))
  sheet1Long <- melt(sheet1, id.vars = ("period"))
  sheet2Long <- melt(sheet2, id.vars = ("period"))
  sheet3Long <- melt(sheet3, id.vars = ("period"))
  sheet4Long <- melt(sheet4, id.vars = ("period"))
  
  AllSheets <- rbind(sheetBLong, sheet1Long, sheet2Long,sheet3Long , sheet4Long )
  AllSheets <- AllSheets[!is.na(AllSheets$value),]
  
  colnames(AllSheets)[2] <- "Subject"
  AllSheets$variable <- variable
  
  AllSheets$period <- ifelse(AllSheets$period == "B", 1, as.numeric(AllSheets$period) +1)
  AllSheets$condition <- ifelse(AllSheets$period == 1, "Baseline period", "Intervention period")
  
  AllSheets$Subject <- substr(AllSheets$Subject, 9, 11)
  
  AllSheets <- AllSheets[,c(2, 1, 5, 4, 3)]
  return(AllSheets)
}
if(FALSE){
  PSQI <- convertLongFormat("PSQI")
  CMAI <- convertLongFormat("CMAI")
  MDS <- convertLongFormat("MDS_ADL")
  CSDD <- convertLongFormat("CSDD")
  
  questionaire <- rbind(PSQI, CMAI, MDS, CSDD )
  
  ###Check subject list 
  
  subListTable <- data.frame(table(questionaire$Subject))
  subListTable$Freq <- subListTable$Freq/4
  
}