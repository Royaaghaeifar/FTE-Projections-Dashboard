#Rightsourcing
library(tidyverse)
library(readxl)

dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/"
setwd(dir)

#Read most recent system rightsourcing file
right <- readRDS("Rightsourcing Raw/Rightsourcing.rds")

#bring in jobcodes from rightsourcing script
JCList <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Labor - Data/Rightsourcing Labor/Rightsource Job Code.csv",stringsAsFactors = F,header=F)
JCList <- JCList %>%
  mutate(J.C.DESCRIPTION = substr(JCList$V1, start = 15, stop = nchar(JCList$V1)))
colnames(JCList)[2] <- "J.C"
right <- left_join(right,JCList, by = c("Job.Title" = "J.C.DESCRIPTION"))

#Read COA for department names
COA <- read.csv("Reference Tables/COA.csv",header = T, stringsAsFactors = F)
Right_Coft <- read.csv("Reference Tables/Right_COFT.csv",header = T, stringsAsFactors = F)
Right_Coft <- Right_Coft %>% mutate(COFT = as.character(COFT))
DepDict <- read.csv("Reference Tables/DepDict.csv",header = T,stringsAsFactors = F)
DepDict <- DepDict %>% mutate(cc = as.character(cc))

#Adjust all Cost Centers  
library(stringr)
right$Dept.[str_length(right$Dept.)==12] <- str_sub(right$Dept.[str_length(right$Dept.)==12],1,8)

right$Dept.[str_length(right$Dept.)==30] <- str_c(
  str_sub(right$Dept.[str_length(right$Dept.)==30], 1, 4),
  str_sub(right$Dept.[str_length(right$Dept.)==30], 13, 14),
  str_sub(right$Dept.[str_length(right$Dept.)==30], 16, 19))

right$Dept.[str_length(right$Dept.)==32] <- str_c(
  str_sub(right$Dept.[str_length(right$Dept.)==32], 1, 4),
  str_sub(right$Dept.[str_length(right$Dept.)==32], 14, 15),
  str_sub(right$Dept.[str_length(right$Dept.)==32], 17, 20))

#Add Location and Description based off cost center
right <- right %>%
  mutate(COFT = str_sub(Dept.,1,4))
#MSBIBSLW Location
right <- left_join(right,Right_Coft)
#MSBIBSLW Description
right <- left_join(right,DepDict, by = c("Dept."="cc"))
#MSHQ Location and Description
right <- left_join(right,COA, by = c("Dept." = "Column2")) %>%
  select(1:27)
WRKD.LOCATION <- rep(NA,nrow(right))
HOME.LOCATION <- rep(NA,nrow(right))
WRKD.DESCRIPTION <- rep(NA,nrow(right))
HOME.DESCRIPTION <- rep(NA,nrow(right))
for(i in 1:nrow(right)){
  if(!is.na(right$Column1)[i]){
    WRKD.LOCATION[i] <- right$Column1[i]
    HOME.LOCATION[i] <- right$Column1[i]
    WRKD.DESCRIPTION[i] <- right$Column3[i]
    HOME.DESCRIPTION[i] <- right$Column3[i]
  } else {
    WRKD.LOCATION[i] <- right$COFT_LOC_GROUP[i]
    HOME.LOCATION[i] <- right$COFT_LOC_GROUP[i]
    WRKD.DESCRIPTION[i] <- right$Name[i]
    HOME.DESCRIPTION[i] <- right$Name[i]
  }
}
#Add new description and location vectors to right table
right <- right %>% mutate(WRKD.LOCATION = WRKD.LOCATION,
                          HOME.LOCATION = HOME.LOCATION,
                          WRKD.DESCRIPTION = WRKD.DESCRIPTION,
                          HOME.DESCRIPTION = HOME.DESCRIPTION)

#Format all necessary columns
data_Rightsourcing <- right %>%
  mutate(
    END.DATE = as.Date(Earnings.E.D, format = "%m/%d/%Y"),
    PAY.CODE = "AG1",
    PAYROLL = "Rightsourcing",
    DPT.HOME = Dept.,
    DPT.WRKD = Dept.,
    HOURS = Hours,
    EXPENSE = Spend) %>%
  filter(END.DATE > as.Date("01/01/2019", format = "%m/%d/%Y")) %>%
  select(c(7,22,28:38)) 
colnames(data_Rightsourcing)[1] <- "J.C.DESCRIPTION"

rm(right,JCList,COA,DepDict,Right_Coft,HOME.DESCRIPTION,HOME.LOCATION,i,WRKD.DESCRIPTION,WRKD.LOCATION)

