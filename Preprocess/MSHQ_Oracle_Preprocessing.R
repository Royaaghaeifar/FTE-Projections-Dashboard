library(dplyr)

data_MSH_MSQ_oracle <- readRDS("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/data_MSH_MSQ_oracle.rds")

#Read COA for department location
COA <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/COA.csv",header = T, stringsAsFactors = F)

#Read in job code descriptions
JCdesc <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/JCdesc.csv",header = T, stringsAsFactors = F)

#Remove NA worked department
oracle <- data_MSH_MSQ_oracle %>%
  filter(Reverse.Map.for.Worked != "")

#Blank home department becomes worked department
Blank <- oracle %>%
  filter(Reverse.Map.for.Home == "") %>%
  mutate(Reverse.Map.for.Home = Reverse.Map.for.Worked)
oracle <- rbind(filter(oracle,Reverse.Map.for.Home != ""),Blank)

#Take first 8 digits of Home and Worked department
oracle <- oracle %>%
  mutate(Reverse.Map.for.Worked = substr(Reverse.Map.for.Worked,1,8),
         Reverse.Map.for.Home = substr(Reverse.Map.for.Home,1,8))

#Bring in department location
oracle <- left_join(oracle,COA, by = c("Reverse.Map.for.Worked" = "Column2")) %>%
  select(1:35)
oracle <- left_join(oracle,COA, by = c("Reverse.Map.for.Home" = "Column2")) %>%
  select(1:36)

#Bring in standardized JC Description
oracle <- left_join(oracle,JCdesc, by = c("Job.Code" = "J.C"))

#Format necessary columns
oracle <- oracle %>% 
  mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
         Hours = as.numeric(Hours),
         Expense = as.numeric(Expense))
#Column names
colnames(oracle)[c(6,7,12:15,29,30,32,33,35:37)] = c("START.DATE","END.DATE","J.C","PAY.CODE","HOME.DESCRIPTION",
                                               "WRKD.DESCRIPTION","DPT.WRKD","DPT.HOME","HOURS","EXPENSE",
                                               "WRKD.LOCATION","HOME.LOCATION","J.C.DESCRIPTION")
#Only take necessary columns
data_MSH_MSQ_oracle <- select(oracle,c(6,7,12:15,29,30,32:37))

rm(Blank,COA,JCdesc,oracle)
