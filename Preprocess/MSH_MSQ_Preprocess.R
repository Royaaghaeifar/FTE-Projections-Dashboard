library(tidyverse)
library(readxl)
library(rstudioapi)

universal_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                        "Productivity/Universal Data/")

data_MSH_MSQ <- readRDS(paste0(universal_dir,"Labor/RDS/data_MSH_MSQ.rds"))

#Read COA for department names
COA <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/COA.csv",
                header = T, stringsAsFactors = F, strip.white = TRUE)

#Read in job code descriptions
JCdesc <- read_xlsx(paste0(universal_dir,
                           "Mapping/MSHS_Jobcode_Mapping.xlsx")) %>%
  filter(PAYROLL == "MSHQ") %>%
  select(J.C, J.C.DESCRIPTION)

#bring in J.C description
row_count <- nrow(data_MSH_MSQ)
data_MSH_MSQ <- left_join(data_MSH_MSQ,JCdesc)
if(nrow(data_MSH_MSQ) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

#bring in department description and Location
row_count <- nrow(data_MSH_MSQ)
data_MSH_MSQ <- left_join(data_MSH_MSQ,COA,by = c("DPT.WRKD" = "Column2")) %>%
  select(1:22)
if(nrow(data_MSH_MSQ) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

colnames(data_MSH_MSQ)[21:22] <- c("LOCATION","DESCRIPTION")

#Correct negative hours and expenses
negHours <- grep('-',data_MSH_MSQ$HOURS)
negExpense <- grep('-', data_MSH_MSQ$EXPENSE)
data_MSH_MSQ$HOURS[negHours] <-
  sub("\\-.*", "",data_MSH_MSQ$HOURS[negHours])
data_MSH_MSQ$HOURS[negHours] <-
  str_trim(data_MSH_MSQ$HOURS[negHours])
data_MSH_MSQ$HOURS[negHours] <-
  paste0("-",data_MSH_MSQ$HOURS[negHours])
data_MSH_MSQ$EXPENSE[negExpense] <-
  sub("\\-.*", "",data_MSH_MSQ$EXPENSE[negExpense])
data_MSH_MSQ$EXPENSE[negExpense] <-
  str_trim(data_MSH_MSQ$EXPENSE[negExpense])
data_MSH_MSQ$EXPENSE[negExpense] <-
  paste0("-",data_MSH_MSQ$EXPENSE[negExpense])

#preprocess data formats
data_MSH_MSQ <- data_MSH_MSQ %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%m/%d/%Y"),
         PAY.CODE = as.character(PAY.CODE),
         HOURS = gsub(",","",HOURS),
         HOURS = as.numeric(HOURS),
         EXPENSE = gsub(",","",EXPENSE),
         EXPENSE = as.numeric(EXPENSE),
         PAY.CODE = str_trim(PAY.CODE),
         HOME.SITE = rep(NA,nrow(data_MSH_MSQ)),
         HOME.LOCATION = rep(NA,nrow(data_MSH_MSQ)),
         HOME.DESCRIPTION = rep(NA,nrow(data_MSH_MSQ)))
colnames(data_MSH_MSQ)[c(3,21,22)] <- c("DPT.HOME", "WRKD.LOCATION",
                                        "WRKD.DESCRIPTION")

rm(COA, JCdesc, negExpense, negHours, universal_dir)
