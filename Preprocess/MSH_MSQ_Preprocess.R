library(tidyverse)

dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/"
setwd(dir)
data_MSH_MSQ <- readRDS("Reference Tables/data_MSH_MSQ.rds")

#Read COA for department names
COA <- read.csv("Reference Tables/COA.csv",header = T, stringsAsFactors = F)

#Read in job code descriptions
JCdesc <- read.csv("Reference Tables/JCdesc.csv",header = T, stringsAsFactors = F)

#bring in J.C description
data_MSH_MSQ <- left_join(data_MSH_MSQ,JCdesc) 

#bring in department description and Location
data_MSH_MSQ <- left_join(data_MSH_MSQ,COA,by = c("DPT.WRKD" = "Column2")) %>%
  select(1:21)
colnames(data_MSH_MSQ)[20:21] <- c("LOCATION","DESCRIPTION")

#Correct negative hours and expenses
negHours <- grep('-',data_MSH_MSQ$HOURS)
negExpense <- grep('-', data_MSH_MSQ$EXPENSE)
data_MSH_MSQ$HOURS[negHours] <- sub("\\-.*", "",data_MSH_MSQ$HOURS[negHours]) 
data_MSH_MSQ$HOURS[negHours] <- str_trim(data_MSH_MSQ$HOURS[negHours]) 
data_MSH_MSQ$HOURS[negHours] <- paste0("-",data_MSH_MSQ$HOURS[negHours])
data_MSH_MSQ$EXPENSE[negExpense] <- sub("\\-.*", "",data_MSH_MSQ$EXPENSE[negExpense]) 
data_MSH_MSQ$EXPENSE[negExpense] <- str_trim(data_MSH_MSQ$EXPENSE[negExpense]) 
data_MSH_MSQ$EXPENSE[negExpense] <- paste0("-",data_MSH_MSQ$EXPENSE[negExpense])

#preprocess data formats
data_MSH_MSQ <- data_MSH_MSQ %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%m/%d/%Y"),
         PAY.CODE = as.character(PAY.CODE),
         HOURS = gsub(",","",HOURS),
         #HOURS = sub("\\-.*", "", HOURS),
         HOURS = as.numeric(HOURS),
         EXPENSE = gsub(",","",EXPENSE),
         #EXPENSE = sub("\\-.*", "", EXPENSE),
         EXPENSE = as.numeric(EXPENSE),
         PAY.CODE = str_trim(PAY.CODE),
         HOME.SITE = rep(NA,nrow(data_MSH_MSQ)),
         HOME.LOCATION = rep(NA,nrow(data_MSH_MSQ)),
         HOME.DESCRIPTION = rep(NA,nrow(data_MSH_MSQ))) 
colnames(data_MSH_MSQ)[c(3,20,21)] <- c("DPT.HOME", "WRKD.LOCATION","WRKD.DESCRIPTION")

rm(COA,JCdesc,negExpense,negHours)
