library(dplyr)
library(tidyr)
library(here)

##MSHQ##
#List files from MSQ Raw folder
PPend_list <- list("04/25/2020","05/23/2020","06/20/2020","08/01/2020","08/29/2020","09/26/2020","10/24/2020","11/21/2020","01/02/2021","01/23/2021","01/30/2021")
folderOracle <- paste0(here(),"/Raw Data/MSHQ Oracle/")     
Oracle_file_list <- list.files(path=folderOracle, pattern="*.txt")
details = file.info(list.files(path = folderOracle, pattern="*.txt", full.names = T)) %>% arrange(mtime)
Oracle_file_list <- rownames(details)
#Read files in MSQ Raw as csv
ORACLElist = lapply(Oracle_file_list, function(x)read.csv(x, sep = "~", header=T, stringsAsFactors = F,colClasses = rep("character",32)))
#Remove overlapping dates from raw txt files
for(i in 1:length(ORACLElist)){
  if(i == 1 | i == 11){
    ORACLElist[[i]] <- ORACLElist[[i]] %>%
      filter(as.Date(End.Date, format = "%m/%d/%Y") <= as.Date(PPend_list[[i]], format = "%m/%d/%Y"))
  } else if(i == 2){
    ORACLElist[[i]] <- ORACLElist[[i]] %>%
      filter(as.Date(End.Date, format = "%m/%d/%Y") <= as.Date(PPend_list[[i]], format = "%m/%d/%Y"))
  } else {
    ORACLElist[[i]] <- ORACLElist[[i]] %>%
      filter(as.Date(End.Date, format = "%m/%d/%Y") <= as.Date(PPend_list[[i]], format = "%m/%d/%Y"),
             as.Date(Start.Date, format = "%m/%d/%Y") > max(as.Date(ORACLElist[[i-1]]$End.Date,format = "%m/%d/%Y")))
  }
}
#Bind all MSHQ files
Oracle = do.call("rbind", ORACLElist)
#Remove Duplicate rows and add worked entity column
Oracle <- Oracle %>% 
  mutate(WRKD.ENTITY = substr(Oracle$WD_COFT,1,3),
         Hours = as.numeric(Hours),
         Expense = as.numeric(Expense))
Oracle <- Oracle%>%
  group_by_at(c(1:13,16:33)) %>%
  summarise(Hours = sum(Hours, na.rm = T),
            Expense = sum(Expense,na.rm = T)) %>%
  ungroup() %>%
  distinct()
#Determine PAYROLL based on WRKD.ENTITY
Oracle_test <- Oracle %>%
  mutate(PAYROLL = case_when(
    WRKD.ENTITY == "102" ~ "MSQ",
    TRUE ~ "MSH"))


#Check sum of hours by end date to make sure data follows proper pattern
check <- Oracle %>% ungroup() %>% group_by(PAYROLL,End.Date) %>% summarise(Hours = sum(Hours)) %>%pivot_wider(id_cols = PAYROLL,values_from = Hours,names_from = End.Date)
#Save .rds
saveRDS(Oracle,file = "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/data_MSH_MSQ_oracle.rds")
