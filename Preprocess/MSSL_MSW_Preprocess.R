#dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement"
setwd(dir)

# Load Libriaries ---------------------------------------------------------
library(readxl)
library(tidyverse)

# Import Data -------------------------------------------------------------
data_MSSL_MSW <- readRDS(paste0(dir,"/MSLW RAW/Data_MSSL_MSW.rds"))

## Preprocess Data ------------------------------------------------------
#data_MSSL_MSW <- data_MSSL_MSW %>% mutate(`START DATE` = paste0(substr(`START DATE`,1,2), "/", substr(`START DATE`,3,4), "/",substr(`START DATE`,5,8)),
#                                          `START DATE` = as.Date(`START DATE`, "%m/%d/%Y"),
#                                          `END DATE` = paste0(substr(`END DATE`,1,2), "/", substr(`END DATE`,3,4), "/",substr(`END DATE`,5,8)),
#                                          `END DATE` = as.Date(`END DATE`, "%m/%d/%Y"))

# Import References -------------------------------------------------------
folder_references <- paste0(dir,"/MSLW Reference Tables")
dict_paycycle_alt <- read_xlsx(paste0(folder_references, "/Dictionary_Alt Pay Cycles.xlsx"))
dict_jobcodes <- read_xlsx(paste0(folder_references, "/MSLW Job Codes.xlsx"))
dict_COFTloc <- read_xlsx(paste0(folder_references, "/Dictionary_COFT.xlsx")) #this will be from matts excel file
dict_site <- read_xlsx(paste0(folder_references, '/Dictionary_Site.xlsx'))

## Splitting Biweekly 2 Pay Cycle ------------------------------------------
#format_biweekly_paycycle <- function(dfs){
#  dict_paycycle_alt$`Start-End` <- paste0(dict_paycycle_alt$`START DATE`, "-", dict_paycycle_alt$`END DATE`)
#  dfs$`Start-End` <- paste0(dfs$`START DATE`, "-", dfs$`END DATE`)
#  dfs_other <- dfs[!(dfs$`Start-End` %in% dict_paycycle_alt$`Start-End`),]
#  dfs_alt <- dfs[dfs$`Start-End` %in% dict_paycycle_alt$`Start-End`,]
#  dfs_alt$Hours <- dfs_alt$Hours/2
#  dfs_alt$Expense <- dfs_alt$Expense/2
#  dfs_alt_1 <- merge.data.frame(dfs_alt, subset(dict_paycycle_alt, select = c('Start-End', 'Start 1', 'End 1')), all.x = T)
#  dfs_alt_2 <- merge.data.frame(dfs_alt, subset(dict_paycycle_alt, select = c('Start-End', 'Start 2', 'End 2')), all.x = T)
#  dfs_alt_1$`START DATE` <- dfs_alt_1$`END DATE` <- dfs_alt_2$`START DATE` <- dfs_alt_2$`END DATE` <- NULL
#  colnames(dfs_alt_1)[which("Start 1"==colnames(dfs_alt_1))] <- "START DATE"
#  colnames(dfs_alt_1)[which("End 1"==colnames(dfs_alt_1))] <- "END DATE"
#  colnames(dfs_alt_2)[which("Start 2"==colnames(dfs_alt_2))] <- "START DATE"
#  colnames(dfs_alt_2)[which("End 2"==colnames(dfs_alt_2))] <- "END DATE"
#  dfs_final <- rbind(dfs_other, dfs_alt_1, dfs_alt_2)
#  dfs_final$`Start-End` <- NULL
#  return(dfs_final)
#  }
#data_MSSL_MSW <- format_biweekly_paycycle(data_MSSL_MSW)

# Lookup Jobcodes ---------------------------------------------------------
data_MSSL_MSW <- merge.data.frame(data_MSSL_MSW, dict_jobcodes, all.x = T)

# Lookup Location ---------------------------------------------------------
data_MSSL_MSW <- merge.data.frame(data_MSSL_MSW, dict_COFTloc, by.x = "WD_COFT", by.y ="COFT" , all.x = T)
colnames(data_MSSL_MSW)[which("COFT_LOC_GROUP"==colnames(data_MSSL_MSW))] <- 'WRKD.LOCATION'
data_MSSL_MSW <- merge.data.frame(data_MSSL_MSW, dict_COFTloc, by.x = "HD_COFT", by.y ="COFT" , all.x = T)
colnames(data_MSSL_MSW)[which("COFT_LOC_GROUP"==colnames(data_MSSL_MSW))] <- 'HOME.LOCATION'

# Cost Center ("Department") ---------------------------------------------
data_MSSL_MSW$DPT.WRKD <- paste0(data_MSSL_MSW$WD_COFT, data_MSSL_MSW$WD_Location, data_MSSL_MSW$WD_Department)
data_MSSL_MSW$DPT.HOME <- paste0(data_MSSL_MSW$HD_COFT, data_MSSL_MSW$HD_Location, data_MSSL_MSW$HD_Department)

# Lookup Site -------------------------------------------------------------
data_MSSL_MSW <- merge.data.frame(data_MSSL_MSW,dict_site, by.x = "Home FacilityOR Hospital ID", by.y = 'Site ID', all.x = T)
colnames(data_MSSL_MSW)[which("Site"==colnames(data_MSSL_MSW))] <- 'HOME.SITE'
data_MSSL_MSW <- merge.data.frame(data_MSSL_MSW,dict_site, by.x = "Facility Hospital Id_Worked", by.y = 'Site ID', all.x = T)
colnames(data_MSSL_MSW)[which("Site"==colnames(data_MSSL_MSW))] <- 'WRKD.SITE'

# Rename Columns ---------------------------------------------------
colnames(data_MSSL_MSW)[which("Hours"==colnames(data_MSSL_MSW))] <- 'HOURS'
colnames(data_MSSL_MSW)[which("Expense"==colnames(data_MSSL_MSW))] <- 'EXPENSE'
colnames(data_MSSL_MSW)[which("Department Name Worked Dept"==colnames(data_MSSL_MSW))] <- "WRKD.DESCRIPTION"
colnames(data_MSSL_MSW)[which("Department Name Home Dept"==colnames(data_MSSL_MSW))] <- "HOME.DESCRIPTION"
colnames(data_MSSL_MSW)[which('Pay Code'==colnames(data_MSSL_MSW))] <- "PAY.CODE"
colnames(data_MSSL_MSW)[which("Position Code Description"==colnames(data_MSSL_MSW))] <- "J.C.DESCRIPTION"
colnames(data_MSSL_MSW)[which('END DATE'==colnames(data_MSSL_MSW))] <- "END.DATE"
colnames(data_MSSL_MSW)[which('Employee ID'==colnames(data_MSSL_MSW))] <- "LIFE"

# Format Columns ----------------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%m/%d/%Y"),
         PAY.CODE = as.character(PAY.CODE))

# Clear Environment -------------------------------------------------------
rm(folder_references, dict_COFTloc, dict_jobcodes, dict_paycycle_alt, dict_site, format_biweekly_paycycle)
