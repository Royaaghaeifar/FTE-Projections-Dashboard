dir_reference <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement"
dir_new <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                  "/Productivity/Analysis/FTE Projections Dashboard")

# Load Libriaries ---------------------------------------------------------
library(readxl)
library(tidyverse)

# Import Data -------------------------------------------------------------
data_MSSL_MSW <- readRDS(paste0(dir_reference,"/MSLW RAW/Data_MSSL_MSW.rds"))

# Import References -------------------------------------------------------
folder_references <- paste0(dir_reference,"/MSLW Reference Tables")
#dict_paycycle_alt <- read_xlsx(paste0(folder_references, "/Dictionary_Alt Pay Cycles.xlsx"))
dict_jobcodes <- read_xlsx(paste0(folder_references, "/MSLW Job Codes.xlsx"))
dict_jobcodes <- dict_jobcodes %>% distinct()
dict_jobcodes_MSBIB <- read_xlsx(paste0(dir_reference, '/MSBIB Reference/MSBI Job Code Dictionary.xlsx'), sheet = "Dictionary")
dict_jobcodes_MSBIB <- dict_jobcodes_MSBIB %>% 
  select(`Job Description`, `Job code`) %>% 
  rename(`Position Code Description` = `Job Description`,
         J.C_MSBIB = `Job code`) %>% distinct()
dict_COFTloc <- read_xlsx(paste0(folder_references, "/Dictionary_COFT.xlsx")) #this will be from matts excel file
dict_site <- as.data.frame(cbind(c("NY2162", "NY2163"), c("MSW", "MSSL")),
                           stringsAsFactors = F)
colnames(dict_site) <- c("Site ID", "Site")

# Lookup Jobcodes ---------------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(`Position Code Description` = case_when(
    is.na(`Position Code Description`) ~ "OTHER",
    TRUE ~ `Position Code Description`))
data_MSSL_MSW <- left_join(data_MSSL_MSW, dict_jobcodes) 
data_MSSL_MSW <- left_join(data_MSSL_MSW, dict_jobcodes_MSBIB)
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(J.C = case_when(
    is.na(J.C) ~ J.C_MSBIB,
    TRUE ~ J.C), 
    J.C_MSBIB = NULL)

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
rm(folder_references, dict_COFTloc, dict_jobcodes, dict_site, dir_reference, dict_jobcodes_MSBIB)
