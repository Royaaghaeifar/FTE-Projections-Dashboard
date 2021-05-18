dir_universal <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                  "/Productivity/Universal Data")

# Load Libriaries ---------------------------------------------------------
library(readxl)
library(tidyverse)

# Import Data -------------------------------------------------------------
data_MSSL_MSW <- readRDS(paste0(dir_universal, "/Labor/RDS/Data_MSSL_MSW.rds"))

# Import References -------------------------------------------------------
dict_jobcodes_all <- read_xlsx(paste0(dir_universal,
                                      "/Mapping/MSHS_Jobcode_Mapping.xlsx"))
dict_jobcodes <- dict_jobcodes_all %>%
  filter(PAYROLL == "MSMW") %>%
  select(J.C, J.C.DESCRIPTION) %>%
  rename(`Position Code Description` = J.C.DESCRIPTION) %>%
  distinct_at("Position Code Description", .keep_all = T)
dict_jobcodes_MSBIB <- dict_jobcodes_all %>%
  filter(PAYROLL == "MSBIB") %>%
  select(J.C, J.C.DESCRIPTION) %>%
  rename(`Position Code Description` = J.C.DESCRIPTION, J.C_MSBIB = J.C) %>%
  distinct_at("Position Code Description", .keep_all = T)
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
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(WRKD.LOCATION = `Location Description`, HOME.LOCATION = NA)

# Cost Center ("Department") ---------------------------------------------
data_MSSL_MSW$DPT.WRKD <- paste0(data_MSSL_MSW$WD_COFT,
                                 data_MSSL_MSW$WD_Location,
                                 data_MSSL_MSW$WD_Department)
data_MSSL_MSW$DPT.HOME <- paste0(data_MSSL_MSW$HD_COFT,
                                 data_MSSL_MSW$HD_Location,
                                 data_MSSL_MSW$HD_Department)

# Lookup Site -------------------------------------------------------------
data_MSSL_MSW <- merge.data.frame(data_MSSL_MSW, dict_site,
                                  by.x = "Home FacilityOR Hospital ID",
                                  by.y = "Site ID", all.x = T)
colnames(data_MSSL_MSW)[which("Site" == colnames(data_MSSL_MSW))] <- "HOME.SITE"
data_MSSL_MSW <- merge.data.frame(data_MSSL_MSW, dict_site,
                                  by.x = "Facility Hospital Id_Worked",
                                  by.y = "Site ID", all.x = T)
colnames(data_MSSL_MSW)[which("Site" == colnames(data_MSSL_MSW))] <- "WRKD.SITE"


# Rename Columns ---------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  rename(HOURS = Hours,
         EXPENSE = Expense,
         WRKD.DESCRIPTION = `Department Name Worked Dept`,
         HOME.DESCRIPTION = `Department Name Home Dept`,
         PAY.CODE = `Pay Code`,
         J.C.DESCRIPTION = `Position Code Description`,
         END.DATE = `END DATE`,
         LIFE = `Employee ID`)

# Format Columns ----------------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>%
  mutate(END.DATE = as.Date(END.DATE, format = "%m/%d/%Y"),
         PAY.CODE = as.character(PAY.CODE))

# Clear Environment -------------------------------------------------------
rm(dict_jobcodes_all, dict_jobcodes, dict_site, dir_universal,
   dict_jobcodes_MSBIB)
