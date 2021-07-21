
# Load Libraries ----------------------------------------------------------
library(readxl)
library(xlsx)
library(tidyverse)
library(here)

# Constants ---------------------------------------------------------------
dir_universal <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                        "/Productivity/Universal Data")
dir_files <- paste0(here(),"/MSMW Legacy/MSMW Legacy")

# Import Data -------------------------------------------------------------
list_data_files <- list.files(dir_files, pattern = "xlsx$", full.names = T)
read_xlsx_files <- function(filename){
  dat <- read_xlsx(filename, sheet= "Export Worksheet")
  l <- length(unlist(strsplit(filename, split = "/", fixed = T)))
  name <- unlist(strsplit(filename, split = "/", fixed = T))[l]
  dat$Source <- name
  return(dat)
}
list_data <-lapply(list_data_files, function(x) read_xlsx_files(x))
list_data <- do.call("rbind", list_data)

# Preprocessing Data ------------------------------------------------------
data_RAW <- list_data %>% 
  mutate(`START DATE`= paste0(substr(`START DATE`,1,2), "/",
                              substr(`START DATE`,3,4), "/",
                              substr(`START DATE`,5,8)),
         `START DATE` = as.Date(`START DATE`, "%m/%d/%Y"),
         `END DATE` = paste0(substr(`END DATE`,1,2), "/",
                             substr(`END DATE`,3,4), "/",
                             substr(`END DATE`,5,8)),
         `END DATE` = as.Date(`END DATE`, "%m/%d/%Y"),
         `Start-End` = paste0(`START DATE`, "-", `END DATE`))
#Filtering each file by dates uploaded into Premier
data_RAW_a <- data_RAW %>% 
  filter(Source == "MSSLW_JAN_DEC19 and JAN_APR20 (1).xlsx"| Source == "MSSLW_JAN_DEC19 and JAN_APR20 (2).xlsx",
         `END DATE` <= as.Date('2020-03-28'))
data_RAW_b <- data_RAW %>% 
  filter(Source == "FEMA_MSSLW_APR_MAY_JUN_2020.xlsx",
         `END DATE` >= as.Date('2020-04-04') | `END DATE` < as.Date('2020-06-06'))
data_RAW_c <- data_RAW %>% 
  filter(Source == "FEMA_MSSLW_JUN_JUL_AUG_2020.xlsx",
         `END DATE` >= as.Date('2020-06-06'),
         `END DATE` < as.Date('2020-08-08'))
data_RAW_d <- data_RAW %>% 
  filter(Source == 'FEMA_MSSLW_JUL_AUG_SEP_2020.xlsx',
         `END DATE` >= as.Date('2020-08-08'),
         `END DATE` < as.Date('2020-08-29'))
data_RAW_f <- data_RAW %>% 
  filter(Source == 'FEMA_MSSLW_OCT20.xlsx',
         `END DATE` >= as.Date('2020-08-29'),
         `END DATE` < as.Date('2020-10-31'))
data_RAW_g <- data_RAW %>%
  filter(Source == "FEMA_MSSLW_NOV_DEC_20.xlsx",
         `END DATE` >= as.Date('2020-10-31'),
         `END DATE` < as.Date('2020-11-28'))
data_RAW_h <- data_RAW %>%
  filter(Source == "FEMA_MSSLW_NOV_JAN_21.xlsx",
         `END DATE` >= as.Date('2020-11-28'),
         `END DATE` < as.Date('2020-12-31'))
data_RAW_i <- data_RAW %>%
  filter(Source == 'FEMA_MSSLW_JAN2021.xlsx',
         `END DATE` >= as.Date('2020-12-31'),
         `END DATE` < as.Date('2021-02-07'))
data_RAW_j <- data_RAW %>%
  filter(Source == 'FEMA_MSSLW_FEB2021.xlsx',
         `END DATE` >= as.Date('2021-02-07'),
         `END DATE` < as.Date('2021-03-07'))
#Combining all the files together
data_final <- rbind(data_RAW_a, data_RAW_b, data_RAW_c, data_RAW_d,data_RAW_f,data_RAW_g,data_RAW_h,data_RAW_i, data_RAW_j)
data_final$`Start-End` <- data_final$Source <- NULL

# Add Payroll Source ------------------------------------------------------
dict_payroll <- data.table::data.table(PAYROLL = c('MSW', 'MSM'), `Facility Hospital Id_Worked` = c ('NY2162', 'NY2163'))
data_MSSL_MSW <-left_join(data_final, dict_payroll)

# Remove Duplicates -------------------------------------------------------
data_MSSL_MSW <- data_MSSL_MSW %>% distinct()

# Save Data ---------------------------------------------------------------
saveRDS(data_MSSL_MSW, paste0(dir_universal,"/Labor/RDS/Data_MSSL_MSW.rds"))
