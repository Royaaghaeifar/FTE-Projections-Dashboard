library(dplyr)
library(readxl)
library(openxlsx)
library(here)

# file paths
dir_universal <- paste0(
  "J:/deans/Presidents/SixSigma/MSHS Productivity",
  "/Productivity/Universal Data/Mapping/")

dir <- paste0(here(), "/Raw Data/")

# code conversion
code_conversion <- read_xlsx(paste0(
  dir_universal,
  "/MSHS_Code_Conversion_Mapping.xlsx")) %>%
  filter(PAYROLL %in% c("MSH", "MSQ")) %>%
  select(COST.CENTER.LEGACY, COST.CENTER.ORACLE)

## MSQ##
# List files from MSQ Raw folder
folder_msq <- paste0(dir, "MSQ Raw/")
msq_file_list <- list.files(path = folder_msq, pattern = "*.txt")
msq_details <- file.info(list.files(path = folder_msq, pattern = "*.txt",
                                    full.names = T)) %>%
  arrange(mtime)
msq_path_list <- rownames(msq_details)
# Read files in MSQ Raw as csv
msq_list <- lapply(
  msq_path_list,
  function(x) {
    read.csv(x,
      sep = ";", header = T,
      stringsAsFactors = F,
      colClasses = rep("character", 18),
      strip.white = TRUE)})
# Bind all MSQ files and assign their site = "MSQ"
msq <- do.call("rbind", msq_list)
# Remove Duplicate rows
msq <- msq %>% distinct()


## MSH##
# List files from MSH Raw folder
folder_msh <- paste0(dir, "MSH Raw/")
msh_file_list <- list.files(path = folder_msh, pattern = "*.txt")
msh_details <- file.info(list.files(path = folder_msh, pattern = "*.txt",
                                    full.names = T)) %>%
  arrange(mtime)
msh_path_list <- rownames(msh_details)
# Read files in MSQ Raw as csv
msh_list <- lapply(msh_path_list, function(x) read.csv(x, sep = ";", header = T,
                                                       stringsAsFactors = F,
                                                       colClasses = rep(
                                                         "character", 18)))
# Bind all MSQ files and assign their site = "MSQ"
msh <- do.call("rbind", msh_list)
# Remove Duplicate rows
msh <- msh %>% distinct()

# Bind MSH and MSQ
mshq <- rbind(msh, msq) %>%
  mutate(four = substr(DPT.WRKD, 1, 4)) %>%
  mutate(PAYROLL = case_when(
    four == "0130" ~ "MSQ",
    TRUE ~ "MSH")) %>%
  select(c(1:17, 20)) %>%
  left_join(code_conversion, by = c("DPT.WRKD" = "COST.CENTER.LEGACY")) %>%
  mutate(DPT.WRKD = case_when(
    is.na(COST.CENTER.ORACLE) ~ DPT.WRKD,
    TRUE ~ COST.CENTER.ORACLE))

# filter data
data_msh_msq <- mshq %>%
  filter(as.Date(END.DATE, format = "%m/%d/%Y") <=
           as.Date("03/21/2020", format = "%m/%d/%Y"))
#save RDS
saveRDS(data_msh_msq, file = paste0("J:/deans/Presidents/SixSigma/",
                                    "MSHS Productivity/Productivity/",
                                    "Universal Data/Labor/RDS/",
                                    "data_MSH_MSQ.rds"))
