# Intro -------------------------------------------------------------------

# Libraries Setup and Memory Clear-----------------------------------------

library(dplyr)
library(readxl)

dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/"
dir_ref <- paste0(dir, "MSBIB Reference/")

data_MSBI_MSB <- readRDS(paste0(dir, "Reference Tables/data_MSBI_MSB.rds"))

# Inputs/Imports ----------------------------------------------------------

coft_desc <- read_excel(
  paste0(dir_ref, "COFT Descriptions.xlsx"),
  sheet = "COFT_TABLE"
)
simp_loc <- read_excel(
  paste0(dir_ref, "COFT Descriptions.xlsx"),
  sheet = "COFT_TABLE_SIMP"
)
loc_desc <- read_excel(
  paste0(dir_ref, "COFT Descriptions.xlsx"),
  sheet = "LOC_TABLE"
)
jc_dict <- read_excel(
  paste0(dir_ref, "MSBI Job Code Dictionary.xlsx")
)
jc_dict_MSLW <- read_excel(paste0(dir, "MSLW Reference Tables/MSLW Job Codes.xlsx"))
jc_dict_MSLW <- jc_dict_MSLW %>% distinct()

# Data Transformations ----------------------------------------------------

payroll_data_process <- data_MSBI_MSB

payroll_data_process$HD_CO <- substr(payroll_data_process$HD_COFT, 1, 2)
payroll_data_process$WD_CO <- substr(payroll_data_process$WD_COFT, 1, 2)

payroll_data_process$DPT.WRKD <- paste0(
  payroll_data_process$WD_COFT, payroll_data_process$WD_Location,
  payroll_data_process$WD_Department
)
payroll_data_process$DPT.HOME <- paste0(
  payroll_data_process$HD_COFT, payroll_data_process$HD_Location,
  payroll_data_process$HD_Department
)

payroll_data_process <- merge(
  payroll_data_process, coft_desc,
  by.x = "WD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    WRKD.COFT.DESC = COFT_Description
  )

payroll_data_process <- merge(
  payroll_data_process, coft_desc,
  by.x = "HD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    HOME.COFT.DESC = COFT_Description
  )

payroll_data_process <- merge(
  payroll_data_process, simp_loc,
  by.x = "WD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    WRKD.LOCATION = COFT_LOC_GROUP
  )

payroll_data_process <- merge(
  payroll_data_process, simp_loc,
  by.x = "HD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    HOME.LOCATION = COFT_LOC_GROUP
  )

payroll_data_process <- merge(
  payroll_data_process, loc_desc,
  by.x = "WD_Location", by.y = "Location",
  all.x = T, all.y = F
)
payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    cc_wd_loc = LOC_Name
  )

payroll_data_process <- merge(
  payroll_data_process, loc_desc,
  by.x = "HD_Location", by.y = "Location",
  all.x = T, all.y = F
)
payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    cc_hd_loc = LOC_Name
  )

payroll_data_process <- payroll_data_process %>%
  mutate(`Position Code Description` = case_when(
    is.na(`Position Code Description`) ~ "OTHER",
    TRUE ~ `Position Code Description`))

payroll_data_process <- merge(
  payroll_data_process, select(jc_dict, "Job Description", "Job code"),
  by.x = "Position Code Description", by.y = "Job Description",
  all.x = T, all.y = F
)
payroll_data_process <- merge(
  payroll_data_process, select(jc_dict_MSLW, "Position Code Description", 'J.C'),
  by.x = 'Position Code Description', by.y = 'Position Code Description',
  all.x = T, all.y = F
)
payroll_data_process <- payroll_data_process %>%
  mutate(`Job code` = case_when(
    is.na(`Job code`) ~ J.C,
    TRUE ~ `Job code`),
    J.C = NULL)


payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    HOME.DESCRIPTION = "Department Name Home Dept",
    WRKD.DESCRIPTION = "Department Name Worked Dept",
    PAY.CODE = "Pay Code",
    LIFE = "Employee ID",
    END.DATE = "END DATE",
    HOURS = Hours,
    EXPENSE = Expense,
    J.C = "Job code",
    J.C.DESCRIPTION = "Position Code Description"
  )

payroll_data_process$PAY.CODE <- as.character(payroll_data_process$PAY.CODE)

payroll_data_process$END.DATE <- paste0(
  substr(payroll_data_process$END.DATE, 1, 2), "/",
  substr(payroll_data_process$END.DATE, 3, 4), "/",
  substr(payroll_data_process$END.DATE, 5, 8)
) %>%
  as.Date(format = "%m/%d/%Y")

data_MSBI_MSB <- payroll_data_process

# Outputs/Exports ---------------------------------------------------------

rm(coft_desc, jc_dict, loc_desc, payroll_data_process, simp_loc, dir_ref, jc_dict_MSLW)