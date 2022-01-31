# Intro -------------------------------------------------------------------

# Libraries Setup and Memory Clear-----------------------------------------

library(dplyr)
library(readxl)
library(rstudioapi)

univ_ref <- paste0(
  "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/",
  "Universal Data/"
)

data_MSBI_MSB <- readRDS(paste0(univ_ref, "Labor/RDS/data_MSBI_MSB.rds"))

# Inputs/Imports ----------------------------------------------------------

coft_desc <- read_excel(
  paste0(univ_ref, "Mapping/MSBIB_Legacy COFT Descriptions.xlsx"),
  sheet = "COFT_TABLE"
)
simp_loc <- read_excel(
  paste0(univ_ref, "Mapping/MSBIB_Legacy COFT Descriptions.xlsx"),
  sheet = "COFT_TABLE_SIMP"
)
loc_desc <- read_excel(
  paste0(univ_ref, "Mapping/MSBIB_Legacy COFT Descriptions.xlsx"),
  sheet = "LOC_TABLE"
)

jc_dict <- read_excel(
  paste0(univ_ref, "Mapping/MSHS_Jobcode_Mapping.xlsx")
)

jc_dict <- distinct_at(jc_dict, vars(c(PAYROLL, J.C.DESCRIPTION)), .keep_all = TRUE)

# Data Transformations ----------------------------------------------------

payroll_data_process <- data_MSBI_MSB

payroll_data_process$HD_CO <- substr(payroll_data_process$HD_COFT, 1, 2)
payroll_data_process$WD_CO <- substr(payroll_data_process$WD_COFT, 1, 2)

# payroll_data_process$DPT.WRKD <- paste0(
#   payroll_data_process$WD_COFT, payroll_data_process$WD_Location,
#   payroll_data_process$WD_Department
# )
# payroll_data_process$DPT.HOME <- paste0(
#   payroll_data_process$HD_COFT, payroll_data_process$HD_Location,
#   payroll_data_process$HD_Department
# )

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process, coft_desc,
  by.x = "WD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    WRKD.COFT.DESC = COFT_Description
  )

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process, coft_desc,
  by.x = "HD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    HOME.COFT.DESC = COFT_Description
  )

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process, simp_loc,
  by.x = "WD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    WRKD.LOCATION = COFT_LOC_GROUP
  )

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process, simp_loc,
  by.x = "HD_COFT", by.y = "COFT",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    HOME.LOCATION = COFT_LOC_GROUP
  )

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process, loc_desc,
  by.x = "WD_Location", by.y = "Location",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    cc_wd_loc = LOC_Name
  )

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process, loc_desc,
  by.x = "HD_Location", by.y = "Location",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    cc_hd_loc = LOC_Name
  )

payroll_data_process <- payroll_data_process %>%
  mutate(`Position Code Description` = case_when(
    is.na(`Position Code Description`) ~ "OTHER",
    TRUE ~ `Position Code Description`))

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process,
  jc_dict %>%
    filter(PAYROLL == "MSBIB") %>%
    select("J.C.DESCRIPTION", "J.C"),
  by.x = "Position Code Description", by.y = "J.C.DESCRIPTION",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}

#Checking row count before join
row_count <- nrow(payroll_data_process)
payroll_data_process <- merge(
  payroll_data_process,
  jc_dict %>%
    filter(PAYROLL == "MSMW") %>%
    select("J.C.DESCRIPTION", "J.C"),
  by.x = "Position Code Description", by.y = "J.C.DESCRIPTION",
  all.x = T, all.y = F
)
#If rows added during left join stop executing code
if(nrow(payroll_data_process) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}


payroll_data_process <- payroll_data_process %>%
  mutate(J.C.x = case_when(
    is.na(J.C.x) ~ J.C.y,
    TRUE ~ J.C.x),
    J.C.y = NULL)

payroll_data_process <- payroll_data_process %>%
  dplyr::rename(
    HOME.DESCRIPTION = "Department Name Home Dept",
    WRKD.DESCRIPTION = "Department Name Worked Dept",
    PAY.CODE = "Pay Code",
    LIFE = "Employee ID",
    END.DATE = "END DATE",
    HOURS = Hours,
    EXPENSE = Expense,
    J.C = J.C.x,
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

rm(coft_desc, jc_dict, loc_desc, payroll_data_process, simp_loc, dir_ref, dir, univ_ref)
