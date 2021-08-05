library(dplyr)
library(readxl)

#universal directory
universal_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                        "Productivity/Universal Data/")

#Read in raw MSHQ Oracle raw refresh
data_MSH_MSQ_oracle <- readRDS(paste0(universal_dir,
                                      "Labor/RDS/data_MSH_MSQ_oracle.rds"))

#Read COA for department location
COA <- read.csv("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/COA.csv",
                header = T, stringsAsFactors = F, strip.white = TRUE)

#Read in job code descriptions
JCdesc <- read_xlsx(paste0(universal_dir,
                           "Mapping/MSHS_Jobcode_Mapping.xlsx")) %>%
  filter(PAYROLL == "MSHQ") %>%
  select(J.C, J.C.DESCRIPTION)

#Replace departments that failed GEAC map
oracle <- data_MSH_MSQ_oracle %>%
  mutate(Reverse.Map.for.Worked = case_when(
    Reverse.Map.for.Worked == "" ~ Department.IdWHERE.Worked,
    TRUE ~ Reverse.Map.for.Worked),
    Reverse.Map.for.Home = case_when(
      Reverse.Map.for.Home == "" ~ paste0(HD_COFT,HD_Location,HD_Department),
      TRUE ~ Reverse.Map.for.Home))

#Take first 8 digits of Home and Worked department
oracle <- oracle %>%
  mutate(
    Reverse.Map.for.Worked =
      case_when(
        nchar(Reverse.Map.for.Worked) == 12 ~
          substr(Reverse.Map.for.Worked,1,8),
        TRUE ~ Reverse.Map.for.Worked),
    Reverse.Map.for.Home =
      case_when(
        nchar(Reverse.Map.for.Home) == 12 ~
          substr(Reverse.Map.for.Home,1,8),
        TRUE ~ Reverse.Map.for.Home))

#Bring in department location
oracle <- left_join(oracle, COA,
                    by = c("Reverse.Map.for.Worked" = "Column2")) %>%
  select(1:35)
oracle <- left_join(oracle, COA,
                    by = c("Reverse.Map.for.Home" = "Column2")) %>%
  select(1:36)

#Bring in standardized JC Description
oracle <- left_join(oracle,JCdesc, by = c("Job.Code" = "J.C"))

#Format necessary columns
oracle <- oracle %>%
  mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
         Hours = as.numeric(Hours),
         Expense = as.numeric(Expense))
#Column names
colnames(oracle)[c(6,7,12:15,29,30,32,33,35:37)] = c("START.DATE","END.DATE",
                                                     "J.C","PAY.CODE",
                                                     "HOME.DESCRIPTION",
                                                     "WRKD.DESCRIPTION",
                                                     "DPT.WRKD","DPT.HOME",
                                                     "HOURS","EXPENSE",
                                                     "WRKD.LOCATION",
                                                     "HOME.LOCATION",
                                                     "J.C.DESCRIPTION")

data_MSH_MSQ_oracle <<- oracle

rm(COA, JCdesc, oracle, universal_dir)
