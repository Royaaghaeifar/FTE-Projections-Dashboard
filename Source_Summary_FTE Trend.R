Source_Func <- function(x){
  source(paste0(getwd(),"/Preprocess/",x))
  setwd(dir)
}

Source_Summary <- function(data){
  library(readxl)
  #Read paycode mapping file and Pay cycle file
  System_Paycode <- read_xlsx("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/All Sites Pay Code Mappings.xlsx")
  colnames(System_Paycode) <- c("PAY.CODE","PAY.CODE.NAME","PAY.CODE.MAPPING","INCLUDE.HOURS","INCLUDE.EXPENSES","JODI","JODI.NO.PTO")
  System_Paycode <- System_Paycode %>%
    mutate(PAY.CODE = str_trim(PAY.CODE)) 
  for(i in 1:nrow(System_Paycode)){
    if(nchar(System_Paycode$PAY.CODE)[i] == 1){
      System_Paycode$PAY.CODE[i] <- paste0("0", System_Paycode$PAY.CODE[i])
    }
  }
  
  #Read in paycycle
  PayCycle <- read_excel("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Useful Tools & Templates/Pay Cycle Calendar.xlsx")
  PayCycle <- PayCycle %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    mutate(Start.Date = as.Date(Start.Date, format = "%m/%d/%Y")) %>%
    mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"))
  
  #Begin summarization
  Department <- data %>%
    group_by(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,END.DATE) %>%
    summarise(HOURS = sum(HOURS, na.rm = T), EXPENSE = sum(EXPENSE, na.rm = T))
  
  #assign pp end dates and summarize
  Summary <- left_join(Department,PayCycle,by = c("END.DATE" = "Date")) %>%
    select(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,End.Date,HOURS,EXPENSE) %>%
    group_by(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,End.Date) %>%
    summarize(HOURS = sum(HOURS, na.rm = T),EXPENSE = sum(EXPENSE, na.rm = T))
  colnames(Summary)[11] <- "PP.END.DATE"
  
  #Bring in paycode mapping and hours included columns
  Site_Summary <- left_join(Summary,System_Paycode) %>%
    select(c(1:10),c(15:17),c(11:13))
  
  #Bring in cost center mappings
  System_Department <- read_xlsx("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/All Sites Cost Center Mappings.xlsx")
  Site_Summary <- left_join(Site_Summary,System_Department, by = c("DPT.WRKD" = "COST.CENTER")) %>%
    ungroup() %>%
    mutate(SITE = case_when(
      is.na(SITE) ~ PAYROLL,
      TRUE ~ SITE),
      PAYROLL = SITE,
      SITE = NULL) 
  
  #Bring in Provider Column
  System_Jobcode <- read_xlsx("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables/All Sites Job Code Mappings.xlsx")
  System_Jobcode <- distinct(System_Jobcode)
  Site_Summary <- left_join(Site_Summary,System_Jobcode, by = c("PAYROLL"="PAYROLL", "J.C"="J.C")) %>%
    select(c(1:16,18))
  
  Site_Summary <- Site_Summary %>% distinct()

  return(Site_Summary)
}
