Source_Func <- function(x){
  source(x)
  setwd(paste0(dir,"Preprocess/"))
}

Source_Summary <- function(data){
  library(readxl)
  
  setwd(dir)
  #Read paycode mapping file and Pay cycle file
  System_Paycode <- read_xlsx("Reference Tables/All Sites Pay Code Mappings.xlsx")
  colnames(System_Paycode) <- c("PAY.CODE","PAY.CODE.NAME","PAY.CODE.MAPPING","INCLUDE.HOURS","INCLUDE.EXPENSES","JODI","JODI.NO.PTO")
  System_Paycode <- System_Paycode %>%
    mutate(PAY.CODE = str_trim(PAY.CODE)) 
  for(i in 1:nrow(System_Paycode)){
    if(nchar(System_Paycode$PAY.CODE)[i] == 1){
      System_Paycode$PAY.CODE[i] <- paste0("0", System_Paycode$PAY.CODE[i])
    }
  }
  
  #Read in paycycle
  PayCycle <- read.csv("Reference Tables/PayCycle.csv", header = T, stringsAsFactors = F)
  PayCycle <- PayCycle %>%
    mutate(Date.1 = as.Date(Date.1, format = "%m/%d/%Y")) %>%
    mutate(Start.Date = as.Date(Start.Date, format = "%m/%d/%Y")) %>%
    mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"))
  
  #Begin summarization
  Department <- data %>%
    group_by(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,END.DATE) %>%
    summarise(HOURS = sum(HOURS, na.rm = T), EXPENSE = sum(EXPENSE, na.rm = T))
  
  #assign pp end dates and summarize
  Summary <- left_join(Department,PayCycle,by = c("END.DATE" = "Date.1")) %>%
    select(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,End.Date,HOURS,EXPENSE) %>%
    group_by(PAYROLL,WRKD.LOCATION,HOME.LOCATION,DPT.WRKD,DPT.HOME,WRKD.DESCRIPTION,HOME.DESCRIPTION,J.C,J.C.DESCRIPTION,PAY.CODE,End.Date) %>%
    summarize(HOURS = sum(HOURS, na.rm = T),EXPENSE = sum(EXPENSE, na.rm = T))
  colnames(Summary)[11] <- "PP.END.DATE"
  
  #Bring in paycode mapping and hours included columns
  Site_Summary <- left_join(Summary,System_Paycode) %>%
    select(c(1:10),c(15:17),c(11:13))
  Site_Summary <- Site_Summary %>% distinct()
  
  return(Site_Summary)
}
