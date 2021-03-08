library(dplyr)
library(readxl)
library(openxlsx)

dir <- "~/MSH-MSQ-Payroll/"
setwd(dir)

##MSQ##
#List files from MSQ Raw folder
setwd(paste0(dir,"MSQ Raw/"))
folderMSQ <- paste0(dir,"MSQ Raw/")     
MSQ_file_list <- list.files(path=folderMSQ, pattern="*.txt")
#Read files in MSQ Raw as csv
MSQlist = lapply(MSQ_file_list, function(x)read.csv(x, sep = ";", header=T, stringsAsFactors = F,colClasses = rep("character",18)))
#Bind all MSQ files and assign their site = "MSQ"
MSQ = do.call("rbind", MSQlist)
#Remove Duplicate rows
MSQ <- MSQ %>% distinct()

##MSH##
#List files from MSH Raw folder
setwd(paste0(dir,"MSH Raw/"))
folderMSH <- paste0(dir,"MSH Raw/")   
MSH_file_list <- list.files(path=folderMSH, pattern="*.txt")                              
#Read files in MSH Raw as csv    
MSHlist = lapply(MSH_file_list, function(x)read.csv(x, sep = ";", header=T, stringsAsFactors = F)) 
#Bind all MSH files and assign their site = "MSH"
MSH = do.call("rbind", MSHlist)
#remove duplicate rows
MSH <- MSH %>% distinct()

#Bind MSH and MSQ
MSHQ <- rbind(MSH,MSQ)

#Assign correct Payroll Site
COA <- read.csv("~/MSH-MSQ-Payroll/Reference Tables/SiteCOA.csv",stringsAsFactors = F)
MSHQ <- left_join(MSHQ,COA,by = c("DPT.WRKD"="Legacy.CostCenter.Fund")) %>%
  select(c(1:17,20))
na <- filter(MSHQ,is.na(PAYROLL))
na <- na %>% 
  mutate(PAYROLL = 
    case_when(substr(na$DPT.WRKD,1,3) == "013" ~ "MSQ",
              substr(na$DPT.WRKD,1,3) != "013" ~ "MSH")
  )

#Form final datafram
data_MSH_MSQ <- rbind(filter(MSHQ,!is.na(PAYROLL)),na)
data_MSH_MSQ <- data_MSH_MSQ %>%
  filter(as.Date(END.DATE, format = "%m/%d/%Y") <= as.Date("03/21/2020", format = "%m/%d/%Y"))

setwd("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/Reference Tables")
saveRDS(data_MSH_MSQ,file = "data_MSH_MSQ.rds")
