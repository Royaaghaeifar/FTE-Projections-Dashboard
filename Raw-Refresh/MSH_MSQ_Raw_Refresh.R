library(dplyr)
library(readxl)
library(openxlsx)
library(here)

dir <- paste0(here(), "/Raw Data/")

##MSQ##
#List files from MSQ Raw folder
folderMSQ <- paste0(dir,"MSQ Raw/")
MSQ_file_list <- list.files(path=folderMSQ, pattern="*.txt")
MSQ_details = file.info(list.files(path = folderMSQ, pattern="*.txt", full.names = T)) %>% arrange(mtime)
MSQ_path_list <- rownames(MSQ_details)
#Read files in MSQ Raw as csv
MSQlist = lapply(MSQ_path_list,
                 function(x)read.csv(x, sep = ";", header=T,
                                     stringsAsFactors = F,
                                     colClasses = rep("character",18),
                                     strip.white = TRUE))
#Bind all MSQ files and assign their site = "MSQ"
MSQ = do.call("rbind", MSQlist)
#Remove Duplicate rows
MSQ <- MSQ %>% distinct()


##MSH##
#List files from MSH Raw folder
folderMSH <- paste0(dir,"MSH Raw/")
MSH_file_list <- list.files(path=folderMSH, pattern="*.txt")
MSH_details = file.info(list.files(path = folderMSH, pattern="*.txt", full.names = T)) %>% arrange(mtime)
MSH_path_list <- rownames(MSH_details)
#Read files in MSQ Raw as csv
MSHlist = lapply(MSH_path_list, function(x)read.csv(x, sep = ";", header=T, stringsAsFactors = F,colClasses = rep("character",18)))
#Bind all MSQ files and assign their site = "MSQ"
MSH = do.call("rbind", MSHlist)
#Remove Duplicate rows
MSH <- MSH %>% distinct()

#Bind MSH and MSQ
MSHQ <- rbind(MSH,MSQ) %>%
  mutate(four = substr(DPT.WRKD,1,4)) %>%
  mutate(PAYROLL = case_when(
    four == "0130" ~ "MSQ",
    TRUE ~ "MSH")) %>%
  select(c(1:17,20))

#filter data
data_MSH_MSQ <- MSHQ %>%
  filter(as.Date(END.DATE, format = "%m/%d/%Y") <= as.Date("03/21/2020", format = "%m/%d/%Y"))
saveRDS(data_MSH_MSQ,file = "/SharedDrive/data/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Universal Data/Labor/RDS/data_MSH_MSQ.rds")
