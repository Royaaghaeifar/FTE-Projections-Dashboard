
#dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/"


##Refresh Master#########################################################################
#setwd(paste0(getwd(),"/Preprocess"))
#folder_preprocess <- getwd()
#create list of preprocessed files
preprocess <- list.files(path=paste0(getwd(),"/Preprocess"))
#bring in functions to run preprocess scripts and summarize them
source("Source_Summary_FTE Trend.R")
#Run source function
System_Source <- lapply(preprocess,function(x)Source_Func(x))
System_Preprocess <- list(MSH_MSQ = data_MSH_MSQ,MSBIB = data_MSBI_MSB,MSSLW = data_MSSL_MSW,MSH_MSQ_Oracle = data_MSH_MSQ_oracle)
#summarize all preprocessed dataframes
System_Summary_List <- lapply(System_Preprocess, function(x)Source_Summary(x))
#bind all summary tables into system summary
System_Summary = do.call("rbind",System_Summary_List)
#Save System Summary table
setwd("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/System Summary")
saveRDS(System_Summary,file = "System_Summary.rds")

#########################################################################################

##Reload Master##########################################################################
setwd("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/System Summary")
System_Summary <- readRDS("System_Summary.rds")
#########################################################################################
