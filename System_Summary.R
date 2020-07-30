
dir <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FEMA Reimbursement/MSHS-FEMA-Reimbursement/"
setwd(dir)

##Refresh Master#########################################################################
setwd(paste0(dir,"Preprocess"))
folder_preprocess <- paste0(dir,"Preprocess")
#create list of preprocessed files
preprocess <- list.files(path=folder_preprocess)
#bring in functions to run preprocess scripts and summarize them
source(paste0(dir,"Source_Summary.R"))
#Run source function
System_Source <- lapply(preprocess,function(x)Source_Func(x))
System_Preprocess <- list(MSH_MSQ = data_MSH_MSQ,MSBIB = data_MSBI_MSB,MSSLW = data_MSSL_MSW,Rightsourcing = data_Rightsourcing)
#summarize all preprocessed dataframes
System_Summary_List <- lapply(System_Preprocess, function(x)Source_Summary(x))
#bind all summary tables into system summary
System_Summary = do.call("rbind",System_Summary_List)
#Save System Summary table
setwd(dir)
saveRDS(System_Summary,file = "System_Summary.rds")
#########################################################################################

##Reload Master##########################################################################
setwd(dir)
System_Summary <- readRDS("System_Summary.rds")
#########################################################################################

# #Savey PP Summary tables as excel files
# setwd("C:/Users/gregl/OneDrive/Documents/MSH-MSQ-Payroll/Summary Tables")
# file <- paste0("MSH_MSQ_PP HOURS_",Sys.Date(),".xlsx")
# hs <- createStyle(textDecoration = "BOLD", fgFill = "#B2B3B2", fontColour = "#000000", halign = "center", border = "TopBottomLeftRight", borderColour = "#000000")
# df <- Summary_Hours_PP
# write.xlsx(df, file=file, startCol = c(1,1), startRow = c(1,1),
#            asTable = c(F,F), headerStyle = hs, borders = "all")