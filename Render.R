#FTE Dashboard Render
library(rmarkdown)
library(here)

#Source system summary to update the system_summary.rds
###Check error report for any mapping issues###########
source("System_Summary.R")

#Save System Summary table
saveRDS(System_Summary,file=paste0("/SharedDrive/data/deans/Presidents/SixSigma/",
                                   "MSHS Productivity/Productivity/",
                                   "Universal Data/Labor/RDS/",
                                   "System_Summary.rds"))

#Enter max PP end date for dashboard
end <- "11/20/2021"
#render FTE Trend Dashboard
render("FTE_Trend_Dashboard.Rmd")

#Archive the FTE Trend Dashboard with date of max pay period
file.copy(paste0(here(),"/FTE_Trend_Dashboard.html"),"/SharedDrive/data/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/Dashboards")
file.rename("/SharedDrive/data/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/Dashboards/FTE_Trend_Dashboard.html",
            paste0("/SharedDrive/data/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/Dashboards/FTE_Trend_Dashboard_",as.Date(end,format="%m/%d/%Y"),".html"))
