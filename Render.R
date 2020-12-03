#FTE Dashboard Render
library(rmarkdown)

#Source system summary to update the system_summary.rds
###Check error report for any mapping issues###########
source("System_Summary.R")

#Save System Summary table
saveRDS(System_Summary,file="J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/System Summary/System_Summary.rds")

#Enter max PP end date for dashboard
end <- "10/24/2020"
#render FTE Trend Dashboard
render("FTE_Trend_Dashboard.rmd")
