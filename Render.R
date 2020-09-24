#FTE Dashboard Render
library(rmarkdown)

#Source system summary to update the system_summary.rds
###Check error report for any mapping issues###########
source("System_Summary.R")

#Save System Summary table
saveRDS(System_Summary,file = "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/System Summary/System_Summary.rds")

#render FTE Trend Dashboard
render(FTE_Trend_Dashboard.R)