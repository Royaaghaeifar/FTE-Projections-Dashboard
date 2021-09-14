library(here)
library(rstudioapi)

row_count <- nrow(var)
#left_join
if(nrow(var) != row_count){
  stop(paste("Row count failed at", basename(getSourceEditorContext()$path)))}


