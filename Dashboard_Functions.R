library(tidyverse)
##Reload Master##########################################################################
setwd("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/System Summary")
System_Summary <- readRDS("System_Summary.rds")
System_Summary <- System_Summary %>%
  filter(PP.END.DATE >= as.Date("12/22/2019",format="%m/%d/%Y"))
#########################################################################################

#Worked hour pay code mappings
worked <- c("REGULAR","OVERTIME","OTHER_WORKED")

# # Mount Sinai corporate colors "USE THIS TO ADD COLORS"
# MountSinai_colors <- c(
#   `light pink`   = "#fcc9e9",
#   `med pink`     = "#fa93d4",
#   `dark pink`    = "#d80b8c",
#   `light purple` = "#c7c6ef",
#   `med purple`   = "#8f8ce0",
#   `light blue`   = "#5cd3ff",
#   `med blue`     = "#06ABEB",
#   `dark blue`    = "#212070",
#   `light grey`   = "#b2b3b2",
#   `dark grey`    = "#686868",
#   `yellow`       = "#E69F00"
# )
# 
# # Function to extract Mount Sinai colors as hex codes
# # Use Character names of MountSinai_colors
# 
# MountSinai_cols <- function(...) {
#   cols <- c(...)
#   
#   if (is.null(cols))
#     return (MountSinai_colors)
#   
#   MountSinai_colors[cols]
# }
# 
# # Create palettes 
# MountSinai_palettes <- list(
#   `all`   = MountSinai_cols("med blue","dark pink","dark blue","light grey", "light blue",
#                             "light pink", "light purple","med pink","med purple","yellow" ),
#   
#   `main`  = MountSinai_cols("med blue","dark pink","dark blue","dark grey","light pink","light blue","light grey"),
#   
#   `pink`  = MountSinai_cols("light pink", "dark pink"),
#   
#   `blue`  = MountSinai_cols("light blue", "dark blue"),
#   
#   `grey`  = MountSinai_cols("light grey", "med blue")
#   
# )
# MountSinai_palettes
# 
# MountSinai_pal <- function(palette = "main", reverse = FALSE, ...) {
#   pal <- MountSinai_palettes[[palette]]
#   
#   if (reverse) pal <- rev(pal)
#   
#   colorRampPalette(pal, ...)
# }

#Service Line Graphs
service_line <- function(hosp,service, startdate = as.Date("12/22/2019",format="%m/%d/%Y")){
  data <- System_Summary %>%
    filter(PP.END.DATE < as.Date("3/1/2020",format="%m/%d/%Y") |
           PP.END.DATE >as.Date("5/9/2020",format="%m/%d/%Y"),
           PROVIDER == 0,
           INCLUDE.HOURS == 1,
           SERVICE.LINE == service,
           PAY.CODE.MAPPING %in% worked) %>%
    group_by(DEFINITION.CODE,DEFINITION.NAME,PP.END.DATE) %>%
    summarise(FTE = sum(HOURS, na.rm = T)/75) %>%
    mutate(DEPARTMENT = paste(DEFINITION.CODE,"-",toupper(DEFINITION.NAME)),
           DATES = as.character(PP.END.DATE))
  data$DATES <- factor(data$DATES)
  y <- round(max(data$FTE),-2)
  service_line_graph <- ggplot(data = data, aes(x=DATES,y=FTE,group=DEPARTMENT,color=DEPARTMENT))+
    geom_line(size=1.5)+
    geom_point(size=2.75, colour = "black")+
    ggtitle(paste(hosp,service,"FTE's By Pay Period"))+
    xlab("Pay Period")+
    ylab("FTE (Full Time Equivalent)")+
    scale_y_continuous(breaks = seq(0,y,y/10))+
    #scale_color_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(service_line_graph)
}

#System Graphs
system_premier <- function(){
  data <- System_Summary %>%
    filter(PP.END.DATE < as.Date("3/1/2020",format="%m/%d/%Y") |
             PP.END.DATE >as.Date("5/9/2020",format="%m/%d/%Y"),
           PROVIDER == 0,
           INCLUDE.HOURS == 1,
           PAY.CODE.MAPPING %in% worked,
           !is.na(DEFINITION.CODE)) %>%
    group_by(PAYROLL,PP.END.DATE) %>%
    summarise(FTE = sum(HOURS, na.rm = T)/75) %>%
    mutate(DATES = as.character(PP.END.DATE))
  data$DATES <- factor(data$DATES)
  y <- round(max(data$FTE),-2)
  system_premier_graph <- ggplot(data=data,aes(x=DATES,y=FTE,group=PAYROLL,color=PAYROLL))+
    geom_line(size=1.5)+
    geom_point(size=2.75, colour = "black")+
    ggtitle("MSHS Premier FTE's By Site")+
    xlab("Pay Period")+
    ylab("FTE (Full Time Equivalent)")+
    scale_y_continuous(breaks = seq(0,y,y/10))+
    #scale_color_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(system_premier_graph)
}
system_no_premier <- function(){
  data <- System_Summary %>%
    filter(PP.END.DATE < as.Date("3/1/2020",format="%m/%d/%Y") |
             PP.END.DATE >as.Date("5/9/2020",format="%m/%d/%Y"),
           PROVIDER == 0,
           INCLUDE.HOURS == 1,
           PAY.CODE.MAPPING %in% worked,
           is.na(DEFINITION.CODE)) %>%
    group_by(PAYROLL,PP.END.DATE) %>%
    summarise(FTE = sum(HOURS, na.rm = T)/75) %>%
    mutate(DATES = as.character(PP.END.DATE))
  data$DATES <- factor(data$DATES)
  y <- round(max(data$FTE),-2)
  system_premier_graph <- ggplot(data=data,aes(x=DATES,y=FTE,group=PAYROLL,color=PAYROLL))+
    geom_line(size=1.5)+
    geom_point(size=2.75, colour = "black")+
    ggtitle("MSHS Premier FTE's By Site")+
    xlab("Pay Period")+
    ylab("FTE (Full Time Equivalent)")+
    scale_y_continuous(breaks = seq(0,y,y/10))+
    #scale_color_manual(values=MountSinai_pal("main")(7))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"))
  return(system_premier_graph)
}
