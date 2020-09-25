library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(knitr)
library(kableExtra)

##Reload Master##########################################################################
System_Summary <- readRDS("J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Analysis/FTE Projections Dashboard/System Summary/System_Summary.rds")
System_Summary <- System_Summary %>%
  filter(PP.END.DATE >= as.Date("12/22/2019",format="%m/%d/%Y"),
         PP.END.DATE <= as.Date(end,format="%m/%d/%Y")) %>%
  replace_na(list(SERVICE.LINE = "Other"))
#########################################################################################

#Worked hour pay code mappings
worked_paycodes <- c('REGULAR','OVERTIME','OTHER_WORKED','EDUCATION','ORIENTATION','AGENCY')
#pre covid pay period end dates
pre_covid_PP <- as.Date(c('2020-01-04','2020-01-18','2020-02-01','2020-02-15','2020-02-29'))
#get unique service lines
service_lines <- list("ICU","Labor & Delivery","Mother/Baby","Progressive","Med/Surg","Psych","RETU", "Perioperative Services","Support Services","Pharmacy","Radiology","Lab","Emergency Department","Other")
report_period_length <- 3
biweekly_fte <- 75
digits_round <- 2

#Site Based Service List
MSH_service_list <- list("ICU","Labor & Delivery","Mother/Baby","Progressive","Med/Surg","Psych","RETU", "Perioperative Services","Support Services","Pharmacy","Radiology","Lab","Emergency Department","Other")
MSQ_service_list <- list("ICU","Med/Surg","Perioperative Services","Support Services","Pharmacy","Radiology","Emergency Department","Other")
MSBI_service_list <- list("ICU","Progressive","Med/Surg","Psych","RETU","Perioperative Services","Support Services","Pharmacy","Radiology","Emergency Department","Other")
MSB_service_list <- list("ICU","Progressive","Med/Surg","Perioperative Services","Support Services","Pharmacy","Radiology","Lab","Emergency Department","Other")
MSW_service_list <- list("ICU","Labor & Delivery","Mother/Baby","Progressive","Med/Surg","Psych","Perioperative Services","Support Services","Pharmacy","Radiology","Lab","Emergency Department","Other")
MSM_service_list <- list("ICU","Progressive","Med/Surg","Psych","Perioperative Services","Support Services","Pharmacy","Radiology","Lab","Emergency Department","Other")
site_list <- list("MSH","MSQ","MSBI","MSB","MSW","MSM")

#Pre filter data 
data <- System_Summary %>%
  filter(PP.END.DATE < as.Date("3/1/2020",format="%m/%d/%Y") | #date must be earlier than 3/1/2020
           PP.END.DATE >as.Date("5/9/2020",format="%m/%d/%Y"),   #or greater than 5/9/2020
         PROVIDER == 0, #remove providers
         INCLUDE.HOURS == 1, #only use included hour paycodes
         PAY.CODE.MAPPING %in% worked_paycodes) %>% #remove unproductive paycodes 
  group_by(PAYROLL,DEFINITION.CODE,DEFINITION.NAME,SERVICE.LINE,PP.END.DATE) %>%
  summarise(FTE = sum(HOURS, na.rm = T)/biweekly_fte) %>% #calculate FTE
  mutate(DEPARTMENT = paste0(DEFINITION.CODE," - ",toupper(DEFINITION.NAME)), #capitalize department
         DATES = as.character(PP.END.DATE)) %>% #add character form of data
  arrange(PP.END.DATE) #arrange by pay period end date

#Get Reporting Period data range
rep <- data %>% ungroup() %>% select(PP.END.DATE) %>% as.vector() %>% distinct() 
rep_per <- as.vector(rep[c(nrow(rep)-2,nrow(rep)-1,nrow(rep)),1])
rep_per[1,1] <- rep_per[1,1]-13
rep_per$PP.END.DATE <- as.character(rep_per$PP.END.DATE)
rep <- paste0(substr(rep_per[1,1],6,7),"/",substr(rep_per[1,1],9,10),"/",substr(rep_per[1,1],1,4)," to ",substr(rep_per[3,1],6,7),"/",substr(rep_per[3,1],9,10),"/",substr(rep_per[3,1],1,4))

# Mount Sinai corporate colors "USE THIS TO ADD COLORS"
MountSinai_colors <- c(
  `light pink`   = "#fcc9e9",
  `med pink`     = "#fa93d4",
  `dark pink`    = "#d80b8c",
  `light purple` = "#c7c6ef",
  `med purple`   = "#8f8ce0",
  `light blue`   = "#5cd3ff",
  `med blue`     = "#06ABEB",
  `dark blue`    = "#212070",
  `light grey`   = "#b2b3b2",
  `dark grey`    = "#686868",
  `yellow`       = "#E69F00"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Create palettes
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("med blue","dark pink","dark blue","light grey", "light blue",
                            "light pink", "light purple","med pink","med purple","yellow" ),
  
  `main`  = MountSinai_cols("med blue","dark pink","dark blue","dark grey","light pink","light blue","light grey"),
  
  `pink`  = MountSinai_cols("light pink", "dark pink"),
  
  `blue`  = MountSinai_cols("light blue", "dark blue"),
  
  `grey`  = MountSinai_cols("light grey", "med blue")
  
)
MountSinai_palettes

MountSinai_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#Site Level Service Line Graphs
service_line <- function(hosp,service){
  library(tidyr)
  data_service <- data %>% #take pre-filtered data
    filter(PAYROLL == hosp, #filter on specific hospital
           SERVICE.LINE == service) #filter on specific service line
  data_service <- data_service %>% 
    pivot_wider(id_cols = c("DEFINITION.CODE","DEFINITION.NAME","DEPARTMENT"),
                names_from = "PP.END.DATE",
                values_from = FTE) #pivot dataframe to bring in NAs for missing PP
  data_service <- data_service[,c(1:3,(ncol(data_service)-7):ncol(data_service))]
  data_service <- data_service %>% 
    pivot_longer(cols = 4:ncol(data_service),
                 names_to = "PP.END.DATE")#pivot dataframe to original form
  data_service <- data_service %>% 
    mutate(FTE = case_when(
      is.na(value) ~ 0, #if FTE is NA -> 0
      !is.na(value) ~ value), #else leave the value
      DATES = as.factor(PP.END.DATE),
      FTE = round(FTE,digits_round)) #turn dates into factor
  data_service$DATES <- factor(data_service$DATES)
  service_line_graph <- ggplot(data = data_service, aes(x=DATES,y=FTE,group=DEPARTMENT,color=DEPARTMENT))+
    geom_line(size=1.5)+
    geom_point(size=2.75)+
    ggtitle(paste(hosp,service,"FTE's By Pay Period"))+
    xlab("Pay Period")+
    ylab("FTE (Full Time Equivalent)")+
    scale_color_manual(values=MountSinai_pal("main")(length(unique(data_service$DEFINITION.CODE))))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"),
          legend.text=element_text(size = 6)) #create and style service line graph
  service_line_graphly <- ggplotly(service_line_graph,tooltip=c("group","x","y")) %>%
    config(displaylogo = F,
           modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center")) #turn graph into plotly interactive
  return(service_line_graphly)
}

#Site level Kable
k <- function(hosp,service){
  library(tidyr)
  kdata <- data %>% 
    filter(PAYROLL == hosp, 
           SERVICE.LINE == service) %>% 
    pivot_wider(id_cols = c(SERVICE.LINE,DEPARTMENT),
                names_from = DATES,
                values_from = FTE) 
  kdata[is.na(kdata)] <- 0 
  colnames(kdata)[1] <- "SERVICE LINE" 
  sort <- colnames(kdata)[ncol(kdata)] 
  kdata <- kdata %>% ungroup() %>% arrange(desc(!!sym(sort))) 
  kdata$`Reporting Period Avg.` <- apply(kdata[,(ncol(kdata)-2):ncol(kdata)],1,mean)
  kdata$`Baseline Avg.` <- rowMeans(subset(kdata, select = c("2020-01-04","2020-01-18","2020-02-01","2020-02-15","2020-02-29"), na.rm = TRUE))
  kdata <- kdata[,c(1:2,(ncol(kdata)-9):ncol(kdata))]
  kdata[,(ncol(kdata)-9):ncol(kdata)] <- round(kdata[,(ncol(kdata)-9):ncol(kdata)],digits_round)
  Ktable <- kdata 
  kable(Ktable) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), fixed_thead = T) %>%
    row_spec(0, background = "#212070", color = "white") %>%
    row_spec(1:nrow(Ktable), color = "black") %>%
    row_spec(0:nrow(Ktable), align = "c", font_size = 11) %>%
    column_spec(1,bold = T) %>%
    collapse_rows(1)
}

#System level Kable
premier_sum_stats <- function(sys.sum, site, serv.line){
  data_export <- data %>% 
    ungroup() %>%
    select(PAYROLL,SERVICE.LINE,FTE,PP.END.DATE) %>%
    filter(PAYROLL == site,
           SERVICE.LINE == serv.line) %>%
    group_by(PAYROLL,SERVICE.LINE,PP.END.DATE) %>%
    summarise(FTE = sum(FTE, na.rm = T)) %>%
    pivot_wider(id_cols = c(PAYROLL,SERVICE.LINE),
                names_from = PP.END.DATE,
                values_from = FTE) %>%
    ungroup() %>%
    mutate(PAYROLL = factor(PAYROLL,levels=c("MSH","MSQ","MSBI","MSB","MSW","MSM")))
  data_export$`Reporting Period Avg.` <- apply(data_export[,(ncol(data_export)-2):ncol(data_export)],1,mean)
  data_export$`Baseline Avg.` <- rowMeans(subset(data_export, select = c("2020-01-04","2020-01-18","2020-02-01","2020-02-15","2020-02-29"), na.rm = TRUE))
  data_export <- data_export[,c(1:2,(ncol(data_export)-9):ncol(data_export))]
  data_export[,(ncol(data_export)-9):ncol(data_export)] <- round(data_export[,(ncol(data_export)-9):ncol(data_export)],digits_round)
  colnames(data_export)[1:2] <- c('Site', 'Service Line')
  data_final <- data_export
  return(data_final)
}
system_kable <- function(table){
  kable(table) %>%
    kable_styling(bootstrap_options = c("striped", "hover"), fixed_thead = T) %>%
    row_spec(0, background = "#212070", color = "white") %>%
    row_spec(1:nrow(table), color = "black") %>%
    row_spec(0:nrow(table), align = "c", font_size = 11) %>%
    column_spec(1,bold = T)
}

#System level service line graph
graph_data <- function(serv.line){
  data_export <- data %>% 
    ungroup() %>%
    select(PAYROLL,SERVICE.LINE,FTE,PP.END.DATE,DATES) %>%
    filter(SERVICE.LINE == serv.line) %>%
    group_by(PAYROLL,SERVICE.LINE,PP.END.DATE,DATES) %>%
    summarise(FTE = round(sum(FTE, na.rm = T),digits_round)) %>%
    ungroup() %>%
    mutate(PAYROLL = factor(PAYROLL,levels=c("MSH","MSQ","MSBI","MSB","MSW","MSM")))
  system_line_graph <- ggplot(data = data_export, aes(x=DATES,y=FTE,group=PAYROLL,color=PAYROLL))+
    geom_line(size=1.5)+
    geom_point(size=2.75)+
    ggtitle(paste(serv.line,"FTE's By Pay Period"))+
    xlab("Pay Period")+
    ylab("FTE (Full Time Equivalent)")+
    scale_color_manual(values=MountSinai_pal("main")(length(unique(data_export$PAYROLL))))+
    theme(plot.title=element_text(hjust=.5,size=20),
          axis.title = element_text(face="bold"),
          legend.text=element_text(size = 6)) #create and style service line graph
  system_line_graphly <- ggplotly(system_line_graph,tooltip=c("group","x","y")) %>%
    config(displaylogo = F,
           modeBarButtonsToRemove = c("lasso2d","autoScale2d","select2d","toggleSpikelines")) %>%
    layout(title = list(xanchor = "center")) #turn graph into plotly interactive
  return(system_line_graphly)
}
