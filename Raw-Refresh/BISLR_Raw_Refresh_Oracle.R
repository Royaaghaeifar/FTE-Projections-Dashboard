# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(here)

# Constants ---------------------------------------------------------------
#Start date is 1 day after the end of the last Premier Distribution
filter_start_dates <- c("2021-02-28", "2021-03-28", "2021-04-25")
#End date is 1 week after the end of the current Premier Distribution
filter_end_dates <- c("2021-04-03", "2021-05-01", "2021-05-29")
#Premier Distribution ex: 2/28/21- 3/27/21, 3/28/21 - 4/24/21

#Names of the weekly paycyle names in the payroll name column in data files
weekly_pc <- c("WEST WEEKLY", "BIB WEEKLY")

#Paths
dir_universal <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity",
                        "/Productivity/Universal Data")
dir_files <- paste0(here(), "/BISLR Oracle/BISLR Oracle/")
#delete first weekly cycle for BISLR 3/28 - 4/3 because it overlaps the start / end dates
#Update legacy raw refresh to include the deleted weekly cycle 2/28/21 - 3/6/21

# Import Data -------------------------------------------------------------
#Sort files based on index (oldest to newest) must match the
  #same order are filter end and start date
sort_import_files <- function(dir_data){
  #Compiling data on files
  name <- list.files(path = dir_data, full.names = F, pattern = "csv$")
  path <- list.files(path = dir_data, full.names = T, pattern = "csv$")
  index <- sapply(name, function(x) unlist(str_split(x, pattern = "_"))[1])
  #Sorting files based on index number
  files <- data.table::data.table(name, path, index)
  files <- files %>% arrange(index)
  #Importing data as list
  data_import <- lapply(as.list(files$path), function(x) read.csv(x, as.is = T))
  return(data_import)
}
#applying the sort and import funtion
data_BISLR <- sort_import_files(dir_files)

# PreProcessing Data ------------------------------------------------------
#Formatting start/end dates needed into date format
filter_end_dates <- as.Date(filter_end_dates)
filter_start_dates <- as.Date(filter_start_dates)

#Formatting start/end date columns in all data files
data_BISLR <- lapply(data_BISLR, function(x)
  x <- x %>% mutate(End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
                    Start.Date = as.Date(Start.Date, format = "%m/%d/%Y")))
#Filtering each file by start/end date specified
data_BISLR <- lapply(1:length(data_BISLR), function(x)
  data_BISLR[[x]] <- data_BISLR[[x]] %>%
    filter(End.Date <= filter_end_dates[x],
           Start.Date >= filter_start_dates[x]))

#Function to delete overlapping weekly pay cycle in BISLR
delete_weekly <- function(df, pay_cycles){
  #list out all weekly pay cycles in file from oldest to newest
  delete_pc <- df %>% filter(Payroll.Name %in% pay_cycles) %>%
    arrange(Start.Date, End.Date) %>%
    mutate(Start_End = paste0(Start.Date, "_", End.Date)) %>%
    select(Start.Date, End.Date, Start_End) %>%
    distinct()
  #filter out the oldest pay cycle
  data_export <- df %>%
    mutate(Start_End = paste0(Start.Date, "_", End.Date)) %>%
    filter(!Start_End %in% delete_pc$Start_End[1]) %>%
    mutate(Start_End = NULL)
  #return the filtered data frame
  return(data_export)

  #validation
  #check 2/28/21 - 3/6/21 paycycle should be removed
  #delete_pc <- data_BISLR[[1]] %>% filter(Payroll.Name %in% weekly_pc) %>%
    #arrange(Start.Date, End.Date) %>%
    #mutate(Start_End = paste0(Start.Date, "_", End.Date)) %>%
    #select(Start.Date, End.Date, Start_End) %>%
    #distinct()
  #data_pc_removed <- data_BISLR[[1]] %>%
    #mutate(Start_End = paste0(Start.Date, "_", End.Date)) %>%
    #filter(!Start_End %in% delete_pc$Start_End[1]) %>%
    #select(Start.Date, End.Date, Start_End) %>%
    #distinct()
  #data_pc_check <- data_BISLR[[1]] %>%
    #select(Start.Date, End.Date) %>%
    #arrange(Start.Date, End.Date) %>%
    #distinct()
}
#Applying function
data_BISLR <- lapply(data_BISLR, function(x) delete_weekly(x, weekly_pc))

#Combine all data tables in list into one
data_BISLR <- do.call(rbind, data_BISLR)

#Removing duplicates
data_BISLR <- data_BISLR %>% mutate(PAYROLL = "BISLR") %>% distinct()
# need to update the Payroll name to distinguish between different sites

data_BISLR <- data_BISLR %>%
  mutate(Position.Code.Description = case_when(
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "BESTREICH, ERIN S") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CRAIG, BRITTANY PIERCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312756 & Employee.Name == "DELAPENHA, SANDRA ELAINE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "GANZ, CINDY MARIE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312702 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "OKAY, DEVIN JOSEPH") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "ROBBINS, STEPHANIE") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L") ~ "DUS_REMOVE",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L (Lauren)") ~ "DUS_REMOVE",
    TRUE ~ Position.Code.Description)
  )

data_BISLR <- data_BISLR %>%
  mutate(Job.Code = case_when(
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "BESTREICH, ERIN S") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CHIANG, JACQUELINE PE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "CRAIG, BRITTANY PIERCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312756 & Employee.Name == "DELAPENHA, SANDRA ELAINE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312755 & Employee.Name == "GANZ, CINDY MARIE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312702 & Employee.Name == "MEEHAN, JENNIFER JOYCE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "OKAY, DEVIN JOSEPH") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 407210340412756 & Employee.Name == "ROBBINS, STEPHANIE") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L") ~ "DUS_RMV",
    (Department.IdWHERE.Worked == 414000040312763 & Employee.Name == "WALKER, THERESA L (Lauren)") ~ "DUS_RMV",
    TRUE ~ Job.Code)
  )

# Save RDS ----------------------------------------------------------------
write_rds(data_BISLR,
          path = paste0(dir_universal,"/Labor/RDS/data_BISLR_oracle.rds"))
