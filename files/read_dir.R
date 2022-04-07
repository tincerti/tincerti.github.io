# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DESCRIPTION ----
# ______________________________________________________________________________

# Last updated 7 April, 2022 by Trevor Incerti

# This file contains a function that can be used to concatenate all data 
# files with common columns from a directory into a single data set, 
# and creates an optional column identifying the name of the file for 
# each row. 

# This can be useful for e.g., administrative data provided in individual 
# files by city. The current function supports any delimited text data files 
# and Excel files. Support for other data types will be added. 

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# REQUIRED LIBRARIES AND HELPER FUNCTIONS ----
# ______________________________________________________________________________

#### Required libraries ####
library(tidyverse)
library(readxl)

#### Helper functions #### 
read_flnm <- function(flnm, delim = NULL, skip = NULL) {
    read_delim(flnm, delim = delim, skip = skip, 
               col_types = cols(.default = "c")) %>% 
      mutate(filename = tools::file_path_sans_ext(fs::path_file(flnm)))
}

read_flnm_xl <- function(flnm, sheet = NULL, skip = NULL) {
    readxl::read_excel(flnm, sheet = sheet, skip = skip) %>% 
      mutate(filename = tools::file_path_sans_ext(fs::path_file(flnm)))
}

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# DEFINE MAIN FUNCTION ----
# ______________________________________________________________________________

# Function arguments:
# Path = filepath of directory where data files are located.
# Extension = data files extension. Currently accepts:
# all extensions compatible with readr::read_delim and "xlsx" for Excel.
# delim = Single character used to separate fields within a record, e.g. ",".
# sheet = Sheet to import if importing from Excel. 
# skip = Number of rows to skip when importing each file.

####  Main function: read in and append all files in a directory #### 
read_dir = function(path, extension, delim, filename, sheet = NULL, skip = 0) {
  
  # Stop and display errors if conflicting arguments are entered
  if (!missing(sheet) & extension != "xlsx") {
    stop("Error: Argument 'sheet' only applies to Excel files")
    
  # Read in delimited text data files
  } else if (filename == FALSE & extension != "xlsx") {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      map_df(~read_delim(., delim = delim, skip = skip, 
                         col_types = cols(.default = "c")))
    
  } else if (filename == TRUE & extension != "xlsx") {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      map_df(~read_flnm(., delim = delim, skip = skip))
    
  # Read in Excel data files  
   } else if (extension == "xlsx" & filename == F) {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      map_df(~readxl::read_excel(., sheet = sheet, skip = skip))
    
  } else if (extension == "xlsx" & filename == T) {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      map_df(~read_flnm_xl(., sheet = sheet, skip = skip))
  }
}