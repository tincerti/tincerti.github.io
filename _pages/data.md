---
layout: splash
title: "Data"
permalink: /data/
author_profile: false
---

## Data

I am currently involved in two ongoing data collection processes. Both data sets will be made publicly available upon completion and validation: 

- *Amakudata* (with Sayumi Miyano, Diana Stanescu, and Hikaru Yamagishi): A dataset of all Japanese bureaucrats who have retired to positions in the private sector (i.e. revolving door or "amakudari" appointments) from 2009 - 2019. While the full dataset is forthcoming, an R Shiny dashboard---[*Amakudashboard*](https://trevorincerti.shinyapps.io/amakudashboard/)--- that allows users to explore the dataset is currently live.
  <details>
  <summary>Data visualizations</summary>
  <ul>
    <li><a href="https://www.trevorincerti.com/files/ministry_publicinterest.html">Flows of bureaucrats from ministries to top public corporations</a>.</li>
    <li><a href="https://www.trevorincerti.com/files/ministry_private.html">Flows of bureaucrats from ministries to top private sector corporations</a>.</li>
    <li><a href="https://www.trevorincerti.com/files/ministry_industry.html">Flows of bureaucrats from ministries to publicly traded companies by industry</a>.</li>
  </ul>
  </details>
  <details>
  <summary>Data dictionary</summary>
  <ul>
    <li> Coming soon </li>
  </ul>
  </details>

<br>

- *Procurement* (with Hikaru Yamagishi): A dataset of all products procured from the private sector by the Japanese government from 2003 - 2018, including the agency or ministry which made the purchase, the company the product was purchased from, and the value of the contract.


## Software 

- read_dir: R function that can be used to concatenate all data files with common columns from a directory into a single data set, and creates an optional column identifying the name of the file for each row. 
  <details markdown=1><summary markdown="span">Code</summary>
  ```R
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
  ```

  </details>










