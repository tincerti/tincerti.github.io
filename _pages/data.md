---
layout: splash
title: "Data"
permalink: /data/
author_profile: false
---

## Data

I am involved in multiple ongoing data collection processes related to Japanese government expenditures and personnel movements. All data sets will be made publicly available upon completion and validation: 

- **Amakudata** (with Sayumi Miyano, Diana Stanescu, and Hikaru Yamagishi): A dataset of all Japanese bureaucrats who have retired to positions in the private sector (i.e. revolving door or "amakudari" appointments) from 2009 - 2019. While the full dataset is forthcoming, an R Shiny dashboard---[*Amakudashboard*](https://trevorincerti.shinyapps.io/amakudashboard/)--- that allows users to explore the dataset is currently live.
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
  
- [**jNPO**](https://github.com/tincerti/jNPO): A dataset of all subsidies and contracts from the Japanese government to nonprofit organizations (NPOs) from 2011 - 2021, including the agency or ministry which provided the subsidy or made the purchase, the NPO that received the subsidy or contract, and the value of the subsidy or contract.  

- **jProcurement** (with Hikaru Yamagishi): A dataset of all products procured from the private sector by the Japanese government from 2003 - 2018, including the agency or ministry which made the purchase, the company the product was purchased from, and the value of the contract.  


## Software 

**read_dir**: R function that can be used to concatenate all data files with common columns from a directory into a single data set, and creates an optional column identifying the name of the file for each row. 
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

# Import/define pipe operator from magrittr ------------------------------------
`%>%` <- magrittr::`%>%`

# Helper functions -------------------------------------------------------------
read_flnm <- function(flnm, delim = NULL, skip = NULL) {
  read_delim(flnm, delim = delim, skip = skip, 
             col_types = cols(.default = "c")) %>% 
    mutate(filename = tools::file_path_sans_ext(fs::path_file(flnm)))
}

read_flnm_xl <- function(flnm, sheet = NULL, skip = NULL, col_types = NULL) {
  readxl::read_excel(flnm, sheet = sheet, skip = skip, col_types = col_types) %>% 
    mutate(filename = tools::file_path_sans_ext(fs::path_file(flnm)))
}

# Main function: read in and append all files in a directory ------------------ 
# Function arguments:
# Path = filepath of directory where data files are located.
# Extension = data files extension. Currently accepts:
# all extensions compatible with readr::read_delim and "xlsx" for Excel.
# delim = Single character used to separate fields within a record, e.g. ",".
# sheet = Sheet to import if importing from Excel. 
# skip = Number of rows to skip when importing each file.

read_dir = function(path, extension, delim, filename, sheet = NULL, skip = 0,
                    col_types = NULL) {
  
  # Stop and display errors if conflicting arguments are entered
  if (!missing(sheet) & extension != "xlsx") {
    stop("Error: Argument 'sheet' only applies to Excel files")
    
    # Read in delimited text data files
  } else if (filename == FALSE & extension != "xlsx") {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~read_delim(., delim = delim, skip = skip, 
                                col_types = cols(.default = "c")))
    
  } else if (filename == TRUE & extension != "xlsx") {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~read_flnm(., delim = delim, skip = skip))
    
    # Read in Excel data files  
  } else if (extension == "xlsx" & filename == F) {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~readxl::read_excel(., sheet = sheet, skip = skip,
                                        col_types = col_types))
    
  } else if (extension == "xlsx" & filename == T) {
    list.files(path = path,
               pattern = paste0("*.", extension),
               full.names = T) %>%
      purrr::map_df(~read_flnm_xl(., sheet = sheet, skip = skip,
                                  col_types = col_types))
  }
}
```

</details>










