# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# LIBRARIES AND DATA IMPORT ----
# ______________________________________________________________________________

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(DT)
library(viridis)

options(encoding = 'UTF-8')

# Data import
amakudari <- read_csv("amakudari_final_2021.csv")

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#  DATA TRANSFORMATIONS ----
# ______________________________________________________________________________

# Designation unclassified and NA values as such
amakudari <- amakudari %>% 
  mutate(year = as.numeric(year(date_ret))) %>%
  mutate(
    across(c(ministry, agency, firm_dest_en, firm_dest_clean, 
             firm_type4_en, firm_type1_en, firm_type2_en, industry, industry_detail), 
           ~replace(., . ==  "-99" , "Not applicable")),
    across(c(ministry, agency, firm_dest_en, firm_dest_clean, 
            firm_type4_en, firm_type1_en, firm_type2_en, industry, industry_detail), 
           ~replace(., . ==  "99" , "Unclassified"))
    ) %>%
  filter(agency != "Police") %>%
  mutate(firm_dest_en = ifelse(firm_dest_clean == "日本年金機構",
                               "JAPAN PENSION SERVICE", firm_dest_en),
         firm_dest_clean = ifelse(firm_dest_clean == "(named  changed to  イオン東北 in 2020)",
                                  "マックスバリュ東北", firm_dest_clean))

# Create dataset of hires per ministry per year 
ministry_year <- amakudari %>% 
  group_by(ministry, agency, firm_dest_en, firm_dest_clean, firm_type2_en,
           post_dest, industry, industry_detail, year) %>%
  summarise(Total = n())

# Create dataset of top hirers by firm
top_hirers <- amakudari %>%
  mutate(year = as.numeric(year(date_ret))) %>%
  group_by(ministry, agency, firm_dest_clean, firm_dest_en, 
           firm_type4_en, firm_type1_en, firm_type2_en, year) %>%
  summarise(total = n()) %>%
  arrange(year, -total) %>%
  ungroup()

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#  VISUALIZATION SETTINGS ----
# ______________________________________________________________________________

# Hover label font
font <- list(
  size = 15,
  color = "white"
)

# Hover label borders
label <- list(
  bgcolor = "#4682B4",
  bordercolor = "transparent",
  font = font
)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#  UI ----
# ______________________________________________________________________________

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("cerulean"),
  
  "Amakudashboard v0.1 (beta)",
                 
    ##### Total hires per year tab ####
    tabPanel("Amukadari per year detailed filter",
    
    # Make application sidebar layout
    sidebarLayout(
        sidebarPanel(
    
    # Ministry drop down
    pickerInput(inputId = "ministries",
                label = "Ministry", 
                choices = sort(unique(ministry_year$ministry)),
                selected = unique(ministry_year$ministry),
                multiple = T,
                options = list(`actions-box` = TRUE)),
    
    # Agency drop down
    pickerInput(inputId = "agencies",
                label = "Agency", 
                choices = sort(unique(ministry_year$agency)),
                selected = sort(unique(ministry_year$agency)),
                multiple = T,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)),
    
    # Destination drop down (English)
    pickerInput(inputId = "firm_dest_en",
                label = "Destination (English)", 
                choices = sort(unique(ministry_year$firm_dest_en)),
                selected = sort(unique(ministry_year$firm_dest_en)),
                multiple = T,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)),
    
    # Destination drop down (Japanese)
    pickerInput(inputId = "firm_dest_clean",
                label = "Destination (日本語)", 
                choices = sort(unique(ministry_year$firm_dest_clean)),
                selected = sort(unique(ministry_year$firm_dest_clean)),
                multiple = T,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)),
    
    # Industry
    pickerInput(inputId = "industry",
                label = "Industry", 
                choices = sort(unique(ministry_year$industry)),
                selected = sort(unique(ministry_year$industry)),
                multiple = T,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)),
    
    # Industry (detailed)
    pickerInput(inputId = "industry_detail",
                label = "Industry (detailed)", 
                choices = sort(unique(ministry_year$industry_detail)),
                selected = sort(unique(ministry_year$industry_detail)),
                multiple = T,
                options = list(`actions-box` = TRUE, `live-search`=TRUE)),
    
    # Firm type drop down
    pickerInput(inputId = "firm_type",
                label = "Destination: Private vs. public interest", 
                choices = sort(unique(top_hirers$firm_type2_en)),
                selected = unique(top_hirers$firm_type2_en),
                multiple = T,
                options = list(`actions-box` = TRUE)),
    
    # Year slider
    sliderInput("year", label = "Select years", 
                min = 2009, 
                max = 2019, 
                step = 1, 
                sep = '', 
                value = c(2009, 2019))),
    
    # Make main panel barplot
    mainPanel(
      #h3(textOutput("caption")),
        plotlyOutput("ministry_year_bar")
        ),
    )
    ),
    ##### Top private sector hires by year tab #####
    tabPanel("Top hirers by year",
             
             # Year drop down
             selectInput(inputId = "year_table",
                         label = "Year", 
                         choices = sort(unique(top_hirers$year))),
             
             # Ministry drop down
             pickerInput(inputId = "ministries_table",
                         label = "Ministry", 
                         choices = sort(unique(top_hirers$ministry)),
                         selected = unique(top_hirers$ministry),
                         multiple = T,
                         options = list(`actions-box` = TRUE)),
             
             # Firm type drop down
             pickerInput(inputId = "firm_type4",
                         label = "Destination: Public or private", 
                         choices = sort(unique(top_hirers$firm_type4_en)),
                         selected = unique(top_hirers$firm_type4_en),
                         multiple = T,
                         options = list(`actions-box` = TRUE)),
             
             # Firm type drop down
             pickerInput(inputId = "firm_type2",
                         label = "Destination: Private vs. public interest", 
                         choices = sort(unique(top_hirers$firm_type2_en)),
                         selected = unique(top_hirers$firm_type2_en),
                         multiple = T,
                         options = list(`actions-box` = TRUE)),
             
             # Firm type drop down
             pickerInput(inputId = "firm_type1",
                         label = "Destination: Type of organization", 
                         choices = sort(unique(top_hirers$firm_type1_en)),
                         selected = unique(top_hirers$firm_type1_en),
                         multiple = T,
                         options = list(`actions-box` = TRUE)),
             
             DT::dataTableOutput("table")
             ),
    
  ##### Search by hirer tab ####
    tabPanel("Search by hirer",
             
             # Year drop down
             # sliderInput("year_hirer", label = "Select years", 
             #             min = 2009, 
             #             max = 2019, 
             #             step = 1, 
             #             sep = '', 
             #             value = c(2009, 2019)),
             
             # Ministry drop down
             pickerInput(inputId = "firm_dest_en_hirer",
                         label = "Destination (English)", 
                         choices = sort(unique(ministry_year$firm_dest_en)),
                         selected = sort(unique(ministry_year$firm_dest_en)),
                         multiple = T,
                         options = list(`actions-box` = TRUE, `live-search`=TRUE)
                         ),
             
             # Destination drop down (Japanese)
             pickerInput(inputId = "firm_dest_clean_hirer",
                         label = "Destination (日本語)", 
                         choices = sort(unique(ministry_year$firm_dest_clean)),
                         selected = sort(unique(ministry_year$firm_dest_clean)),
                         multiple = T,
                         options = list(`actions-box` = TRUE, `live-search`=TRUE)),
    
    verticalLayout(plotlyOutput("hirer_bar"), DT::dataTableOutput("hirer_ministry_table"))
    #splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("hirer_bar"), DT::dataTableOutput("hirer_ministry_table"))
    #plotlyOutput("hirer_bar")
    ),
  
  #### Ministry-public interest corporation flows HTML widget ####
  tabPanel("Flows to top public interest",
           htmlOutput("ministry_publicinterest")
           ),
  
  #### Ministry-private corporation flows HTML widget ####
  tabPanel("Flows to top private",
           htmlOutput("ministry_private")
  ),

  #### Ministry-industry flows HTML widget ####
  tabPanel("Flows from ministry to industry",
           htmlOutput("ministry_industry")
  ),
  
  ##### Notes tab ####
    tabPanel("Notes",
             mainPanel(
               HTML(
                 paste(
                   h4("Please see the following notes and terms and conditions of use. Violaters will be subject to strong disapproval and public aspersions on their character."),'<br/>',
                   h5("• Data source: Cabinet Bureau of Personnel Affairs"),
                   h5("• Data is preliminary and subject to change."),
                   h5("• Japanese language data for individual firms is more complete as not all firms have English names. English firm names are from Nikkei NEEDS database, and in some cases refer to parent organization."),
                   h5("• English firm names and industry classifications only available for firms matched with Nikkei NEEDS database."),
                   h5("• Downloading or scraping data for analysis purposes is PROHIBITED. Full dataset and data dictionary will be made available shortly. At this time, tabular data will be updated to include all entries rather than the top 10 only."),
                   h5("• Please cite as: Incerti, Trevor, Sayumi Miyano, Diana Stanescu, and Hikaru Yamagishi. ''Amakudata: a new dataset of revolving door hires.'' Forthcoming."),
                   h5("• If you find any errors or bugs, or have suggestions for additional functionality, filters, or visualizations, please contact me at trevor.incerti@yale.edu"),
                   h5("• Dashboard built using R Shiny. © Trevor Incerti.")
                 )
               )
             ),
    
    )
)

# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#  SERVER ----
# ______________________________________________________________________________

addResourcePath("tmpuser", getwd())

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #### Amakudari per year detailed filter ####
  output$ministry_year_bar <- renderPlotly({
  
    plotly_data <- ministry_year %>%
      filter(ministry %in% input$ministries) %>%
      filter(agency %in% input$agencies) %>%
      filter(firm_dest_en %in% input$firm_dest_en) %>%
      filter(firm_dest_clean %in% input$firm_dest_clean) %>%
      filter(industry %in% input$industry) %>%
      filter(industry_detail %in% input$industry_detail) %>%
      filter(firm_type2_en %in% input$firm_type) %>%
      filter(year >= input$year[1] & year <= input$year[2]) %>%
      group_by(year) %>%
      summarize(Total = sum(Total)) %>%
      rename(Year = year)

    plot_ly(plotly_data, 
    x = ~Year,
    y = ~Total,
    name = "Amukudata",
    hoverinfo = 'y',
    type = "bar",
    color = I("lightsteelblue")
    )
  })
  
  #### Top hirers table ####
  output$table <- DT::renderDataTable(
    top_hirers %>%
      filter(year %in% input$year_table) %>%
      filter(ministry %in% input$ministries_table) %>%
      filter(firm_type4_en %in% input$firm_type4) %>%
      filter(firm_type2_en %in% input$firm_type2) %>%
      filter(firm_type1_en %in% input$firm_type1) %>%
      select(firm_dest_clean, firm_dest_en, year, total) %>%
      rename(
        `Firm name (Japanese)` = firm_dest_clean,
        `Firm name (English)` = firm_dest_en,
        Year = year, Total = total) %>%
      arrange(-Total) %>%
      slice(1:10)
    )
  
  #### Hirers by ministry stacked bar ####
  output$hirer_bar <- renderPlotly({
    
    plotly_data_hirer_ministry <- ministry_year %>%
      filter(firm_dest_en %in% input$firm_dest_en_hirer) %>%
      filter(firm_dest_clean %in% input$firm_dest_clean_hirer) %>%
      group_by(year, ministry) %>%
      summarize(Total = sum(Total)) %>%
      rename(
        Year = year,
        Ministry = ministry
        )
    
    plot_ly(plotly_data_hirer_ministry, 
            x = ~Year,
            y = ~Total,
            color = ~Ministry,
            hoverlabel = label,
            text = ~Ministry,
            hovertemplate = paste('<b>%{text}</b><br><br>',
                                  '<b>Year</b>: %{x}',
                                  '<br><b>Hires</b>: %{y}<br><extra></extra>'),
            type = "bar",
            colors = 'viridis'
    ) %>%
      layout(barmode = 'stack')
    })
  
  #### Hirers by ministry table ####
  output$hirer_ministry_table <- DT::renderDataTable({
    
    hirer_ministry_table_inter <- ministry_year %>%
      filter(firm_dest_en %in% input$firm_dest_en_hirer) %>%
      filter(firm_dest_clean %in% input$firm_dest_clean_hirer) %>%
      group_by(ministry, agency, firm_dest_clean, firm_dest_en, year) %>%
      summarize(Total = sum(Total)) %>%
      ungroup() %>%
      rename(
        Ministry = ministry, 
        Agency = agency,
        `Firm name (Japanese)` = firm_dest_clean,
        `Firm name (English)` = firm_dest_en,
        Year = year) %>%
      arrange(-Total, `Firm name (Japanese)`, Ministry, Agency, Year) %>%
      slice(1:10)
  })
  
  #### Ministry-public interest corporation flows HTML widget ####
  output$ministry_publicinterest <- renderUI({
    tags$iframe(seamless="seamless", src= "tmpuser/ministry_publicinterest.html", 
                width="100%", height=800)
  })
  
  #### Ministry-private firm flows HTML widget ####
  output$ministry_private <- renderUI({
    tags$iframe(seamless="seamless", src= "tmpuser/ministry_private.html", 
                width="100%", height=800)
  })
  
  #### Ministry-industry flows HTML widget ####
  output$ministry_industry <- renderUI({
    tags$iframe(seamless="seamless", src= "tmpuser/ministry_industry.html", 
                width="100%", height=800)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

