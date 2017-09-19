## ============================================================================
## ==== 00. preliminaries =====================================================
## ==== 01. ui  ===============================================================
## ============================================================================

## ==== 00. preliminaries =====================================================
library(shiny)
library(DT)
library(markdown)
source("00-data-import.R", local = TRUE)

## ==== 01. ui  ===============================================================
# Define UI for application that draws a histogram
fluidPage(
  theme = "bootstrap.css",
          
          # Application title
          titlePanel("Household Structure"),
          fluidRow(
            column(3, offset = 2,
                   selectInput('cntryL', 'Country 1', 
                               unique(levels(df$country))),
                   selectInput('typL', 'Classification of Households',
                               c("UN Typology" = "un", 
                                 "Intergenerational" = "inter")),
                   selectInput('grpL', 'Age group', 
                               c('Over 60' = "old",
                                 "Under 60" = "young"))
            ),
            column(2
                   ,
                   actionButton("copyR", "Copy ->"),
                   br(),
                   actionButton("copyL", "<- Copy")
            ),
            
            column(3,
                   selectInput('cntryR', 'Country 2', unique(levels(df$country))),
                   selectInput('typR', 'Classification of Households',
                               c("UN Typology" = "un", 
                                 "Intergenerational" = "inter")),
                   selectInput('grpR', 'Age group',  
                               c('Over 60' = "old",
                                 "Under 60" = "young"))
            ),
            tags$style(type='text/css', "#copyR { width:100%; margin-top: 25px;}"),
            tags$style(type='text/css', "#copyL { width:100%; margin-top: 15px;}")
          ),
          
          # Show a plot comparing both countries
          # And .csv selection and download
  sidebarPanel(width =2,
               helpText("Compare household living arrangments across countries,
                      ages, genders and classifications. Click on the Summary
                      tab for more details.")),
  mainPanel(width = 8, 
            tabsetPanel(position = "left",
                        tabPanel("Plot", plotOutput("distPlot")), 
                        tabPanel("Table",  DT::dataTableOutput("table")),
                        tabPanel("Summary", includeMarkdown("include.md"))
            )
  )
)


# licence css: https://github.com/thomaspark/bootswatch/blob/gh-pages/LICENSE
