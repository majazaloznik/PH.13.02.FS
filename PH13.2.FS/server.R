## ============================================================================
## ==== 00. preliminaries =====================================================
## ==== 01. server ============================================================
## ============================================================================


## ==== 00. preliminaries =====================================================

library(shiny)
library(rfigshare)
library(dplyr)
library(tidyr)
source("00-data-import.R", local = TRUE)


## ==== 01. server=============================================================
# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$distPlot <- renderPlot({
    # plot setup
    layout(matrix(c(1,2), nrow = 1))
    par(xpd = TRUE)
    # plot drawing
    FunPlotHalf(cntry = input$cntryL,
                grp = input$grpL,
                typ = input$typL,
                pos = "left")
    FunPlotHalf(cntry = input$cntryR,
                grp = input$grpR,
                typ = input$typR,
                pos = "right")
  })
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df %>% 
      dplyr::filter((country == input$cntryL &
                       (grepl(input$grpL, group) | group == "total") &
                       typology == input$typL) |
                      (country == input$cntryR &
                         (grepl(input$grpR, group) | group == "total") &
                         typology == input$typR))
    data
  }))
  
  observeEvent(input$copyR, {
    updateSelectInput(session, "cntryR", selected = input$cntryL)
    updateSelectInput(session, "typR", selected = input$typL)
    updateSelectInput(session, "grpR", selected = input$grpL)
  })  
  observeEvent(input$copyL, {
    updateSelectInput(session, "cntryL", selected = input$cntryR)
    updateSelectInput(session, "typL", selected = input$typR)
    updateSelectInput(session, "grpL", selected = input$grpR)
  }) 
}
