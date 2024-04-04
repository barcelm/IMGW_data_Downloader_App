# download and install required packages
my_packages = c("dplyr", "remotes", "lubridate", "openxlsx", "shiny","shinythemes","shinyjs")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))

library(shiny)
library(dplyr)
library(remotes)
library(lubridate)
library(openxlsx)
library(shinyjs)
library(shinythemes)


pack_climat = "climate"

install_if_missing2 = function(n) {
  if (n %in% rownames(installed.packages()) == FALSE) {
    install_github("bczernecki/climate")
  }
}

invisible(sapply(pack_climat, install_if_missing2))

library(climate)


# Define UI for data download app ----
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title ----
                navbarPage("IMGW Data Downloader",
                
                tabPanel("Download",
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Choose dataset ----
                    selectInput("rodzaj", "Type of data:",
                                choices = c('','Hydrological', 'Meteorological')),
                    
                    conditionalPanel("input.rodzaj == 'Hydrological'",
                                     selectInput("typ", "Data type:",
                                                 choices = c("",'Q', 'H')),
                                     selectInput("interval", "Data interval:",
                                                 choices = c('','Daily', 'Monthly', 'Annual'))),
                    
                    conditionalPanel("input.rodzaj == 'Meteorological'",
                                     selectInput("typ1", "Data type:",
                                                 choices = c('','Precipitation', 'Climatic', 'Synoptic')),
                                     selectInput("interval1", "Data interval:",
                                                 choices = c('','Hourly','Daily', 'Monthly'))),
                    #data----
                    renderText("Data range"),
                    column(fluidRow(selectInput('dateRange1',
                                                'From:', choices = c(1951:year(Sys.Date())))),width = 6),
                    
                    column(fluidRow(selectInput('dateRange2',
                                                'To:', choices = c((year(Sys.Date())):1951))),width = 6),
                    
                    #Stations----
                    textInput("Stacje", "Station name (if empty download all):", "", placeholder = "e.g.:POZNAŃ-MOST ROCHA "),
                    
                    #Button----
                    column(fluidRow(actionButton("downloadData", label = "Download data")),width = 6),
                    column(fluidRow(downloadButton("downloadFile", label = "Save Excel")),width = 6.1)
                    
                  ), #sidebar Panel
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    dataTableOutput("calosc_table")
                  )
                ) #sidebar layout
              ), #tabpanel
               # tabPanel("Data Visualization",
                #         mainPanel()
                 #        ),
                tabPanel("About", 
                         
                         mainPanel(
                  h5("The goal of the Application is to 
                  automatize downloading of in-situ meteorological
                  and hydrological data from publicl repository of Polish 
                     Institute of Meterology and Water Management - National 
                     Research Institute IMGW-PIB https://dane.imgw.pl/"),
                  h5("Application base on Climate R package of Bartosz Czernecki 
                     https://github.com/bczernecki/climate"),
                  h4("Version 1.0.1"),
                  h5("Latest update 2023-08-02: visual improvement, color update,added tabs, added license, 
                     description, references."),
                  h4("License"),
                  h5('Copyright (c) 2023 Bartłomiej Sobczyk'),
                  h5('Permission is hereby granted, free of charge, to any person obtaining a copy
                      of this software and associated documentation files (the "Software"), to deal
                      in the Software without restriction, including without limitation the rights
                      to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                      copies of the Software, and to permit persons to whom the Software is
                      furnished to do so, subject to the following conditions:'),
                  h5('The above copyright notice and this permission notice shall be included in all
                      copies or substantial portions of the Software.'),
                  h5('THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
                  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
                  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
                  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
                  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
                  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
                  SOFTWARE.'),
                  h4("References"),
                  citation("climate"),
                  h4("Author"),
                  verbatimTextOutput("author")
                ))
              )
    
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # render signature ----
    output$author <- renderText("Bartłomiej Sobczyk, Poznań 2023")
  # create hydro_imgw with arguments----
    
  calosc <- eventReactive(input$downloadData, {
    if (input$rodzaj == "Hydrological") {
      # choose an interval----
      interval_choice <- switch(input$interval,
                                Daily = "daily",
                                Monthly = "monthly",
                                Annual = "semiannual_and_annual")
      
      # check the interval----
      if (is.null(interval_choice)) {
        return(NULL)
      }
      
      hydro_imgw(
        interval = interval_choice,
        year = c(input$dateRange1:input$dateRange2),
        coords = T,
        value = input$typ,
        station = input$Stacje,
        col_names = "full",
        allow_failure = T
      )
    } else if (input$rodzaj == "Meteorological") {
      # choose an interval----
      interval_choice1 <- switch(input$interval1,
                                 Hourly = "hourly",
                                 Daily = "daily",
                                 Monthly = "monthly")
      rank_choice <- switch(input$typ1,
                            Precipitation = "precip",
                            Climatic = "climate",
                            Synoptic = "synop")
      # check the interval----
      if (is.null(interval_choice1)) {
        return(NULL)
      }
      meteo_data <- meteo_imgw(
        interval = interval_choice1,
        rank = rank_choice,
        year = c(input$dateRange1:input$dateRange2),
        coords = T,
        station = input$Stacje,
        col_names = "full",
        allow_failure = T
      )
      meteo_data
    }
  })
  
  # render table----
  
  output$calosc_table <- renderDataTable({
    calosc()
  })
  
  # file download----
  observeEvent(input$downloadData, {
    xlsxFile <- tempfile(fileext = ".xlsx")
    write.xlsx(calosc(), xlsxFile, asTable = FALSE)
    shinyjs::show("downloadFile")  # download button
    
    output$downloadFile <- downloadHandler(
      filename = function() {
        if (input$rodzaj == "Hydrological") {
          paste("hydro_data_", Sys.Date(), ".xlsx", sep = "")
        } else if (input$rodzaj == "Meteorological") {
          paste("meteo_data", Sys.Date(), ".xlsx", sep = "")
        }
      },
      content = function(file) {
        file.copy(xlsxFile, file)
      }
    )
  }
  )
  
  ### Data Visualization ----
  
  
}

# Create Shiny app----
shinyApp(ui, server)
