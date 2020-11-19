#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(shinythemes)

# download data and create data sets for plotting ...
ohioDF <- read_csv("https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")
TODAY <- Sys.Date()

OhioDF <- OhioDF %>% 
    filter(Sex != "Total") %>% 
    mutate(AgeFactor = factor(`Age Range`),
           OnsetDate = mdy(`Onset Date`))

# data frame with counts by County and OnsetDate
OhioCountyDF <- OhioDF %>% 
    group_by(County, OnsetDate) %>% 
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`,
                          na.rm=TRUE),
              nhosp = sum(`Hospitalized Count`,
                          na.rm=TRUE)) 

# data frame with Blue County Data
# BC_DF <- OhioCountyDF %>% 
#     filter(County == "Butler") 

# ** check out ui (selectInput) and server (title with paste)
varnames <- c("Cases" = "ncases",
              "Deaths" = "ndead",
              "Hospitalizations" = "nhosp")

# define FirstCase date for use in input
FirstCase <- min(OhioDF$OnsetDate)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # shinythemes::themeSelector(), # can be used to explore different themes
    theme = shinytheme("readable"),
    
    
    # Application title
    titlePanel("Ohio County COVID-19 Moving Average Results"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "yvar", 
                        label= "Select response to explore: ", 
                        choices = varnames,
                        selected="cases"),
            selectInput(inputId = "county", 
                        label= "Select county to highlight: ", 
                        choices = c("Butler","Hamilton","Warren",
                                    "Preble"),
                        selected="Butler"),
            sliderInput("MAdays",
                        "Days averaged:",
                        min = 2,
                        max = 30,
                        value = 7),
            dateRangeInput("daterange",
                           "Date range:",
                           start = FirstCase,
                           end   = TODAY)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Butler County",
                         plotOutput("BCPlot")),
                tabPanel("Ohio Counties",
                         plotOutput("MAPlot"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    County_DF <- reactive({
        OhioCountyDF %>%
            filter(County == input$county)
    })
    
    output$MAPlot <- renderPlot({
        ggplot() +
            labs(x="Onset Date", y="Number",
                 title=
                     paste(names(varnames)[varnames==input$yvar],
                           " - ", 
                           input$MAdays, "d Moving Average"),
                 subtitle=paste("Updated: ",TODAY),
                 #             tag = "Butler County\nhighlighted", 
                 caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
            geom_ma(data=OhioCountyDF, 
                    aes_string(x="OnsetDate", 
                               y=input$yvar, 
                               group="County"),
                    color="lightgrey",
                    n=input$MAdays, linetype=1, size=1) +
            geom_ma(data=County_DF(), aes_string(x="OnsetDate", 
                                                 y=input$yvar),
                    n=input$MAdays, linetype=1, color="blue", size=1.25) +
            scale_x_date(date_breaks = "1 month",
                         date_labels = "%b %d",
                         limits = input$daterange) +
            theme_minimal()
    })
    
    output$CountyPlot <- renderPlot({
        ggplot() +
            labs(x="Onset Date", y="Number",
                 title=paste(names(varnames)[varnames==input$yvar],
                             " - ", 
                             input$MAdays, "d Moving Average"),
                 subtitle=paste("Updated: ",TODAY),
                 #                 tag = "Only Butler Co.",
                 caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
            # geom_ma(data=OhioCountyDF, 
            #         aes_string(x="OnsetDate", 
            #                    y=input$yvar, 
            #                    group="County"),
            #         color="lightgrey",
            #         n=input$MAdays, linetype=1, size=1) +
            geom_col(data=County_DF(), 
                     aes_string(x="OnsetDate",
                                y=input$yvar)) +
            geom_ma(data=County_DF(), 
                    aes_string(x="OnsetDate",
                               y=input$yvar),
                    n=input$MAdays, linetype=1, color="blue", 
                    size=1.25) +
            scale_x_date(date_breaks = "1 month",
                         date_labels = "%b %d",
                         limits = input$daterange) +
            theme_minimal()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

