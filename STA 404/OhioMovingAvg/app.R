#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Ohio MovingAvg from Module 4 homework

library(shiny)
library(lubridate)
library(tidyquant)
library(tidyverse)


#download data and create data sets for plotting
OhioDF <- read_csv(file="https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")
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
              nhosp = sum(`Hospitalized Count`, na.rm = TRUE )) 

# data frame with Blue County Data
BC_DF <- OhioCountyDF %>% 
    filter(County == "Butler") 



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ohio County COVID-19 Moving Average Result"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "yvar", label= "Select response to explore", 
                        choices = c("ncases", "nhosp", "ndead"),
                        selected="ncases"),
            
            sliderInput("MADays",
                        "Days averaged:",
                        min = 2,
                        max = 30,
                        value = 7)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("MAPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$MAPlot <- renderPlot({
        ggplot() +
            labs(x="Onset Date", y=paste("Number of", input$yvar),
                 title=paste(input$MADays, "Day Moving Average"),
                 subtitle=paste("Updated: ",TODAY),
                 caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
            geom_ma(data=OhioCountyDF, 
                    aes_string (x="OnsetDate", y=input$yvar, group="County"),
                    color="lightgrey",
                    n=input$MADays, linetype=1, size=1) +
            geom_ma(data=BC_DF, aes_string (x="OnsetDate", y=input$yvar),
                    n=input$MADays, linetype=1, color="blue", size=1.25) +
            scale_x_date(date_breaks = "1 month",
                         date_labels = "%b %d") +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
