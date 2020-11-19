#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyquant)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(scales)
library(grid)
library(plotly)

#Loading in state map and extracing just ohio with counties
map.county <- map_data('county')  
ohio.county <- subset(map.county, region=="ohio")

#get corona cases/hospitizations/deaths/data from ohio.gov
ohioDF <- read_csv("https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")

#get ohio population by county from census.gov source = https://www.census.gov/data/datasets/time-
#series/demo/popest/2010s-counties-total.html#par_textimage_739801612
ohioPop <- read_csv("co-est2019-annres-39.csv")

#filter out unneeded rows and columns from total ohio county populations before combining tables
ohioPop <- ohioPop[5:92,2]

#filter out row of totals and mutate date variable to be an actual date type
ohioDF <- ohioDF %>% 
    filter(Sex != "Total") %>% 
    mutate(OnsetDate = mdy(`Onset Date`))

#Creates dataset grouped by County with case, hospitalization, and death count, used for state map and for bottom 3 plots, removes NA values, breaks cases, hospitalizations and deaths by categorical amounts for shading purposes
ohioCountyDF <- ohioDF %>% 
    group_by(County) %>% 
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`, na.rm = TRUE),
              nhos = sum(`Hospitalized Count`, na.rm = TRUE)) %>% 
    mutate(CaseCat = cut(ncases,
                         breaks = c(0, 1000, 2500, 5000, 
                                    15000, 30000, 50000))) %>% 
    mutate(HosCat = cut(nhos,
                        breaks = c(0, 100, 500, 
                                   1000, 2000, 2500, 3000))) %>% 
    mutate(DeathCat = cut(ndead,
                          breaks = c(0, 150, 
                                     200, 350, 600, 700))) 

#match total ohio county populations with covid statistics per county so that we can analyze rates of infection.
#rename county to region to map it with ohio.county
#https://www.sharpsightlabs.com/blog/rename-columns-in-r/
combinedOhio <- cbind(ohioCountyDF, ohioPop) %>% 
    mutate(countyPop = as.double(`X2`)) %>% 
    select(!`X2`) %>% 
    mutate(rate = (ncases/countyPop) * 1000) %>% 
    mutate(County = str_to_lower(County)) %>% 
    rename(subregion = County)

#combine ohio state map and previously binded dataset together so that data can be displayed on ohio map by county
rateDB <- merge(ohio.county, combinedOhio, by.x = "subregion", by.y = "subregion")

#ohio map with discrete shading by total cases 
ohioMap <- ggplot(data = rateDB, aes(x=long,y=lat,group = group, fill=CaseCat)) +
    geom_polygon(color = "black") +
    scale_fill_brewer() +
    guides(fill = FALSE) +
    theme_map()

#Dataset for right panel of four charts, grouped by date of infection, created vars for number of cases, hospitalizations and for deaths, removed NA values
ohioCasesDF <- ohioDF %>% 
    group_by(OnsetDate) %>% 
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`, na.rm = TRUE),
              nhos = sum(`Hospitalized Count`, na.rm = TRUE))

#breaks for dates on x-axis graph every 2 months
datebreaks <- seq(as.Date("2020-01-01"), as.Date("2020-11-01"), by = "2 months")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ohio COVID-19 Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "mapvar", label= "Select a variable", 
                        choices = c("CaseCat", "HosCat", "DeathCat"),
                        selected="CaseCat")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel (
                tabPanel("Ohio Map",
                         plotOutput("OhioPlot")),
                tabPanel("Ohio Charts",
                         plotOutput("OhChartPlot"))
                #tabPanel("County Charts",
                #         plotOutput("CoChartPlot")),
                #tabPanel("Age Charts",
                 #        plotOutput("AgePlot")),
                #tabPanel("Acknowledgements",
                 #        plotOutput("Acknowledgements"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$OhioPlot <- renderPlot({
        #ohio map with discrete shading by total cases 
        ohioMap <- ggplot(data = rateDB, aes_string(x="long",y="lat",group = "group", fill=input$mapvar)) +
            geom_polygon(color = "black") +
            scale_fill_brewer() +
            guides(fill = FALSE) +
            theme_map()
        ohioMap
    })
    
    output$OhChartPlot <- renderPlot({
        casesBar <- ggplot(data = ohioCasesDF, aes(x = OnsetDate, y = ncases)) +
            geom_col(color = "blue") +
            scale_x_date(breaks = datebreaks, labels = date_format("%b %d")) +
            scale_fill_brewer() +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.subtitle = element_text(hjust = 0.5, size = 20, color = "red")) +
            labs(y = "", x = "", title = "Cases", subtitle = paste(sum(ohioCasesDF$ncases))) 
        casesBar
    })

    #output$CoChartPlot <- renderPlot({
        
    #})
    
    #output$AgePlot <- renderPlot({
        
    #})
    
    #output$Acknowledgements({
        
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
