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
library(readxl)
library(plotly)


#Loading in state map and extracing just ohio with counties
map.county <- map_data('county')  
ohio.county <- subset(map.county, region=="ohio")

#get corona cases/hospitizations/deaths/data from ohio.gov
ohioDF <- read_csv("https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")

#get ohio population by county from census.gov source = https://www.census.gov/data/datasets/time-
#series/demo/popest/2010s-counties-total.html#par_textimage_739801612
ohioPop <- read_excel("co-est2019-annres-39.xlsx")

#filter out unneeded rows and columns from total ohio county populations before combining tables
ohioPop <- ohioPop[5:92,2]

#Get today's date
Today <- Sys.Date()

#filter out row of totals and mutate date variable to be an actual date type
ohioDF <- ohioDF %>% 
    filter(Sex != "Total") %>% 
    mutate(AgeFactor = factor(`Age Range`), OnsetDate = mdy(`Onset Date`))

#Creates dataset grouped by County with case, hospitalization, and death count, used for state map and for bottom 3 plots, removes NA values, breaks cases, hospitalizations and deaths by categorical amounts for shading purposes
ohioCountyDF <- ohioDF %>% 
    group_by(County) %>% 
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`, na.rm = TRUE),
              nhos = sum(`Hospitalized Count`, na.rm = TRUE),
              nrec = (ncases - ndead)) %>% 
    mutate(Cases = cut(ncases,
                         breaks = c(0, 1000, 2250, 4000, 13000,
                                    15000, 30000, 35000, 70000))) %>% 
    mutate(Hospitalizations = cut(nhos,
                        breaks = c(0, 100, 500, 
                                   1000, 2000, 2500, 5000))) %>% 
    mutate(Deaths = cut(ndead,
                          breaks = c(0, 10, 20, 60, 150, 
                                     200, 300, 600, 900))) %>% 
    mutate(Recovered = cut(nrec, 
                            breaks = c(0, 500, 1000, 3000, 10000, 15000, 25000, 50000, 100000)))


#match total ohio county populations with covid statistics per county so that we can analyze rates of infection.
#rename county to region to map it with ohio.county
#https://www.sharpsightlabs.com/blog/rename-columns-in-r/
combinedOhio <- cbind(ohioCountyDF, ohioPop) %>% 
    mutate(countyPop = as.double(`...2`)) %>% 
    select(!`...2`) %>% 
    mutate(County = str_to_lower(County)) %>% 
    rename(subregion = County)

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
                        choices = c("Cases", "Hospitalizations", "Deaths", "Recovered"),
                        selected="Cases"),
            selectInput(inputId = "county", label= "Select a county", 
                        choices = unique(ohioCountyDF$County),
                        selected="Franklin"),
            checkboxInput(inputId= "rate", label = "Rates"),
            downloadButton("downloadFile", "Download File for Current Tab"),
            downloadButton("downloadImage", "Download Image")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel (
                tabPanel("Ohio Map", value = 1,
                         plotlyOutput("OhioPlot")),
                tabPanel("Ohio Charts", value =2,
                         plotOutput("OhChartPlot")),
                tabPanel("County Charts", value =3,
                         plotOutput("CoChartPlot")),
                tabPanel("Age Charts", value =4,
                         plotOutput("AgePlot")),
                tabPanel("Acknowledgements", value =5,
                         textOutput("Acknowledgements")),
                id = "tabselected"
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Dataset grouped by date of infection, created vars for number of cases, hospitalizations and for deaths, removed NA values
    ohioCasesDF <- reactive ({ ohioDF %>% 
        filter(County == input$county) %>% 
        group_by(OnsetDate) %>% 
        summarize(Cases = sum(`Case Count`),
                  Deaths = sum(`Death Due to Illness Count`, na.rm = TRUE),
                  Hospitalizations = sum(`Hospitalized Count`, na.rm = TRUE),
                  Recovered = sum(case_when((OnsetDate < Today - 21) ~ (Cases - Deaths)))) }) 

    #used for Age plot
    ageDF <- reactive ({ohioDF %>% 
            filter(County == input$county, AgeFactor != "Unknown") %>% 
            group_by(AgeFactor, OnsetDate) %>% 
            summarize(Cases = sum(`Case Count`),
                      Deaths = sum(`Death Due to Illness Count`, na.rm = TRUE),
                      Hospitalizations = sum(`Hospitalized Count`, na.rm = TRUE),
                      Recovered = sum(`Case Count`) - sum(`Death Due to Illness Count`, na.rm = TRUE)) 
        
    })
    
    #adds recovery variable
    ohioRec <- reactive({ ohioCasesDF() %>%
        mutate(Recovered = case_when((OnsetDate < Today - 21) ~ (Cases - Deaths)))  })
    
    
    #combine ohio state map and previously binded dataset together so that data can be displayed on ohio map by county
    rateDB <-  reactive ({merge(ohio.county, combinedOhio, by.x = "subregion", by.y = "subregion") %>% 
            mutate(rate = (if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else (if(input$mapvar == "Hospitalizations") nhos else (ncases-ndead))) /countyPop) * 1000) })
    
    output$OhioPlot <- renderPlotly({
        if(input$rate) {
           #ohio map with discrete shading by rates
           ggplot(data = rateDB(), aes_string(x="long",y="lat",group = "group", fill=rateDB()$rate)) +
                geom_polygon(color = "black") +
                scale_fill_gradient2(low="#559999",mid="grey90",high="#BB650B",
                                     midpoint=median(rateDB()$rate)) +
                labs(title = paste("Ohio COVID-19", input$mapvar, "Rate per 1000 Residents")) +
                theme_map()+
                theme(legend.position = "none")
        } else {
        #ohio map with discrete shading by total cases
            ggplot(data = rateDB(), aes_string(x="long",y="lat",group = "group", fill=input$mapvar)) +
                geom_polygon(color = "black") +
                labs(title = paste("Ohio COVID-19", input$mapvar, "Count per 1000 Residents")) +
                scale_fill_brewer() +
                theme_map() +
                theme(legend.position = "none")
        }
    })
    
    output$OhChartPlot <- renderPlot({
        ggplot(data = ohioRec(), aes_string(x = (if(input$mapvar == "Recovered") ohioRec()$OnsetDate + 21 else ohioRec()$OnsetDate), y = input$mapvar)) +
            geom_col(color = "blue") +
            geom_ma(n = 7, linetype = 1, color = "red", size = 1) +
            scale_x_date(breaks = datebreaks, labels = date_format("%b %d")) +
            scale_fill_brewer() +
            theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.subtitle = element_text(hjust = 0.5, size = 20, color = "red")) +
            labs(y = "", x = "", title = paste(input$mapvar, "in", input$county, "County"))
    })

    output$CoChartPlot <- renderPlot({
        ggplot(data = ohioCountyDF[tail(order(if(input$mapvar == "Cases") ohioCountyDF$ncases else (if(input$mapvar == "Deaths") ohioCountyDF$ndead else (if(input$mapvar == "Hospitalizations") ohioCountyDF$nhos else ohioCountyDF$nrec))), 20), ],
               aes(x=reorder(County, -if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else (if(input$mapvar == "Hospitalizations") nhos else nrec))),
                   y=if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else (if(input$mapvar == "Hospitalizations") nhos else nrec)),
                   fill = !!as.symbol(input$mapvar))) +
            geom_col() +
            scale_fill_brewer() +
            guides(fill = FALSE) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.25),  axis.ticks.x = element_blank(), plot.title =
                      element_text(hjust = 0.5), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor
                  = element_blank()) +
            labs(y = paste(input$mapvar), x = "County", title = paste(input$mapvar, "Count"))
    })
    
    output$AgePlot <- renderPlot({
        ggplot(data = ageDF(), aes_string(x = (if(input$mapvar == "Recovered") ageDF()$OnsetDate + 21 else ageDF()$OnsetDate) , y = input$mapvar, group = "AgeFactor", color = "AgeFactor")) +
            geom_ma(n = 7, linetype = 1, size = 1) +
            labs(x = "Age", y = "Number of Cases", title = paste(input$mapvar, "by Age for", input$county, "County"), color = "Age") +
            theme_minimal()
    })
    
    output$Acknowledgements <- renderText({
       "Created by John Doll\nLast Edited 11-25-2020\nSources:\n\tR Studio"
    })
    
    output$downloadFile <- downloadHandler(
        filename = "COVID-19 Dataset",
        content = function(file) {
            if(input$tabselected==1) {
                write.csv(rateDB(), file, row.names = FALSE)
            } else if (input$tabselected==2) {
                 write.csv(ohioRec(), file, row.names = FALSE)
            } else if (input$tabselected==3) {
                write.csv(ohioCountyDF(), file, row.names = FALSE)
            } else if (input$tabselected==4) {
                write.csv(ageDF(), file, row.names = FALSE)
            }
        }
    )
    
    output$downloadImage <- downloadHandler(
        filename = "COVID-19 Graph",
        content = function(file) {
            if(input$tabselected == 1 && input$rate) {
                ggsave(file, plot = ggplot(data = rateDB(), aes_string(x="long",y="lat",group = "group", fill=rateDB()$rate)) +
                           geom_polygon(color = "black") +
                           scale_fill_gradient2(low="#559999",mid="grey90",high="#BB650B",
                                                midpoint=median(rateDB()$rate)) +
                           labs(title = paste("Ohio COVID-19", input$mapvar, "Rate per 1000 Residents")) +
                           theme_map()+
                           theme(legend.position = "none"), device = "png", height = 6, width = 6)
            } else if (input$tabselected == 1 && !input$rate) {
                ggsave(file, plot = ggplot(data = rateDB(), aes_string(x="long",y="lat",group = "group", fill=input$mapvar)) +
                           geom_polygon(color = "black") +
                           labs(title = paste("Ohio COVID-19", input$mapvar, "Count per 1000 Residents")) +
                           scale_fill_brewer() +
                           theme_map() +
                           theme(legend.position = "none"), device = "png", height = 6, width = 6)
            } else if (input$tabselected == 2) {
                ggsave(file, plot = ggplot(data = ohioRec(), aes_string(x = (if(input$mapvar == "Recovered") ohioRec()$OnsetDate + 21 else ohioRec()$OnsetDate), y = input$mapvar)) +
                           geom_col(color = "blue") +
                           geom_ma(n = 7, linetype = 1, color = "red", size = 1) +
                           scale_x_date(breaks = datebreaks, labels = date_format("%b %d")) +
                           scale_fill_brewer() +
                           theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.subtitle = element_text(hjust = 0.5, size = 20, color = "red")) +
                           labs(y = "", x = "", title = paste(input$mapvar, "in", input$county, "County")), device = "png", height = 6, width = 6)
            } else if (input$tabselected == 3) {
                ggsave(file, plot = ggplot(data = ohioCountyDF[tail(order(if(input$mapvar == "Cases") ohioCountyDF$ncases else (if(input$mapvar == "Deaths") ohioCountyDF$ndead else (if(input$mapvar == "Hospitalizations") ohioCountyDF$nhos else ohioCountyDF$nrec))), 20), ],
                                           aes(x=reorder(County, -if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else (if(input$mapvar == "Hospitalizations") nhos else nrec))),
                                               y=if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else (if(input$mapvar == "Hospitalizations") nhos else nrec)),
                                               fill = !!as.symbol(input$mapvar))) +
                           geom_col() +
                           scale_fill_brewer() +
                           guides(fill = FALSE) +
                           theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1.25),  axis.ticks.x = element_blank(), plot.title =
                                     element_text(hjust = 0.5), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor
                                 = element_blank()) +
                           labs(y = paste(input$mapvar), x = "County", title = paste(input$mapvar, "Count")), device = "png", height = 6, width = 6)
            } else if (input$tabselected == 4) {
                ggsave(file, plot = ggplot(data = ageDF(), aes_string(x = (if(input$mapvar == "Recovered") ageDF()$OnsetDate + 21 else ageDF()$OnsetDate) , y = input$mapvar, group = "AgeFactor", color = "AgeFactor")) +
                           geom_ma(n = 7, linetype = 1, size = 1) +
                           labs(x = "Age", y = "Number of Cases", title = paste(input$mapvar, "by Age for", input$county, "County"), color = "Age") +
                           theme_minimal(), device = "png", height = 6, width = 6)
            }
        }
    )
}


#ggsave(file, plot = ohioMap, device = "png")
# Run the application 
shinyApp(ui = ui, server = server)