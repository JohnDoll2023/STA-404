#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#  
# Project2.R
# DIR  Users/johndoll/Sophomore Year/STA 404/STA 404/Project2
# Revised 12/01/20

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

#Loading in state map and extracting just ohio with counties
#https://stringr.tidyverse.org/reference/case.html
map.county <- map_data('county')  
ohio.county <- subset(map.county, region=="ohio") %>% 
    mutate(County = str_to_title(subregion)) %>% 
    select(!subregion)

#get corona cases/hospitalizations/deaths/data from ohio.gov then filter out row of totals and mutate date variable to be an actual date type
ohioDF <- read_csv("https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv") %>% 
    filter(Sex != "Total") %>% 
    mutate(AgeFactor = factor(`Age Range`), OnsetDate = mdy(`Onset Date`))

#get ohio population by county from census.gov source = https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv and filter for just ohio and then for just counties, then select their populations
ohioPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") %>% 
    filter(STNAME == "Ohio", CTYNAME != "Ohio") %>% 
    mutate(CTYNAME = str_remove(CTYNAME, " County")) %>% 
    select(CTYNAME, POPESTIMATE2019)

ohioDF <- merge(ohioDF, ohioPop, by.x = "County", by.y = "CTYNAME")

#Creates data set grouped by County with case, hospitalization, and death count, removes NA values, breaks cases, hospitalizations and deaths by categorical amounts for shading purposes
ohioCountyDF <- ohioDF %>% 
    group_by(County) %>% 
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`, na.rm = TRUE),
              nhos = sum(`Hospitalized Count`, na.rm = TRUE)) %>% 
    mutate(Cases = cut(ncases,
                         breaks = c(0, 1000, 2250, 4000, 13000,
                                    15000, 30000, 35000, 100000))) %>% 
    mutate(Hospitalizations = cut(nhos,
                        breaks = c(0, 100, 500, 
                                   1000, 2000, 2500, 10000))) %>% 
    mutate(Deaths = cut(ndead,
                          breaks = c(0, 10, 20, 60, 150, 
                                     200, 300, 600, 1000)))

#match total ohio county populations with covid statistics per county so that we can analyze rates of infection.
#rename county to region to map it with ohio.county
#https://www.sharpsightlabs.com/blog/rename-columns-in-r/
combinedOhio <- cbind(ohioCountyDF, ohioPop) %>% 
    mutate(countyPop = as.double(`POPESTIMATE2019`)) %>% 
    select(!c(`POPESTIMATE2019`, `CTYNAME`))

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
                        choices = c("Cases", "Hospitalizations", "Deaths"),
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
            tableOutput("check"),
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
    
    #Dataset grouped by date of infection, created vars for number of cases, hospitalizations and for deaths, removed NA values, only for selected county
    ohioCasesDF <- reactive ({ ohioDF %>% 
        filter(County == input$county) %>% 
        group_by(OnsetDate) %>% 
        summarize(Cases = sum(`Case Count`),
                  Deaths = sum(`Death Due to Illness Count`, na.rm = TRUE),
                  Hospitalizations = sum(`Hospitalized Count`, na.rm = TRUE),
                  Population = `POPESTIMATE2019`) }) 

    #Data set used for Age plot, filters for selected county and removes unknown age cases
    ageDF <- reactive ({ohioDF %>% 
            filter(County == input$county, AgeFactor != "Unknown") %>% 
            group_by(AgeFactor, OnsetDate) %>% 
            summarize(Cases = sum(`Case Count`),
                      Deaths = sum(`Death Due to Illness Count`, na.rm = TRUE),
                      Hospitalizations = sum(`Hospitalized Count`, na.rm = TRUE),
                      Population = `POPESTIMATE2019`) })
    
    #combine ohio state map and previously binded data set together so that data can be displayed on ohio map by county
    rateDB <-  reactive ({merge(ohio.county, combinedOhio, by.x = "County", by.y = "County") %>% 
            mutate(rate = round(((if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else nhos)) /countyPop) * 1000, digits = 2)) })
    
    output$OhioPlot <- renderPlotly({
        if(input$rate) {
           #ohio map with discrete shading by rates
           mapgg <- ggplot(data = rateDB(), aes(x=long,y=lat,group = group, fill = rateDB()$rate, text = paste('County: ', County, "<br>Population: ", countyPop, "<br>Cases: ", ncases, "<br>Deaths: ", ndead, "<br>Hospitalizations: ", nhos, "<br>Rate (", input$mapvar, "): ", rateDB()$rate, sep = ""))) +
                geom_polygon(color = "black") +
                scale_fill_gradient2(low="#559999",mid="grey90",high="#BB650B",
                                     midpoint=median(rateDB()$rate)) +
                labs(title = paste("Ohio COVID-19", input$mapvar, "Rate per 1000 Residents")) +
                theme_map()+
                theme(legend.position = "none")
        } else {
        #ohio map with discrete shading by total cases
            mapgg <- ggplot(data = rateDB(), aes(x=long,y=lat,group = group, fill = !!as.symbol(input$mapvar), text = paste('County: ', County, "<br>Population: ", countyPop, "<br>Cases: ", ncases, "<br>Deaths: ", ndead, "<br>Hospitalizations: ", nhos, "<br>Rate (", input$mapvar, "): ", rateDB()$rate, sep = ""))) +
                geom_polygon(color = "black") +
                labs(title = paste("Ohio COVID-19", input$mapvar, "Count per 1000 Residents")) +
                scale_fill_brewer() +
                theme_map() +
                theme(legend.position = "none")
        }
        ggplotly(mapgg, tooltip = c("text")) %>%
            layout(showlegend = FALSE)
    })
    
    output$OhChartPlot <- renderPlot({
        ggplot(data = ohioCasesDF(), aes(x = OnsetDate, y = if(input$rate) !!as.symbol(input$mapvar)/Population else !!as.symbol(input$mapvar))) +
            geom_col(color = "blue") +
            geom_ma(n = 7, linetype = 1, color = "red", size = 1) +
            scale_x_date(breaks = datebreaks, labels = date_format("%b %d")) +
            scale_fill_brewer() +
            theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), plot.subtitle = element_text(hjust = 0.5, size = 20, color = "red")) +
            labs(y = if(input$rate) paste("Rate of", if(input$mapvar == "Cases") "Infected (Infected/County Population)" else (if(input$mapvar == "Deaths") "Deaths (Deaths/County Population" else "Hospitalizations (Hospitalizations/County Population)")) else "", x = "", title = paste(input$mapvar, "in", input$county, "County"))
    })

    output$CoChartPlot <- renderPlot({
        ggplot(data = combinedOhio[tail(order((if(input$rate) (if(input$mapvar == "Cases") combinedOhio$ncases else (if(input$mapvar == "Deaths") combinedOhio$ndead else combinedOhio$nhos))/combinedOhio$countyPop else (if(input$mapvar == "Cases") combinedOhio$ncases else (if(input$mapvar == "Deaths") combinedOhio$ndead else combinedOhio$nhos)))), 20), ],
               aes(x = if(input$rate) ((if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else nhos))/countyPop) else (if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else nhos)),
                   y = reorder(County, -if(input$rate) ((if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else nhos))/countyPop) else (if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else nhos))),
                   fill = !!as.symbol(input$mapvar))) +
            geom_col() +
            scale_fill_brewer() +
            guides(fill = FALSE) +
            theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), plot.subtitle = element_text(hjust = 0.5)) +
            labs(y = paste(input$mapvar), x = "County", title = paste(input$mapvar, "Count"), subtitle = paste("Shading based on total", input$mapvar))
    })
    
    output$AgePlot <- renderPlot({
        ggplot(data = ageDF(), aes(x = OnsetDate , y = if(input$rate) !!as.symbol(input$mapvar)/Population else !!as.symbol(input$mapvar), group = AgeFactor, color = AgeFactor)) +
            geom_ma(n = 7, linetype = 1, size = 1) +
            scale_x_date(breaks = datebreaks, labels = date_format("%b %d")) +
            labs(x = "Age", y = paste(if(input$rate) "Rate of" else "Number of" , input$mapvar), title = paste(input$mapvar, "by Age for", input$county, "County"), color = "Age") +
            theme_minimal() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    })
    
    output$Acknowledgements <- renderText({
       "Created by John Doll\nLast Edited 11-25-2020\nSources:\n\tR Studio"
        #ggsaave https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2/topics/ggsave
        #as.symbol not aes_string https://stackoverflow.com/questions/35345782/shiny-passing-inputvar-to-aes-in-ggplot2
        #input$tabselected https://stackoverflow.com/questions/38863215/how-do-i-access-print-track-the-current-tab-selection-in-a-shiny-app
        #file download https://shiny.rstudio.com/articles/download.html
        #nrec https://www.statology.org/conditional-mutating-r/
        #ternary operator https://stackoverflow.com/questions/8790143/does-the-ternary-operator-exist-in-r
        #get rid of legend for chlorpleth https://www.bing.com/search?q=get+rid+of+legend+in+plotly&FORM=AWRE
        #limit decimal places plotly https://stackoverflow.com/questions/42141878/limit-decimal-places-in-variable-in-r
        #renderplotly https://www.bing.com/search?FORM=U523DF&PC=U523&q=use+plotly+in+shiny+app
        #many things here and there, mainly with axes and labels https://r-graphics.org/recipe-legend-title-text
        #tooltip https://stackoverflow.com/questions/38733403/edit-labels-in-tooltip-for-plotly-maps-using-ggplot2-in-r
        #separator in paste https://r.789695.n4.nabble.com/paste-eliminate-spaces-td792315.html
    })
    
    output$downloadFile <- downloadHandler(
        filename = "COVID-19 Dataset",
        content = function(file) {
            #filters for which tab is currently selected so that correct data set can be downloaded
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
            #filters for tab and for some input variables so that correct graph can be downloaded
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
                ggsave(file, plot = ggplot(data = ohioCasesDF(), aes_string(x = "OnsetDate", y = input$mapvar)) +
                           geom_col(color = "blue") +
                           geom_ma(n = 7, linetype = 1, color = "red", size = 1) +
                           scale_x_date(breaks = datebreaks, labels = date_format("%b %d")) +
                           scale_fill_brewer() +
                           theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank(), plot.subtitle = element_text(hjust = 0.5, size = 20, color = "red")) +
                           labs(y = "", x = "", title = paste(input$mapvar, "in", input$county, "County")), device = "png", height = 6, width = 6)
            } else if (input$tabselected == 3) {
                ggsave(file, plot = ggplot(data = ohioCountyDF[tail(order(if(input$mapvar == "Cases") ohioCountyDF$ncases else (if(input$mapvar == "Deaths") ohioCountyDF$ndead else ohioCountyDF$nhos)), 20), ],
                                           aes(x = if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else nhos),
                                               y = reorder(County, -if(input$mapvar == "Cases") ncases else (if(input$mapvar == "Deaths") ndead else nhos)),
                                               fill = !!as.symbol(input$mapvar))) +
                           geom_col() +
                           scale_fill_brewer() +
                           guides(fill = FALSE) +
                           theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank()) +
                           labs(y = paste(input$mapvar), x = "County", title = paste(input$mapvar, "Count")), device = "png", height = 6, width = 6)
            } else if (input$tabselected == 4) {
                ggsave(file, plot = ggplot(data = ageDF(), aes_string(x = "OnsetDate" , y = input$mapvar, group = "AgeFactor", color = "AgeFactor")) +
                           geom_ma(n = 7, linetype = 1, size = 1) +
                           scale_x_date(breaks = datebreaks, labels = date_format("%b %d")) +
                           labs(x = "Age", y = "Number of Cases", title = paste(input$mapvar, "by Age for", input$county, "County"), color = "Age") +
                           theme_minimal() +
                           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()), device = "png", height = 6, width = 6)
            }
        }
    )
    
    # output$check <- renderTable ({
    #     view(ageDF())
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)