#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages 
library(shiny)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(shinythemes)
library(maps)             
library(ggplot2)
library(ggmap)            
library(mapproj)
library(ggthemes)
library(plotly)
library(stringr)
library(scales)

# download and modify data
OhioDF <- read_csv(file="https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")
TODAY <- Sys.Date()
Popdata <- read_csv(file =  "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")

PopOH <- Popdata %>%
    filter(STNAME == "Ohio") %>%
    select(POPESTIMATE2019, CTYNAME) %>%
    filter(CTYNAME != "Ohio") %>%
    rename("County" = CTYNAME) %>%
    mutate(County = str_remove_all(County, " County"))

OhioDF <- OhioDF %>% 
    filter(Sex != "Total") %>% 
    mutate(AgeFactor = factor(`Age Range`),
           OnsetDate = mdy(`Onset Date`))

# data for tab 1 map

OHcounty <- OhioDF %>%
    group_by(County) %>%
    mutate(Recovered = ifelse(OnsetDate + 21 < TODAY &
                                  is.na(`Date Of Death`),
                              `Case Count`, 0)) %>%
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`,
                          na.rm=TRUE),
              nhosp = sum(`Hospitalized Count`,
                          na.rm=TRUE),
              nrec= sum(Recovered)) %>%
    mutate(CaseCats = cut(ncases, breaks=c(0, 2000, 5000, 10000,
                                           20000, 350000, 75000)))

states_map <- map_data("state")
ohio_map <- subset(states_map, states_map$region=="ohio")
map_county <- map_data('county')
oh_county <- subset(map_county, region=="ohio")
oh_county <- oh_county %>%
    mutate(subregion = str_to_title(subregion, locale = "en"))

ncases.county <- merge(oh_county, OHcounty, by.x="subregion", by.y = "County")

mcolor <- c("(0,2e+03]" = "#d4d5cb","(2e+03,5e+03]" = "#bad0d7",
            "(5e+03,1e+04]" =  "#a9cdde", "(1e+04,2e+04]" = "#81b7da",
            "(2e+04,3.5e+04]" = "#5e93c0", "(3.5e+04,7.5e+04]" = "#2b5c8a")

## rates

Pop.c<- merge(PopOH, ncases.county, by.x="County", by.y = "subregion")

Pop.c <- Pop.c %>%
    mutate(caserate = ncases/POPESTIMATE2019*10000,
           deathrate = ndead/POPESTIMATE2019*10000,
           hosprate = nhosp/POPESTIMATE2019*10000,
           recrate = nrec/POPESTIMATE2019*10000)

Pop.c <- Pop.c %>%
    mutate(`Total Cases` = ncases,
           `Total Deaths` = ndead,
           Hospitalizations = nhosp,
           Recovered = nrec,
           `Rate of Cases` = caserate,
           `Rate of Deaths` = deathrate,
           `Rate of Hospitalizations` =  hosprate,
           `Rate of Recovered` = recrate,
           `Case Category` = CaseCats) %>%
    mutate(`Rate of Cases` = round(`Rate of Cases`, digits = 2),
           `Rate of Deaths` = round(`Rate of Deaths`, digits = 2),
           `Rate of Hospitalizations` = round(`Rate of Hospitalizations`, digits = 2),
           `Rate of Recovered` = round(`Rate of Recovered`, digits = 2))


# data frame with counts by County and OnsetDate - tab 2

OhioCountyDF <- OhioDF %>%
    group_by(County, OnsetDate)  %>%
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`,
                          na.rm=TRUE),
              nhosp = sum(`Hospitalized Count`,
                          na.rm=TRUE))

BarTime <- merge(OhioCountyDF, PopOH, by= "County")

BarRates <- BarTime %>%
    mutate(arate = ncases/POPESTIMATE2019*10000,
           erate = ndead/POPESTIMATE2019*10000,
           orate = nhosp/POPESTIMATE2019*10000)%>%
    mutate(arate = round(arate, digits = 2),
           erate = round(erate, digits = 2),
           orate = round(orate, digits = 2))

rnames <- c("Cases" = "ncases",
            "Deaths" = "ndead",
            "Hospitalizations" = "nhosp",
            "Case Rate" = "arate",
            "Death Rate" = "erate",
            "Hospitalization Rate" = "orate")



# data frame for Comparison of Counties - tab 3
OhioBar <- OhioDF %>%
    group_by(County) %>%
    mutate(ncases = sum(`Case Count`),
           ndead = sum(`Death Due to Illness Count`,
                       na.rm=TRUE),
           nhosp = sum(`Hospitalized Count`,
                       na.rm=TRUE))
Ohio_Bar <- merge(OhioBar, PopOH, by="County")

Ohio_Bar <- Ohio_Bar %>%
    mutate(crate = ncases/POPESTIMATE2019*10000,
           drate = ndead/POPESTIMATE2019*10000,
           hrate = nhosp/POPESTIMATE2019*10000) %>%
    mutate(crate = round(crate, digits = 2),
           drate = round(drate, digits = 2),
           hrate = round(hrate, digits = 2)) %>%
    filter(ncases > 2000,
           ndead >50,
           nhosp > 50)

varinames <- c("Cases" = "ncases",
               "Deaths" = "ndead",
               "Hospitalizations" = "nhosp",
               "Case Rate" = "crate",
               "Death Rate" = "drate",
               "Hospitalization Rate" = "hrate")

## Data for Tab 4 - Age Comparisons

OhioAge <- OhioDF %>%
    filter(AgeFactor != "Unknown") %>%
    group_by(OnsetDate, AgeFactor, County) %>%
    mutate(Recovered = ifelse(OnsetDate + 21 < TODAY &
                                  is.na(`Date Of Death`),
                              `Case Count`, 0)) %>%
    summarize(ncases = sum(`Case Count`),
              ndead = sum(`Death Due to Illness Count`,
                          na.rm=TRUE),
              nhosp = sum(`Hospitalized Count`,
                          na.rm=TRUE),
              nrec= sum(Recovered))

vnames <- c("Cases" = "ncases",
            "Deaths" = "ndead",
            "Hospitalizations" = "nhosp",
            "Recovered Cases" = "nrec")


### Data for Tab 5

HeatAge <- OhioDF %>%
    filter(AgeFactor != "Unknown") %>%
    group_by(OnsetDate, AgeFactor, County) %>%
    summarize(Cases = sum(`Case Count`),
              Deaths = sum(`Death Due to Illness Count`,
                          na.rm=TRUE),
              Hospitalizations = sum(`Hospitalized Count`,
                          na.rm=TRUE)) 

age_var <- c("Cases" = "Cases",
             "Deaths" = "Deaths",
             "Hospitalizations" = "Hospitalizations")


# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("readable"),
    # Application title
    titlePanel("Ohio COVID-19 Data"),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Ohio Map",
                         plotlyOutput("MapPlot")),
                tabPanel("Counts Over Time",
                         plotOutput("Responses"),
                         sidebarPanel(
                             selectInput(inputId = "yvar",
                                         label = "Select response to explore: ",
                                         choices = rnames,
                                         selected = "Cases"),
                             selectInput(inputId = "county",
                                         label = "Select county to highlight: ",
                                         choices = unique(OhioDF$County),
                                         selected = "Butler"),
                             sliderInput("MAdays",
                                         "Days averaged:",
                                         min = 2,
                                         max = 30,
                                         value = 7))),
                tabPanel("Comparison of Counties",
                         plotOutput("CountyPlot"),
                         sidebarPanel(
                             selectInput(inputId = "xvar",
                                         label = "Select response to explore: ",
                                         choices = varinames,
                                         selected = "Cases"))),
                tabPanel("Age Group Comparisons",
                         plotOutput("AgePlot"),
                         sidebarPanel(
                             selectInput(inputId = "agev",
                                         label = "Select response to explore:",
                                         choices = vnames,
                                         selected = "Cases"),
                             selectInput(inputId = "county2",
                                         label = "Select county to highlight: ",
                                         choices = unique(OhioAge$County),
                                         selected = "Butler"),
                             downloadButton(outputId = "down", label = "Download the Plot"))),
                tabPanel("Age Group Heat Map",
                         plotlyOutput("Agemap"),
                         sidebarPanel(
                             selectInput(inputId = "agevar",
                                         label = "Select a response to explore:",
                                         choices = age_var,
                                         selected = "Cases"),
                             selectInput(inputId = "county3",
                                         label = "Select county to highlight: ",
                                         choices = unique(HeatAge$County),
                                         selected = "Butler"))),
                tabPanel("Acknowledgements and References",
                         mainPanel(
                             h1("Ashley Lefebvre"),
                             br(),
                             h2("Date of Construction: Dec-4-2020"),
                             br(),
                             h3("Data Sources:"),
                             p("-https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv"),
                             p("-https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"),
                             br(),
                             h3("Citation of Packages"),
                             p("-R Core Team (2020). R: A language and environment for
                                statistical computing. R Foundation for Statistical
                                Computing, Vienna, Austria. URL
                                https://www.R-project.org/."),
                             p("-Garrett Grolemund, Hadley Wickham (2011). Dates and
                                Times Made Easy with lubridate. Journal of Statistical
                                Software, 40(3), 1-25. URL
                                http://www.jstatsoft.org/v40/i03/."),
                             p("-Matt Dancho and Davis Vaughan (2020). tidyquant: Tidy
                                Quantitative Financial Analysis. R package version
                                1.0.1. https://CRAN.R-project.org/package=tidyquant"),
                             p("-Original S code by Richard A. Becker, Allan R. Wilks.
                                R version by Ray Brownrigg. Enhancements by Thomas P
                                Minka and Alex Deckmyn. (2018). maps: Draw
                                Geographical Maps. R package version 3.3.0.
                                https://CRAN.R-project.org/package=maps"),
                             p("-H. Wickham. ggplot2: Elegant Graphics for Data
                               Analysis. Springer-Verlag New York, 2016."),
                             p("-D. Kahle and H. Wickham. ggmap: Spatial Visualization
                                with ggplot2. The R Journal, 5(1), 144-161. URL
                                http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf"),
                             p("- Doug McIlroy. Packaged for R by Ray Brownrigg, Thomas
                                 P Minka and transition to Plan 9 codebase by Roger
                                Bivand. (2020). mapproj: Map Projections. R package
                                version 1.2.7.
                                https://CRAN.R-project.org/package=mapproj"),
                             p("-Jeffrey B. Arnold (2019). ggthemes: Extra Themes,
                                Scales and Geoms for 'ggplot2'. R package version
                                4.2.0. https://CRAN.R-project.org/package=ggthemes"),
                             p("-Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and
                                Jonathan McPherson (2020). shiny: Web Application
                                Framework for R. R package version 1.5.0.
                                https://CRAN.R-project.org/package=shiny"),
                             p("-Winston Chang (2018). shinythemes: Themes for Shiny. R
                                package version 1.1.2.
                                https://CRAN.R-project.org/package=shinythemes"),
                             p("- C. Sievert. Interactive Web-Based Data Visualization
    `                           with R, plotly, and shiny. Chapman and Hall/CRC
                                Florida, 2020."),
                             p("-Hadley Wickham (2019). stringr: Simple, Consistent
                                Wrappers for Common String Operations. R package
                                version 1.4.0.
                                https://CRAN.R-project.org/package=stringr"),
                             p("- Hadley Wickham and Dana Seidel (2020). scales: Scale
                                Functions for Visualization. R package version 1.1.1.
                                https://CRAN.R-project.org/package=scales")))
                         ))
        )


# Define server logic required to draw a histogram
server <- function(input, output) {
    County_DF <- reactive({
        OhioCountyDF %>%
            filter(County %in% c(input$county))})

    output$MapPlot <- renderPlotly({  #done
        ggplotly(
        StateCount <-ggplot(data = Pop.c, aes(x=long, y=lat, group=County, fill=`Case Category`,
                                              cn=`Total Cases`, ch=Hospitalizations, cd=`Total Deaths`,cr=Recovered,
                                              rn=`Rate of Cases`, rh=`Rate of Hospitalizations`, rd=`Rate of Deaths`, 
                                              rr=`Rate of Recovered`)) +
            geom_polygon(color="black") + 
            coord_map("polyconic") +
            scale_fill_manual(values = mcolor) +
            labs(title = "COVID-19 Interactive Map of Ohio",
                 subtitle=paste("Updated: ",TODAY),
                 caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  panel.background = element_blank(),
                  legend.position = "none"),
        tooltip = c("group", "fill", "cn", "ch", "cd", "cr", "rn", "rh", "rd", "rr"))
    })
        
    output$Responses <- renderPlot({ #done
        County_DF <- reactive({
            BarRates %>%
                filter(County %in% c(input$county))})
        
        ggplot() +
            labs(x="Onset Date", y="Number",
                 title=
                     paste(names(rnames)[rnames==input$yvar],
                           " - ",
                           input$MAdays, "d Moving Average"),
                 subtitle=paste("Updated: ",TODAY),
                 tag = paste(input$county, "\nCounty\nhighlighted"),
                 caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
            geom_ma(data=BarRates,
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
    
    output$CountyPlot <- renderPlot({  #done
        ggplot(Ohio_Bar,aes_string(y="fct_reorder(County, ncases)",
                   x=input$xvar)) +
            labs( tag = paste(names(varinames)[varinames==input$xvar],
                  "- Comparison by County"),
                         subtitle=paste("Updated: ",TODAY),
                         caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv",
                  x=paste(names(varinames)[varinames==input$xvar]),
                  y="County") +
            geom_col(position = "dodge",
                     fill="steelblue",
                     color="black") +
            theme(panel.grid = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.position = "none",
                  axis.text.x = element_blank()) +
            theme_minimal()
    })
    
    output$AgePlot <- renderPlot({ #done
        County_DF <- reactive({
            OhioAge %>%
                filter(County %in% c(input$county2))})
        
        PlotL <- ggplot(County_DF(), aes_string(x="OnsetDate", y=input$agev)) +
            geom_line() +
            scale_x_date(date_breaks = "3 month",
                         date_labels = "%b %d") +
            facet_wrap(~ AgeFactor, scales = "free_y") +
            labs(x="Onset Date", y=paste(names(vnames[vnames==input$agev])),
                 tag = paste(input$county2,
                             "- Comparison of Age Groups"),
                 subtitle=paste("Updated: ",TODAY),
                 caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
            theme(strip.text = element_text(face = "bold", size = rel(1)),
                strip.background = element_rect(fill = "lightblue", colour = "black",
                                                size = 1))
        PlotL
    })
    
    output$Agemap <- renderPlotly({ #done
        County_DF <- reactive({
            HeatAge %>%
                filter(County %in% c(input$county3))})
        
        ggplotly(
        ggplot(County_DF(), aes_string(x="OnsetDate", y="AgeFactor", fill=input$agevar)) +
            geom_raster() +
            labs(x="Onset Date", 
                 y=paste(names(age_var[age_var==input$agevar])),
                 tag = paste(input$county3,
                             "- Comparison of Age Groups"),
                 subtitle=paste("Updated: ",TODAY),
                 caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
            scale_fill_viridis_c() +
            scale_x_date(date_breaks = "1 month",
                         date_labels = "%b %d"))
    })
    
    output$down <- downloadHandler(
        filename = function() { #specify the filename
        paste("AgePlot", "png", sep = ".")
        },
        content = function(file){
        ggsave(file, plot=PlotL, device="png")
            dev.off()
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
