#load packages
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(ggplot2, lubridate, tidyverse, patchwork,maps,ggmap,mapproj,ggthemes,shiny,shinythemes,plotly,tidyquant)


# read in the data
ohioCovidDashboard <- "https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv"
OhioDF <- read_csv(file= ohioCovidDashboard)
TODAY <- Sys.Date()

#clean data
OhioDF<-OhioDF %>% 
    filter(Sex != "Total")

OhioDF<- OhioDF %>% rename(
    OnsetDate=`Onset Date`
)


#mutate data types 
OhioDF<- OhioDF %>% 
    mutate(AgeFactor = factor(`Age Range`),
           OnsetDate = mdy(OnsetDate))



#group by county for tab 1
ohio_county_df<- OhioDF %>% 
    group_by(County) %>% 
    summarise(ncases = sum(`Case Count`),
              nhosp=sum(`Hospitalized Count`, na.rm = TRUE),
              ndead=sum(`Death Due to Illness Count`, na.rm = TRUE))

#group by County and OnsetDate for tab 2
ohio_county_onset_df <- OhioDF %>%
    group_by(County, OnsetDate) %>%
    summarize(ncases = sum(`Case Count`),
              nhosp = sum(`Hospitalized Count`,
                          na.rm=TRUE),
              ndead = sum(`Death Due to Illness Count`,
                          na.rm=TRUE))

#create segments of cases to separate the bar graph by color
ohio_county_df<- ohio_county_df %>% 
    mutate(CaseCat = cut(ncases,
                         breaks = c(0, 1000, 2500, 5000, 
                                    15000, 30000, 75000 )))

#create segments of cases to separate the bar graph by color
ohio_county_onset_df<- ohio_county_onset_df %>% 
    mutate(CaseCat = cut(ncases,
                         breaks = c(0, 1000, 2500, 5000, 
                                    15000, 30000, 75000 )))

# Ohio Population data
OhioCountyPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
OhioPop <- OhioCountyPop %>%
    filter(STNAME == "Ohio") %>%
    filter(CTYNAME != "Ohio") %>%
    mutate(County = str_remove_all(CTYNAME," County"),
           Pop2019 = POPESTIMATE2019) %>%
    select(County, Pop2019)

# add county population to DF grouped by county and construct rates
OhioCountyDF <- merge(ohio_county_df, OhioPop, by="County")

OhioCountyDF <- OhioCountyDF %>% 
    mutate(caseRate10K = round((ncases/Pop2019*10000),2),
           deathRate10K = round((ndead/Pop2019*10000),2),
           hospRate10K = round((nhosp/Pop2019*10000),2))

FirstCase <- min(OhioCountyDF$OnsetDate)
LastCase <- max(OhioCountyDF$OnsetDate)


# add county population to DF grouped by county and onset date and construct rates
OhioCountyOnsetDF <- merge(ohio_county_onset_df, OhioPop, by="County")
OhioCountyOnsetDF <- OhioCountyOnsetDF %>% 
    mutate(caseRate10K = ncases/Pop2019*10000,
           deathRate10K = ndead/Pop2019*10000,
           hospRate10K = nhosp/Pop2019*10000)

FirstCase <- min(OhioCountyOnsetDF$OnsetDate)
LastCase <- max(OhioCountyOnsetDF$OnsetDate)

#used for easy reference in shiny app
varnames <- c("Cases" = "ncases",
              "Deaths" = "ndead",
              "Hospitalizations" = "nhosp",
              "Case Rate" = "caseRate10K",
              "Hospitalization Rate" = "hospRate10K",
              "Death Rate" = "deathRate10K")

#used for easy reference in shiny app for proporational plot
varnames.2 <- c("Cases" = "propcases",
              "Deaths" = "propdead",
              "Hospitalizations" = "prophosp")


#get map information 
county_map <- map_data("county")
oc_map <- subset(county_map, county_map$region == "ohio")

ohio_df_map<-OhioCountyDF
ohio_df_map$County<-stringr::str_to_lower(ohio_df_map$County)

#merge the data frames
map_data<- merge(oc_map, ohio_df_map, 
                 by.x = "subregion",
                 by.y = "County")

#age dataframe
ohio_county_onset_age_df <- OhioDF %>%
    group_by(County, AgeFactor, OnsetDate) %>% #added sex here, remove if an issue for tab 4
    summarize(ncases = sum(`Case Count`),
              nhosp = sum(`Hospitalized Count`,
                          na.rm=TRUE),
              ndead = sum(`Death Due to Illness Count`,
                          na.rm=TRUE))

#create segments of cases to separate the bar graph by color
ohio_county_onset_age_df <- ohio_county_onset_age_df  %>% 
    mutate(CaseCat = cut(ncases,
                         breaks = c(0, 1000, 2500, 5000, 
                                    15000, 30000, 75000 )))
OhioAgeDF<- ohio_county_onset_age_df

#remove unknown ages
OhioAgeDF<- OhioAgeDF %>% 
    filter(AgeFactor != "Unknown")

#obtain rates for visualizations
OhioAgeDF <- merge(OhioAgeDF, OhioPop, by="County")
OhioAgeDF<- OhioAgeDF %>% 
    mutate(caseRate10K = ncases/Pop2019*10000,
           deathRate10K = ndead/Pop2019*10000,
           hospRate10K = nhosp/Pop2019*10000)

#percentage calculations for proporational visualization
OhioNewAgeDF <- OhioAgeDF %>%
    group_by(OnsetDate, AgeFactor) %>%
    summarise(n = sum(ncases),
              m = sum(nhosp),
              p = sum(ndead)) %>%
    mutate(propcases = n / sum(n),
           prophosp = m / sum(m),
           propdead = p / sum(p))




# Define UI for application
ui <- fluidPage(
    # theme 
    theme = shinytheme("spacelab"), 
    
    # Application title
    titlePanel("COVID-19 Impact in Ohio"),
    
    # UI with tabs
    tabsetPanel(
        #tab 1
        tabPanel("Ohio Map",
                 plotlyOutput("Map")),
        #tab 2
        tabPanel("County Response",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "yvar",
                             label = "Select Response: ",
                             choices = varnames,
                             selected = "cases"
                         ),
                         selectInput(
                             inputId = "county",
                             label = "Select counties to highlight: ",
                             choices = unique(OhioCountyOnsetDF$County),
                             selected = "Butler"
                         ), 
                         sliderInput("MAdays",
                                     "Days Averaged:",
                                     min = 2, 
                                     max = 30, 
                                     value = 7
                         ),
                         dateRangeInput("daterange", 
                                        "Date Range:",
                                        start = FirstCase,
                                        end = TODAY)
                     ),
                     mainPanel(plotOutput("movingAverage"), height="100%") #cite this
                 )),
        
        #tab 3
        tabPanel("County Comparison",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "yvar.2",
                             label = "Select Response: ", 
                             choices = varnames, 
                             selected = "cases"
                         )
                     ),
                     mainPanel(plotOutput("CountyComparisons"))
                 )),
        
        #tab 4
        tabPanel("Age Comparison",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "yvar.3",
                             label = "Select Response: ",
                             choices = varnames,
                             selected = "cases"
                         ),
                         selectInput(
                             inputId = "county.2",
                             label = "Select Counties to Highlight: ",
                             choices = unique(OhioAgeDF$County),
                             selected = "Butler"
                         )
                     ),
                     mainPanel(plotOutput("AgeComparisons"))
                 )),
        
        #tab 5
        tabPanel("Proportional Impact by Age", 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             inputId = "yvar.4",
                             label = "Select Response: ", 
                             choices = varnames.2, 
                             selected = "cases"
                         ),
                         dateRangeInput("daterange.2", 
                                        "Date Range:",
                                        start = FirstCase,
                                        end = TODAY)
                     ),
                     mainPanel(plotOutput("StackedArea"))
                    )),
        
        #tab 6
        tabPanel("Acknowledgements & References",
                 tags$div(
                     tags$br(),
                     tags$h3("Author"),
                     tags$h4("Darek Davis"),
                     tags$br(),
                     tags$h3("Date of Construction"),
                     tags$h4("2020-12-03"), #same format as data in onset date
                     tags$br(),
                     tags$h3("Sources"),
                     tags$h4("Data"),
                     tags$a("Ohio Department of Health COVID-19 Data",
                            href="https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv"),
                     tags$br(),
                     tags$a("Census Bureau Population Data",
                            href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"),
                     tags$br(),tags$br(),
                     tags$h4("R Version 4.0.2"),
                     tags$p("R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/."),  
                     tags$br(),tags$br(),
                     tags$h4("R Packages"),
                     tags$p("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016"),  
                     tags$br(),
                     tags$p("Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL http://www.jstatsoft.org/v40/i03/"),
                     tags$br(),
                     tags$p("Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686 "),
                     tags$br(),
                     tags$p("Thomas Lin Pedersen (2020). patchwork: The Composer of Plots. R package version 1.0.1. https://CRAN.R-project.org/package=patchwork"),
                     tags$br(),
                     tags$p("Original S code by Richard A. Becker, Allan R. Wilks. R version by Ray Brownrigg. Enhancements by Thomas P Minka and Alex Deckmyn. (2018). maps: Draw Geographical Maps. R package version 3.3.0. https://CRAN.R-project.org/package=maps"),
                     tags$br(),
                     tags$p("D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf"),
                     tags$br(),
                     tags$p("Doug McIlroy. Packaged for R by Ray Brownrigg, Thomas P Minka and transition to Plan 9 codebase by Roger Bivand.(2020). mapproj: Map Projections. R package version 1.2.7. https://CRAN.R-project.org/package=mapproj"),
                     tags$br(),
                     tags$p("Jeffrey B. Arnold (2019). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.0. https://CRAN.R-project.org/package=ggthemes"),
                     tags$br(),
                     tags$p("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny"),
                     tags$br(),
                     tags$p("Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2. https://CRAN.R-project.org/package=shinythemes"),
                     tags$br(),
                     tags$p("C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020."),
                     tags$br(),
                     tags$p("Matt Dancho and Davis Vaughan (2020). tidyquant: Tidy Quantitative Financial Analysis. R package version 1.0.1. https://CRAN.R-project.org/package=tidyquant"),
                 )
        )
    )
)


# Define server logic required to display visuals
server <- function(input, output) {
    
    #make county selection reactive
    CountyOnsetDF <- reactive({
        OhioCountyOnsetDF %>%
            filter(County %in% c(input$county))
    })  
    
    #make age selection reactive
    OhioCountyAgeDF <- reactive({
        OhioAgeDF %>% 
            filter(County %in% c(input$county.2))
    })
    
    #create plot for tab 1
    output$Map <- renderPlotly({
        print(ggplotly(
            ggplot(map_data, aes(x=long, y=lat, group=group, fill=CaseCat,
                                 text=paste(toupper(subregion),
                                            "\n Cases:", round(ncases,0), #round to zero to increase readability and comprehension
                                            "\n Hospitalizations:", round(nhosp,0),
                                            "\n Deaths:", round(ndead,0),
                                            "\n Case Rate:", round(caseRate10K,0),
                                            "\n Hospitalization Rate:", round(hospRate10K,0),
                                            "\n Death Rate:", round(deathRate10K,0)))) +
                labs(title= paste("COVID-19 Impact by County")) +
                geom_polygon(colour="black") +
                coord_map() +
                scale_fill_brewer() +
                theme_nothing()+
                theme(plot.title = element_text(face = "bold", size = 18)),
            tooltip = c("text")
        )
        )
    })
    
    #create plot for tab 2
    output$movingAverage <- renderPlot({
        ggplot() +
            labs(x = "Onset Date", y= "Total", 
                 title = paste(names(varnames)[varnames == input$yvar],
                               "-", input$MAdays, 
                               "Day Moving Average"),
                 subtitle = paste("Updated: ", TODAY),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")
            ) +
            geom_col(
                data = CountyOnsetDF(),
                aes_string(
                    x = "OnsetDate",
                    y = input$yvar
                ),
                fill = "cornflowerblue",
                alpha = 0.5,
                position = "dodge"
            ) +
        geom_ma(data = CountyOnsetDF(),
            aes_string(x="OnsetDate", y=input$yvar),
            color = "#0072B2",
            n=input$MAdays,
            linetype=1,
            size=1.25
        ) +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%b %d",
                     limits=input$daterange) +
        theme_minimal() +
        theme(
            axis.line.x = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(face = "bold", size = 18),
            plot.subtitle = element_text(face = "bold", color = "darkred", size = 18)
            )
    })
    
    #create plot for tab 3
    output$CountyComparisons <- renderPlot({
        ggplot(data=OhioCountyDF,
               aes_string(y="reorder(County, ncases)",
                          x=input$yvar.2, 
                          fill="CaseCat")) +
            labs(title=paste("Count of ",names(varnames)[varnames == input$yvar.2]," by County"),
                 subtitle = paste("Updated: ", TODAY),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv"))+
            geom_col() +
            scale_fill_brewer()+
            theme_classic()+
            theme(
                axis.title = element_blank(),
                axis.text.x = element_blank(), 
                axis.ticks = element_blank(), 
                axis.line = element_blank(),
                plot.title = element_text(face = "bold", size = 18),
                plot.subtitle = element_text(face = "bold", color = "darkred", size = 18)
            ) +
            geom_text(aes_string(label=(input$yvar.2)), hjust=-0.2, vjust=0.1, size=4) +   
            scale_x_continuous()+
            guides(fill=FALSE)
    }, height= 1200)
    
    #create plot for tab 4
    output$AgeComparisons<- renderPlot({
        ggplot() +
            geom_col(data = OhioCountyAgeDF(),
                     aes_string(x="OnsetDate", y=input$yvar.3, group="AgeFactor"),
                     fill="cornflowerblue")+
            labs(title=paste("Count of ",names(varnames)[varnames == input$yvar.3]," by Age Factor"),
                 subtitle = paste("Updated: ", TODAY),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")
            ) +
            scale_x_date(date_breaks="1 month", date_labels="%b %d") +
            theme_minimal()+
            facet_wrap("~AgeFactor")+
            labs(x="Onset Date", y=paste(names(varnames)[varnames == input$yvar3])) +
            theme(
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                strip.text = element_text(face="bold",color = "darkred"),
                plot.title = element_text(face = "bold", size = 18),
                plot.subtitle = element_text(face = "bold", color = "darkred", size = 18)
            )
    })
    
    #create plot for tab 5
    output$StackedArea<- renderPlot({
        
        ggplot(OhioNewAgeDF, aes_string(x="OnsetDate", y=input$yvar.4, fill="AgeFactor")) + 
            geom_area(colour="black")+
            labs(x="Onset Date", y=" Percentage", 
                 title = paste("Proportional Stacked Area Graph by Age Factor"),
                 subtitle = paste("Updated: ", TODAY),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")) +
            scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9","#009E73","#F0E442","#CC79A7","#D55E00","#0072B2"))+
            scale_x_date(date_breaks = "1 month", date_labels="%b %d", limits = input$daterange.2) +
            theme_classic() +
            theme(
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks = element_blank(),
                legend.title = element_blank(),
                plot.title = element_text(face = "bold", size = 18),
                plot.subtitle = element_text(face = "bold", color = "darkred", size = 18)
            )
    })
    
    #output for tab 6 is just text
    output$ack <- renderText({
        paste("Text")
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
