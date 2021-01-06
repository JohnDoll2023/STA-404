# STA 504 Dynamic Dashboard Final Project
# Austin Chamroontaneskul
# December 4, 2020
################################
# load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)

# load in and clean COVID dataset
ohioCovidDashboard <- "https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv"
OhioDF <- read_csv(file= ohioCovidDashboard) %>% 
  filter(Sex != "Total") %>%
  mutate(AgeFactor = factor(`Age Range`),
         OnsetDate = mdy(`Onset Date`))

# load in and clean population dataset
OhioCountyPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
OhioPop <- OhioCountyPop %>%
  filter(STNAME == "Ohio") %>%
  filter(CTYNAME != "Ohio") %>%
  mutate(County = str_remove_all(CTYNAME," County"),
         Pop2019 = POPESTIMATE2019) %>%
  select(County, Pop2019)

# data frame with counts by County and OnsetDate
OhioCountyDF <- OhioDF %>%
  group_by(County) %>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))

###################### Tab 1 - choropleth map ############################
# add county population to DF and construct rates
OhioCountyDF <- merge(OhioCountyDF, OhioPop, by="County")
OhioCountyDF <- OhioCountyDF %>% 
  mutate(caseRate10K = round(ncases/Pop2019*10000,2),
         deathRate10K = round(ndead/Pop2019*10000,2),
         hospRate10K = round(nhosp/Pop2019*10000,2),
         CountCatC = as.character(cut(ncases,
                                      breaks = c(0, 1000, 2000, 
                                                 5000, signif(max(ncases) + 10000,1)),
                                      dig.lab = 10)))

# manipulate case count ranges for proper display in map legend later
# to avoid complications with levels option in scale_fill_brewer
OhioCountyDF$CountCatC <- factor(case_when(OhioCountyDF$CountCatC == "(0,1000]" ~ "0-1000",
                                           OhioCountyDF$CountCatC == "(1000,2000]" ~ "1001-2000",
                                           OhioCountyDF$CountCatC == "(2000,5000]" ~ "2001-5000",
                                           OhioCountyDF$CountCatC == gsub(" ","",paste("(5000,",signif(max(OhioCountyDF$ncases) + 10000,1),"]")) ~ ">5000",
                                           TRUE ~ OhioCountyDF$CountCatC), levels = c("0-1000","1001-2000","2001-5000",">5000"))

# load ohio county map data
counties = map_data("county")
OhioCounties = subset(counties, region == "ohio")

# data frame with counties formatted for proper grammar
OhioCounties <- OhioCounties %>% 
  mutate(County=str_to_title(subregion)) %>% 
  select(-c(subregion))

# data frame merged by county names
MapData <- merge(OhioCountyDF,OhioCounties,
                 by="County")

###################### Tab 2 - bar graphs over time ######################
# data frame with counts over time (OnsetDate)
OhioCountyTimeDF <- OhioDF %>%
  group_by(County, OnsetDate) %>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))

# merge population dataset with COVID dataset
OhioCountyTimeDF <- merge(OhioCountyTimeDF, OhioPop, by="County")
OhioCountyTimeDF <- OhioCountyTimeDF %>% 
  mutate(caseRate10K = ncases/Pop2019*10000,
         deathRate10K = ndead/Pop2019*10000,
         hospRate10K = nhosp/Pop2019*10000)

###################### Tab 3 - bar graphs by county ######################
# we use the OhioCountyDF already built earlier
OhioCountyCountyDF <- OhioCountyDF

###################### Tab 4 - bar graphs by age #########################
# data frame with counts over time (OnsetDate) by age
OhioAgeTimeDF <- OhioDF %>%
  filter(`Age Range` != "Unknown") %>% 
  group_by(County, OnsetDate, `Age Range`) %>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))

# merge population dataset with COVID dataset
OhioAgeTimeDF <- merge(OhioAgeTimeDF, OhioPop, by="County") %>%
  rename(AgeRange = `Age Range`)

###################### Tab 5 - bar graphs by sex #########################
# data frame summarized by sex
OhioSexDF <- OhioDF %>% 
  group_by(OnsetDate, Sex) %>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))

###################### Tab 6 - top counties table ########################
# create new dataframes ordered by most to least ncases
OhioTopCountyDF <- OhioCountyDF[,c(1:4,6:8,5)] %>% 
  arrange(desc(ncases))

# rename variables for table display
OhioTopCountyDF <- OhioTopCountyDF%>% 
  rename(`Case Count` = ncases,
         `Death Count` = ndead,
         `Hospitalization Count` = nhosp,
         `Case Rate` = caseRate10K,
         `Death Rate` = deathRate10K,
         `Hospitalization Rate` = hospRate10K,
         `2019 Population` = Pop2019)

###################### Miscellaneous variables/mappings ###################
# set up color mappings for counties
ColorsChoices <- c("#C0392B","#E74C3C","#8E44AD","#3498DB",
                   "1ABC9C","#95A5A6","#27AE60","#2ECC71",
                   "#F1C40F","#F39C12","#E67E22")
Color <- c("Adams" = "#E74C3C", "Butler" = "#E74C3C", "Crawford" = "#E74C3C", "Franklin" = "#E74C3C", "Hardin" = "#E74C3C",
           "Jefferson" = "#E74C3C", "Madison" = "#E74C3C", "Montgomery" = "#E74C3C", "Pickaway" = "#E74C3C", "Scioto" = "#E74C3C",
           "Van Wert" = "#E74C3C", "Allen" = "#8E44AD", "Carroll" = "#8E44AD", "Cuyahoga" = "#8E44AD", "Fulton" = "#8E44AD", 
           "Harrison" = "#8E44AD", "Knox" = "#8E44AD", "Mahoning" = "#8E44AD", "Morgan" = "#8E44AD", "Pike" = "#8E44AD", "Seneca" = "#8E44AD", 
           "Vinton" = "#8E44AD", "Ashland" = "#3498DB", "Champaign" = "#3498DB", "Darke" = "#3498DB", "Gallia" = "#3498DB", "Henry" = "#3498DB", 
           "Lake" = "#3498DB", "Marion" = "#3498DB", "Morrow" = "#3498DB", "Portage" = "#3498DB", "Shelby" = "#3498DB", "Warren" = "#3498DB", 
           "Ashtabula" = "#1ABC9C", "Clark" = "#1ABC9C", "Defiance" = "#1ABC9C", "Geauga" = "#1ABC9C", "Highland" = "#1ABC9C", "Lawrence" = "#1ABC9C", 
           "Medina" = "#1ABC9C", "Muskingum" = "#1ABC9C", "Preble" = "#1ABC9C", "Stark" = "#1ABC9C", "Washington" = "#1ABC9C", "Athens" = "#95A5A6", 
           "Clermont" = "#95A5A6", "Delaware" = "#95A5A6", "Greene" = "#95A5A6", "Hocking" = "#95A5A6", "Licking" = "#95A5A6", "Meigs" = "#95A5A6", "Noble" = "#95A5A6", 
           "Putnam" = "#95A5A6", "Summit" = "#95A5A6", "Wayne" = "#95A5A6", "Auglaize" = "#2ECC71", "Clinton" = "#2ECC71", "Erie" = "#2ECC71", 
           "Guernsey" = "#2ECC71", "Holmes" = "#2ECC71", "Logan" = "#2ECC71", "Mercer" = "#2ECC71", "Ottawa" = "#2ECC71", "Richland" = "#2ECC71", 
           "Trumbull" = "#2ECC71", "Williams" = "#2ECC71", "Belmont" = "#F39C12", "Columbiana" = "#F39C12", "Fairfield" = "#F39C12", 
           "Hamilton" = "#F39C12", "Huron" = "#F39C12", "Lorain" = "#F39C12", "Miami" = "#F39C12", "Paulding" = "#F39C12", "Ross" = "#F39C12", 
           "Tuscarawas" = "#F39C12", "Wood" = "#F39C12", "Brown" = "#E67E22", "Coshocton" = "#E67E22", "Fayette" = "#E67E22", "Hancock" = "#E67E22", 
           "Jackson" = "#E67E22", "Lucas" = "#E67E22", "Monroe" = "#E67E22", "Perry" = "#E67E22", "Sandusky" = "#E67E22", "Union" = "#E67E22", "Wyandot" = "#E67E22")

# set up color mappings for age ranges
ColorAge <- c("0-19" = "#A9CCE3","20-29" = "#5DADE2","30-39" = "#48C9B0","40-49" = "#17A589",
              "50-59" = "#138D75","60-69" = "#117A65","70-79" = "#196F3D","80+" = "#145A32")

# set up color mappings for sexes
ColorSex <- c("Male" = "#3498DB", "Female" = "#E74C3C")

# vectors of variables for selected response by user input
varnames <- c("Case Counts" = "ncases",
              "Death Counts" = "ndead",
              "Hospitalization Counts" = "nhosp",
              "Case Rates" = "caseRate10K",
              "Death Rates" = "deathRate10K",
              "Hospitalization Rates" = "hospRate10K")

# the following vector is used for the age and sex visualizations
# that do not require/cannot include rates
varnamesAge <- c("Case Counts" = "ncases",
                 "Death Counts" = "ndead",
                 "Hospitalization Counts" = "nhosp")

# set dates for date calculations
TODAY <- Sys.Date()
FirstCase <- min(OhioDF$OnsetDate)
LastCase <- max(OhioDF$OnsetDate)

# define UI for application
ui <- fluidPage(
  
  theme = shinytheme("readable"),
  
  titlePanel("Ohio COVID-19 Dynamic Dashboard"),
  tabsetPanel(
    ###################### Tab 1 - choropleth map ############################
    tabPanel("Ohio Map", 
             plotlyOutput("Map"),
             tags$div(
               tags$tbody(paste("Updated: ",TODAY)),
               tags$br(),
               tags$tbody("* Rates are counts per 10,000 residents by county."),
               tags$br(),
               tags$a("Source: Ohio Department of Health Dashboard Data",
                      href="https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")
             )
    ),
    ###################### Tab 2 - bar graphs over time ######################
    tabPanel("Counts/Rates Over Time",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "yvar",
                             label= "Select response to explore: ",
                             choices = varnames,
                             selected="Case Counts"),
                 selectizeInput(inputId = "countytime",
                                label = "Click in the box to select counties to highlight: ",
                                choices = unique(OhioCountyTimeDF$County),
                                multiple = TRUE,
                                selected = c("Butler","Hamilton","Preble","Cuyahoga","Delaware")),
                 sliderInput("MAdays",
                             "Days averaged:",
                             min = 2,
                             max = 30,
                             value = 7),
                 dateRangeInput("daterange",
                                "Date range:",
                                start = FirstCase,
                                end   = TODAY)),
               mainPanel(
                 plotOutput("BarTime")
               )
             )
    ),
    ###################### Tab 3 - bar graphs by county ######################
    tabPanel("Counts/Rates By County",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "zvar",
                             label= "Select response to explore: ",
                             choices = varnames,
                             selected="Cases"),
                 selectizeInput(inputId = "countycounty",
                                label = "Click in the box to select county to highlight: ",
                                choices = unique(OhioCountyTimeDF$County),
                                multiple = TRUE,
                                selected = c("Butler","Hamilton","Preble","Cuyahoga","Delaware"))
               ),
               mainPanel(
                 plotOutput("BarCounty")
               )
             )
    ),
    ###################### Tab 4 - bar graphs by age #########################
    tabPanel("Counts by Ages",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "avar",
                             label= "Select response to explore: ",
                             choices = varnamesAge,
                             selected="Case Counts"),
                 selectInput(inputId = "countyage",
                             label = "Click in the box to select county to highlight: ",
                             choices = unique(OhioAgeTimeDF$County),
                             selected = "Butler"),
                 selectizeInput(inputId = "agerange",
                                label = "Click in the box to select age ranges to highlight: ",
                                choices = unique(OhioAgeTimeDF$AgeRange),
                                multiple = TRUE,
                                selected = c("0-19","20-29","60-69","70-79")),
                 sliderInput("MAdaysAge",
                             "Days averaged:",
                             min = 2,
                             max = 30,
                             value = 7),
                 dateRangeInput("daterangeAge",
                                "Date range:",
                                start = FirstCase,
                                end   = TODAY)),
               mainPanel(
                 plotOutput("BarAge")
               )
             )
    ),
    ###################### Tab 5 - bar graphs by sex #########################
    tabPanel("Counts by Sex",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "bvar",
                             label= "Select response to explore: ",
                             choices = varnamesAge,
                             selected="Case Counts"),
                 selectizeInput(inputId = "sex",
                                label = "Click in the box to select sexes to highlight: ",
                                choices = unique(OhioSexDF$Sex),
                                multiple = TRUE,
                                selected = c("Male","Female")),
                 sliderInput("MAdaysSex",
                             "Days averaged:",
                             min = 2,
                             max = 30,
                             value = 7),
                 dateRangeInput("daterangeSex",
                                "Date range:",
                                start = FirstCase,
                                end   = TODAY)),
               mainPanel(
                 plotOutput("BarSex")
               )
             )
    ),
    ###################### Tab 6 - top counties table ####################
    tabPanel("Table", 
             tags$tbody("* Rates are counts per 10,000 residents by county."),
             dataTableOutput("table"),
             tags$a("Source: Ohio Department of Health Dashboard Data",
                    href="https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")
    ),
    ###################### Tab 7 - references ################################
    tabPanel("References",
             tags$div(
               tags$p("An Ohio COVID Dashboard by Austin Chamroontaneskul"),
               tags$p("Date of Construction: December 4, 2020"),
               tags$p("The COVID data were obtained from a ",
                      tags$a("CSV data set",
                             href="https://coronavirus.ohio.gov/static/COVIDSummaryData.csv",
                             .noWS = "outside"
                      ),
                      " downloaded from the ",
                      tags$a("Ohio Department of Health COVID-19 Dashboard",
                             href="https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards",
                             .noWS = "outside")),
               tags$p("The population data were obtained from a ",
                      tags$a("CSV data set",
                             href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
                             .noWS = "outside"
                             ),
                      " downloaded from the ",
                      tags$a("United States Census Bureau",
                             href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/",
                             .noWS = "outside")),
               tags$p("Color hexes were selected from ",
                      tags$a("HTMLcolorcodes.com",
                             href="https://htmlcolorcodes.com/",
                             .noWS = "outside")),
               tags$p("R packages utilized:"),
               tags$ul(
                 tags$li("C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and
  Hall/CRC Florida, 2020."),
                 tags$li("Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of
  Statistical Software, 40(3), 1-25. URL https://www.jstatsoft.org/v40/i03/."),
                 
                 tags$li("Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,
                         https://doi.org/10.21105/joss.01686"),
                 tags$li("Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2.
  https://CRAN.R-project.org/package=shinythemes"),
                 tags$li("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web
  Application Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny")
               )
             )
    )
  )
)


# define server logic required to draw output
server <- function(input, output, session) {
  
  # define reactive functions for tabs 2-5
  Time_DF <- reactive({
    OhioCountyTimeDF %>%
      filter(County %in% c(input$countytime))})
  
  County_DF <- reactive({
    OhioCountyCountyDF %>%
      filter(County %in% c(input$countycounty))})
  
  Age_DF <- reactive({
    OhioAgeTimeDF %>%
      filter(County %in% c(input$countyage),
             AgeRange %in% c(input$agerange))})
  
  Sex_DF <- reactive({
    OhioSexDF %>% 
      filter(Sex %in% c(input$sex))})
  
  ###################### Tab 1 - choropleth map ############################
  output$Map <- renderPlotly({
    MapGG <- ggplot(data=MapData, aes(x=long,
                                      y=lat,
                                      group=group,
                                      fill=CountCatC,
                                      County = County,
                                      `Case Count` = ncases,
                                      `Death Count` = ndead,
                                      `Hospitalization Count` = nhosp,
                                      `Case Rate` = caseRate10K,
                                      `Death Rate` = deathRate10K,
                                      `Hospitalization Rate` = hospRate10K)) +
      scale_fill_brewer(palette = "Blues", name = "Case Counts") +
      geom_polygon(colour = "black") +
      coord_map("polyconic") +
      theme_light() +
      theme(axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank()) +
      labs(title = paste("Cumulative Counts/Rates from",FirstCase,"to",LastCase))
    ggplotly(MapGG, tooltip = c("County","Case Count","Death Count","Hospitalization Count",
                                "Case Rate","Death Rate","Hospitalization Rate"))
  })
  
  ###################### Tab 2 - bar graphs over time ######################
  output$BarTime <- renderPlot({
    ggplot() +
      labs(x="Onset Date", y="Number",
           title=paste(names(varnames)[varnames==input$yvar],
                       "by County - ", input$MAdays,
                       "d Moving Average"),
           subtitle=paste("Updated: ",TODAY),
           caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                          "\n","* Rates are counts per 10,000 residents by county.")) +
      geom_col(data=Time_DF(),
               aes_string(x="OnsetDate",
                          y=input$yvar,
                          fill = "County"),
               alpha=0.5,
               position = "dodge") +
      geom_ma(data=Time_DF(),
              aes_string(x = "OnsetDate",
                         y = input$yvar,
                         color = "County"),
              n=input$MAdays, linetype=1, size=1.25) +
      scale_fill_manual("County",values=Color) +
      scale_color_manual("County",values=Color) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %d",
                   limits=input$daterange) +
      theme_minimal()
  })
  
  ###################### Tab 3 - bar graphs by county ######################
  output$BarCounty <- renderPlot({
    ggplot() +
      labs(x="Number", y="County",
           title=paste("Cumulative",names(varnames)[varnames==input$zvar],"from",FirstCase,"to",LastCase),
           subtitle=paste("Updated: ",TODAY),
           caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                          "\n","* Rates are counts per 10,000 residents by county.")) +
      geom_col(data = County_DF(), aes_string(y = paste("reorder(County,",input$zvar,")"), 
                                              x = input$zvar,
                                              fill = "County")) +
      scale_x_continuous(expand=c(0,0)) +
      scale_fill_manual("County",values=Color) +
      theme_light() +
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank())
  })
  
  ###################### Tab 4 - bar graphs by age #########################
  output$BarAge <- renderPlot({
    ggplot() +
      labs(x="Onset Date", y="Number",
           title=paste(names(varnamesAge)[varnamesAge==input$avar],
                       "by Age Range - ", input$MAdaysAge,
                       "d Moving Average"),
           subtitle=paste("Updated: ",TODAY),
           caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")) +
      #           tag = paste(input$county,"County"))
      geom_col(data=Age_DF(),
               aes_string(x="OnsetDate",
                          y=input$avar,
                          fill = "AgeRange"),
               alpha=0.5,
               position = "dodge") +
      geom_ma(data=Age_DF(),
              aes_string(x = "OnsetDate",
                         y = input$avar,
                         color = "AgeRange"),
              n=input$MAdaysAge, linetype=1, size=1.25) +
      scale_fill_manual("AgeRange",values = ColorAge) +
      scale_color_manual("AgeRange",values = ColorAge) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %d",
                   limits=input$daterangeAge) +
      theme_minimal()
  })
  
  ###################### Tab 5 - bar graphs by sex #########################
  output$BarSex <- renderPlot({
    ggplot() +
      labs(x="Onset Date", y="Number",
           title=paste(names(varnamesAge)[varnamesAge==input$bvar],
                       "by Sex - ", input$MAdaysSex,
                       "d Moving Average"),
           subtitle=paste("Updated: ",TODAY),
           caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")) +
      geom_col(data=Sex_DF(),
               aes_string(x="OnsetDate",
                          y=input$bvar,
                          fill = "Sex"),
               alpha=0.5,
               position = "dodge") +
      geom_ma(data=Sex_DF(),
              aes_string(x = "OnsetDate",
                         y = input$bvar,
                         color = "Sex"),
              n=input$MAdaysSex, linetype=1, size=1.25) +
      scale_fill_manual("Sex",values = ColorSex) +
      scale_color_manual("Sex",values = ColorSex) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %d",
                   limits=input$daterangeSex) +
      theme_minimal()
  })
  
  ###################### Tab 6 - top counties table ####################
  output$table <- renderDataTable(OhioTopCountyDF)
}

# run the application 
shinyApp(ui = ui, server = server)