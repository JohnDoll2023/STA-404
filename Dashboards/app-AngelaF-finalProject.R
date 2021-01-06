# Project: STA 404/504B Dynamic Data Display (Final project)
# Program Name: finalProject.R
# Directory: C:\Users\Angela Famera\Desktop\Fall 2020\STA404\STA404\FinalProject.R
# Programmar: Angela Famera

# Load Libraries
library(shiny)
library(shinythemes)
library(maps)
library(ggplot2)
library(ggmap)
library(mapproj)
library(ggthemes)
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(plotly)
library(tidyquant)


# Create variable to read today's date
Today <- Sys.Date()

# Import population data from the census bureau 
OhioCountyPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
# Filter population data so that only Ohio is selected and all counties from other states are removed
# Remove the " County" at the end of each county variable to make merging of datasets easier
# Keep only the 2019 population estimates
OhioPop <- OhioCountyPop %>%
  filter(STNAME == "Ohio") %>%
  filter(CTYNAME != "Ohio") %>%
  mutate(County = str_remove_all(CTYNAME," County"),
         Pop2019 = POPESTIMATE2019) %>%
  select(County, Pop2019)

# Import covid data from the ohio department of health
ohioDF <-
  read_csv(file = "https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")

# Remove Totals column
# Mutate OnsetDate to get MDY format
# Factor Age Range
ohioDF <- ohioDF %>%
  filter(Sex != "Total") %>%
  mutate(OnsetDate = mdy(`Onset Date`),
         AgeFactor = factor(`Age Range`))


# *** Data Frame 1: Choropleth Map of Ohio & Comparing Counties ***

# Group by County
# Summarize for number of cases, hospitalizations, and deaths
OhioCountyDF <- ohioDF %>%
  group_by(County) %>%
  summarize(
    ncases = sum(`Case Count`),
    ndead = sum(`Death Due to Illness Count`,
                na.rm = TRUE),
    nhosp = sum(`Hospitalized Count`,
                na.rm = TRUE)
  )

# Join OhioCountyDF and OhioPop to get one dataset
OhioCountyDF <- merge(OhioCountyDF, OhioPop, by = "County")


# Add case rate per population of 10000
OhioCountyDF <- OhioCountyDF %>%
  mutate(
    caseRate10K = round((ncases / Pop2019 * 10000), 2),
    deathRate10K = round((ndead / Pop2019 * 10000),2),
    hospRate10K = round((nhosp / Pop2019 * 10000),2)
  )

# According to the recording from Wed, Nov 18th, the "select count or rates" should
# be reasonable case breaks (i.e. round and natural) for number of cases.
OhioCountyDF <- OhioCountyDF %>%
  mutate(CaseCat = cut(ncases,
                       breaks = c(0, 1000, 2500, 5000,
                                  15000, 30000, 60000)))

# *** Data Frame 2: DF 1 Continued...but ONLY for Choropleth Map of Ohio ***

# Import data from library(maps)
countiesMap <- map_data("county")

# Select only counties in Ohio
ohio_countiesMap <- subset(countiesMap, region == "ohio")

# Convert county names to lower case to join the data frames easily
OhioMapDF <- OhioCountyDF %>%
  mutate(county = tolower(County)) %>%
  select(-County)

# Merge the COVID data with the Ohio County Map Data
joinedData <- merge(ohio_countiesMap,
                    OhioMapDF,
                    by.x = "subregion",
                    by.y = "county")

# *** Data Frame 3: Response Over Time ***

# Group by County and OnsetDate
# Summarize for number of cases, hospitalizations, and deaths
OhioResponseDF <- ohioDF %>%
  group_by(County, OnsetDate) %>%
  summarize(
    ncases = sum(`Case Count`),
    ndead = sum(`Death Due to Illness Count`,
                na.rm = TRUE),
    nhosp = sum(`Hospitalized Count`,
                na.rm = TRUE)
  )

# Join OhioResponseDF and OhioPop to get one dataset
OhioResponseDF <- merge(OhioResponseDF, OhioPop, by = "County")

# Add case rate per population of 10000
OhioResponseDF <- OhioResponseDF %>%
  mutate(
    caseRate10K = ncases / Pop2019 * 10000,
    deathRate10K = ndead / Pop2019 * 10000,
    hospRate10K = nhosp / Pop2019 * 10000
  )

# Create a variable for the date of the first recorded case
FirstCase <- min(OhioResponseDF$OnsetDate)
# Create a variable for the date of the last recorded case
LastCase <- max(OhioResponseDF$OnsetDate)


# *** Data Frame 4: Age Comparisons ***

# Group by County, OnsetDate, and Age
# Summarize for number of cases, hospitalizations, and deaths
OhioAgeDF <- ohioDF %>%
  group_by(AgeFactor, County, OnsetDate) %>%
  summarize(
    ncases = sum(`Case Count`),
    ndead = sum(`Death Due to Illness Count`,
                na.rm = TRUE),
    nhosp = sum(`Hospitalized Count`,
                na.rm = TRUE)
  )

# Join OhioAgeDF and OhioPop to get one dataset
OhioAgeDF <- merge(OhioAgeDF, OhioPop, by = "County")

# Add case rate per population of 10000
OhioAgeDF <- OhioAgeDF %>%
  mutate(
    caseRate10K = round((ncases / Pop2019 * 10000), 2),
    deathRate10K = round((ndead / Pop2019 * 10000),2),
    hospRate10K = round((nhosp / Pop2019 * 10000),2)
  )

# Remove unknown ages
OhioAgeDF <- OhioAgeDF %>%
  filter(AgeFactor != "Unknown")

# *** Data Frame 5: Stacked Area Graph ***

OhioStackedDF <- OhioAgeDF %>%
  group_by(OnsetDate, AgeFactor) %>%
  summarise(n = sum(ncases)) %>%
  mutate(percentage = n / sum(n))

# Create a variable for the date of the first recorded case
FirstCase2 <- min(OhioStackedDF$OnsetDate)
# Create a variable for the date of the last recorded case
LastCase2 <- max(OhioStackedDF$OnsetDate)

# 11nov20: correspondence vector between ui variable names and server variable names
varnames <- c("Cases" = "ncases",
              "Deaths" = "ndead",
              "Hospitalizations" = "nhosp",
              "Case Rate" = "caseRate10K",
              "Hospitalization Rate" = "hospRate10K",
              "Death Rate" = "deathRate10K")


# Define UI for application
ui <- fluidPage(
  # Choose theme
  theme = shinytheme("readable"),
  # Application title
  titlePanel(title = "Ohio COVID-19 Shiny App"),
  
  # Create seperate tabs for individual displays
  tabsetPanel(
    
    tabPanel("Map of Ohio",
             plotlyOutput("finalMap")),
    
    tabPanel("Response Over Time",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "yvar",
                   label = "Select a Response to Explore: ",
                   choices = varnames,
                   selected = "cases"
                 ),
                 selectInput(
                   inputId = "county",
                   label = "Select a County to Highlight: ",
                   choices = unique(OhioResponseDF$County),
                   selected = "Butler"
                 ),
                 sliderInput(
                   "MAdays",
                   "Days averaged:",
                   min = 2,
                   max = 30,
                   value = 7
                 ),
                 dateRangeInput("daterange",
                                "Date range:",
                                start = FirstCase,
                                end   = Today)
               ),
               mainPanel(plotOutput("response"), height="100%")
             )),
    
    tabPanel("Comparing Counties", 
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "yvar2",
                   label = "Select a Response to Explore: ",
                   choices = varnames,
                   selected = "cases"
                 )
               ),
               mainPanel(plotOutput("compare"))
             )),
    
    tabPanel("Age Comparisons",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "yvar3",
                   label = "Select a Response to Explore: ",
                   choices = varnames,
                   selected = "cases"
                 ),
                 selectInput(
                   inputId = "county2",
                   label = "Select a County to Highlight: ",
                   choices = unique(OhioAgeDF$County),
                   selected = "Butler"
                 )
               ),
             mainPanel(plotOutput("age"))
             )),
    
    tabPanel("Stacked Area Graph for Ages", 
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("daterange2",
                                "Date Range:",
                                start = FirstCase2,
                                end   = Today)),
             mainPanel(plotOutput("new"))
             )),
    
    tabPanel("Acknowledgements & References", 
             tags$div(
               tags$br(),
               tags$h4("Author"),
               tags$h6("Angela G. Famera"),
               tags$br(),
               tags$h4("Date of Construction"),
               tags$h6("2020-12-03"),
               tags$br(),
               tags$h4("Sources"),
               tags$h6("Data"),
               tags$ul(
                 tags$li("The Ohio COVID-19 data was obtained from a ", 
                         tags$a("CSV data set",
                                href = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv"), "downloaded from the ",
                         tags$a("Ohio Department of Health COVID-19 Dashboard",
                                href = "https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards")),
                 tags$li("The Popuation Data was obtained from a ",
                         tags$a("CSV data set", href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"),
                         "downloaded from the Census Bureau")
               ),
               tags$h6("R Libraries"),
               tags$ul(
                 tags$li("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework
  for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny"),
                 tags$li(" Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2.
  https://CRAN.R-project.org/package=shinythemes"),
                 tags$li("Original S code by Richard A. Becker, Allan R. Wilks. R version by Ray Brownrigg.
Enhancements by Thomas P Minka and Alex Deckmyn. (2018). maps: Draw
Geographical Maps. R package version 3.3.0. https://CRAN.Rproject.org/package=maps"),
                 tags$li("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New
York, 2016."),
                 tags$li("D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal,
5(1), 144-161. URL http://journal.r-project.org/archive/2013-1/kahlewickham.pdf"),
                 tags$li("Doug McIlroy. Packaged for R by Ray Brownrigg, Thomas P Minka and transition to
Plan 9 codebase by Roger Bivand. (2020). mapproj: Map Projections. R package
version 1.2.7. https://CRAN.R-project.org/package=mapproj"),
                 tags$li("Jeffrey B. Arnold (2019). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R
package version 4.2.0. https://CRAN.R-project.org/package=ggthemes"),
                 tags$li("Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software,
4(43), 1686, https://doi.org/10.21105/joss.01686"),
                 tags$li("Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with
lubridate. Journal of Statistical Software, 40(3), 1-25. URL
http://www.jstatsoft.org/v40/i03/"),
                 tags$li("Hadley Wickham and Dana Seidel (2020). scales: Scale Functions for Visualization.
R package version 1.1.1. https://CRAN.R-project.org/package=scales"),
                 tags$li("Thomas Lin Pedersen (2020). patchwork: The Composer of Plots. R package version
1.0.1. https://CRAN.R-project.org/package=patchwork"),
                 tags$li("C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida,
  2020."),
                 tags$li(" Matt Dancho and Davis Vaughan (2020). tidyquant: Tidy Quantitative Financial Analysis. R package version 1.0.1.
  https://CRAN.R-project.org/package=tidyquant")
               ),
               tags$h6("Outside Sources"),
               tags$ul(
                 tags$li(tags$a("Basic Stacked area chart with R", 
                                href="https://www.r-graph-gallery.com/136-stacked-area-chart.html")),
                 tags$li(tags$a("Citing R packages in your Thesis/Paper/Assignments",
                                href="https://www.blopig.com/blog/2013/07/citing-r-packages-in-your-thesispaperassignments/#:~:text=citation()%20To%20cite%20R,R%2Dproject.org%2F.")),
                 tags$li(tags$a("Convert ggplot object in shiny application", 
                                href="https://stackoverflow.com/questions/37663854/convert-ggplot-object-to-plotly-in-shiny-application")),
                 tags$li(tags$a("GGPLOT TITLE, SUBTITLE AND CAPTION", 
                                href="https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/#:~:text=Change%20the%20font%20appearance%20(text,%E2%80%9Cbold%E2%80%9D%20and%20%E2%80%9Cbold.")),
                 tags$li(tags$a("Scale and size of plot in RStudio shiny", 
                                href="https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny")),
                 tags$li(tags$a("Shiny HTML Tage Glossary", 
                                href="https://shiny.rstudio.com/articles/tag-glossary.html")),
                 tags$li(tags$a("Shiny Themes", href="https://rstudio.github.io/shinythemes/")),
               )
             ))
    ))
  
  # Define server  
  server <- function(input, output) {
    
    # Make county selection reaction for Response Over Time
    County_DF <- reactive({
      OhioResponseDF %>%
        filter(County %in% c(input$county))
    })  
    
    # Make county selection reaction for Age Comparisons
    County_DF2 <- reactive({
      OhioAgeDF %>%
        filter(County %in% c(input$county2))
    })
    
    # Display interactive Choropleth Map of Ohio using Plotly
    output$finalMap <- renderPlotly({
      print(ggplotly(
        # text=paste() allows you to customize the tool tip
        ggplot(joinedData, aes(x=long,y=lat, group=group, fill=CaseCat,
                               text=paste(toupper(subregion),
                                          "\n Case Count: ", round(ncases, 2),
                                          "\n Hospitalization Count: ", round(nhosp, 2),
                                          "\n Death Count: ", round(ndead, 2),
                                          "\nCase Rate: ", round(caseRate10K, 2), 
                                          "\nHospitalization Rate: ", round(hospRate10K, 2),
                                          "\nDeath Rate: ", round(deathRate10K, 2)))) +
          labs(title = paste("Interactive Map of Ohio")) +
          geom_polygon(colour="black") +
          coord_map("polyconic")+
          # I chose to incorporate tans/blues because they most closely represent
          # the color scheme of the map on the Tableau Dashboard being mimicked
          scale_fill_manual(values = c("moccasin", 
                                       "wheat3", 
                                       "lightblue3", 
                                       "skyblue3", 
                                       "cornflowerblue", 
                                       "dodgerblue4")) +
          labs(fill="Rate of COVID-19 Cases\nPer 1000 Residents") +
          theme_nothing()+
          theme(
            plot.title = element_text(face = "bold", size = 20) 
          ),
        tooltip = c("text")
      ))
    })
    
    # Display bar chart of Response Over Time
    output$response <- renderPlot({
      ggplot() +
        labs(
          x = "Onset Date",
          y = "Number",
          title = paste(names(varnames)[varnames == input$yvar],
                        " - ", input$MAdays,
                        "Day Moving Average"),
          subtitle = paste("Updated: ",Today),
          caption = paste(
            "Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv"
          )
        ) +
        geom_col(
          data = County_DF(),
          aes_string(
            x = "OnsetDate",
            y = input$yvar
          ),
          fill = "cornflowerblue",
          alpha = 0.5,
          position = "dodge"
        ) +
        geom_ma(
          data = County_DF(),
          aes_string(
            x = "OnsetDate",
            y = input$yvar
          ),
          color = "cornflowerblue",
          n = input$MAdays,
          linetype = 1,
          size = 1.25
        ) +
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%b %d",
          limits = input$daterange
        ) +
        theme_classic() +
        theme(
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 20),
          plot.subtitle = element_text(face = "bold", colour = "firebrick4", size = 15)
        )
    })
    
    # Display horizontal bar graph Comparing Counties
    output$compare <- renderPlot({
      ggplot(data = OhioCountyDF,
             aes_string(y="fct_reorder(County, ncases)",
                        x=input$yvar2,
                        fill="CaseCat")) +
        labs(title = paste("County Comparisons by", names(varnames)[varnames == input$yvar2]))+
        geom_col() +
        # I chose the same color scheme as the map for to be consistent
        scale_fill_manual(values = c("moccasin", 
                                     "wheat3", 
                                     "lightblue3", 
                                     "skyblue3", 
                                     "cornflowerblue", 
                                     "dodgerblue4")) +
        theme_classic()+
        theme(
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(face = "bold", size = 20)
        ) +
        geom_text(aes_string(label=input$yvar2), hjust = -0.3, size=4) +
        # While it is not a perfect solution, scale_x_continuous allows you to see
        # all the text if the window is full screen. Shrinking the window, however,
        # does cut the highest county's text off slightly.
        scale_x_continuous()+
        guides(fill=FALSE)
    }, 
    # Height set to lengthen the display and make is more readable
    height = 1000)
    
    # Display bar chart of faceted Age Comparisons
    output$age <- renderPlot({
      ggplot() +
        labs(title = paste("Distribution of", names(varnames)[varnames == input$yvar3], "by Age Factor"))+
        geom_col(data=County_DF2(), 
                 aes_string(
                   x="OnsetDate", 
                   y=input$yvar3, 
                   group="AgeFactor"), 
                 fill="cornflowerblue")+
        scale_x_date(date_breaks = "1 month",date_labels = "%b %d") +
        theme_classic() +
        # Homework 8 suggestion: "...diff rows better for comparing distributions)"
        # Kept a light gray line to better differentiate between the months throughout the rows
        facet_grid("AgeFactor ~ .") +
        labs(x="Onset Date", y=paste(names(varnames)[varnames == input$yvar3])) +
        theme(
          axis.line.x = element_blank(),
          strip.background = element_rect(colour ="wheat3", fill = "moccasin"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", colour = "firebrick4"),
          panel.grid.major.x = element_line(color = "gray95"),
          plot.title = element_text(face = "bold", size = 20)
        )
    }, 
    # Height set to lengthen the display and make is more readable
    height=700)
    
    output$new <- renderPlot({
      ggplot(OhioStackedDF, aes(x=OnsetDate, y=percentage, fill=AgeFactor)) + 
        geom_area(colour="black")+
        labs(x="Onset Date", y="Case Count Percentage", title = "Proportional Stacked Area Graph for Ages Over Time",
             subtitle = paste("Percentage Based on Number of Cases"))+
        # Tried to keep a similar color scheme used in other tabs. Had to incorporate 
        # a new color or two to make the age groups easily discernable. 
        scale_fill_manual(values = c("firebrick4", 
                                     "cornflowerblue", 
                                     "moccasin",
                                     "black",
                                     "dodgerblue4",
                                     "aliceblue",
                                     "palevioletred",
                                     "lightblue3"))+
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%b %d",
          limits = input$daterange2
        ) +
        theme_classic() +
        theme(
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(face = "bold", size = 20),
          plot.subtitle = element_text(face = "bold", colour = "firebrick4", size = 15),
          legend.title = element_blank()
        )
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  