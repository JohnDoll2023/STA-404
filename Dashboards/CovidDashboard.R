# Dynamic Dashboard Server Project
# Austin Chamroontaneskul, et al.
# December 21, 2020
################################
# load necessary packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(tidyquant)

# load in and clean COVID dataset
ohioCovidDashboard <- "https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv"
OhioDF <- read_csv(file= ohioCovidDashboard) %>% 
  filter(Sex != "Total") %>%
  mutate(AgeFactor = factor(`Age Range`),
         OnsetDate = mdy(`Onset Date`),
         DeathDate = mdy(`Date Of Death`),
         HospDate = mdy(`Admission Date`))

#used for total DF, no unknowns removed
OhioTotalDF <- OhioDF %>% 
  group_by(County) %>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`),
            nhosp = sum(`Hospitalized Count`))

# load in and clean population dataset
OhioCountyPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
OhioPop <- OhioCountyPop %>%
  filter(STNAME == "Ohio") %>%
  filter(CTYNAME != "Ohio") %>%
  mutate(County = str_remove_all(CTYNAME," County"),
         Pop2019 = POPESTIMATE2019) %>%
  select(County, Pop2019)

# data frame with counts by County and OnsetDate
OhioCountyCases <- OhioDF %>%
  mutate(Date = OnsetDate) %>%
  group_by(County, Date, Sex) %>%
  summarize(ncases = sum(`Case Count`))

OhioCountyDeaths <- OhioDF %>%
  mutate(Date = DeathDate) %>%
  group_by(County, Date, Sex) %>%
  summarize(ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE))

OhioCountyHosp <- OhioDF %>%
  mutate(Date = HospDate) %>%
  group_by(County, Date, Sex) %>%
  summarize(nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))

# need to left join so all union of all
# case / hosp/ death dates included
OhioCountyDFB <- OhioCountyCases %>%
  left_join(OhioCountyHosp) %>%
  left_join(OhioCountyDeaths) %>%
  mutate(ndead = ifelse(is.na(ndead),0,ndead),
         nhosp = ifelse(is.na(nhosp),0,nhosp),
         ncases = ifelse(is.na(ncases),0,ncases))

OhioCountyDF <- OhioDF %>%
  group_by(County) %>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))


###################### Tab 1 - choropleth map ############################
# add county population to DF and construct rates
OhioCountyDF <- merge(OhioCountyDF, OhioPop, by="County")%>%
  mutate(caseRate10K = round(ncases/Pop2019*10000,0),
         deathRate10K = round(ndead/Pop2019*10000,0),
         hospRate10K = round(nhosp/Pop2019*10000,0),
         CountCatC = as.character(cut(ncases,
                                      breaks = c(0, 1000, 2000, 
                                                 5000, 15000, 30000, signif(max(ncases) + 2000000,1)),
                                      dig.lab = 10)))

#data frame with Cumulative counts and rates
OhioTotalDF <- merge(OhioTotalDF, OhioPop, by="County") %>% 
  summarize(ncases = sum(ncases),
            ndead = sum(ndead),
            nhosp = sum(nhosp),
            caseRate10K = round(sum(ncases)/sum(Pop2019)*10000,0),
            deathRate10K = round(sum(ndead)/sum(Pop2019)*10000,0),
            hospRate10K = round(sum(nhosp)/sum(Pop2019)*10000,0))

# manipulate case count ranges for proper display in map legend later
# to avoid complications with levels option in scale_fill_brewer
OhioCountyDF$CountCatC <- factor(case_when(OhioCountyDF$CountCatC == "(0,1000]" ~ "0-1000",
                                           OhioCountyDF$CountCatC == "(1000,2000]" ~ "1001-2000",
                                           OhioCountyDF$CountCatC == "(2000,5000]" ~ "2001-5000",
                                           OhioCountyDF$CountCatC == "(5000,15000]" ~ "5001-15000",
                                           OhioCountyDF$CountCatC == "(15000,30000]" ~ "15001-30000",
                                           OhioCountyDF$CountCatC == gsub(" ","",paste("(30000,",signif(max(OhioCountyDF$ncases) + 2000000,1),"]")) ~ ">30000",
                                           TRUE ~ OhioCountyDF$CountCatC), levels = c("0-1000","1001-2000","2001-5000","5001-15000","15001-30000",">30000"))

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
# merge population dataset with COVID dataset
OhioCountyTimeDF <- merge(OhioCountyDFB, OhioPop, by="County") %>% 
  mutate(caseRate10K = ncases/Pop2019*10000,
         deathRate10K = ndead/Pop2019*10000,
         hospRate10K = nhosp/Pop2019*10000)

###################### Tab 3 - bar graphs by county ######################
# we use the OhioCountyDF already built earlier

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
# data frame summarized by sex and date
OhioSexDF <- OhioCountyTimeDF %>%
  filter(Sex != "Unknown") %>% 
  group_by(Date, Sex) %>%
  summarize(ncases = sum(ncases),
            ndead = sum(ndead),
            nhosp = sum(nhosp),
            caseRate10K = sum(caseRate10K),
            deathRate10K = sum(deathRate10K),
            hospRate10K = sum(hospRate10K))

# data frame of totals by sex
OhioCountySexCountsDF <- OhioCountyTimeDF %>%
  group_by( Sex) %>%
  summarize(ncases = sum(ncases),
            ndead = sum(ndead),
            nhosp = sum(nhosp),
            caseRate10K = round(sum(caseRate10K), 0),
            deathRate10K = round(sum(deathRate10K), 0),
            hospRate10K = round(sum(hospRate10K), 0 ))

FemaleCountsDF = OhioCountySexCountsDF%>%
  filter(Sex=="Female")

MaleCountsDF = OhioCountySexCountsDF%>%
  filter(Sex=="Male")

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
# set up color mappings for sexes
ColorSex <- c("Male" = "#3498DB", "Female" = "#E74C3C")

# color mapping for counties
ColorCounties <- c("#d77355","#508cd7","#64b964","#e6c86e",
                   "#000000","#beaed4","#f0027f","#dcf5ff")

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
             downloadButton("tab1Download", "Download the map"),
             plotlyOutput("Map"),
             tags$div(
               tags$body("Scroll over a county for further data"),
               tags$br(),
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
                                label = "Type in the Counties you wish to compare: ",
                                choices = unique(OhioCountyTimeDF$County),
                                multiple = TRUE,
                                selected = c("Butler", "Cuyahoga","Franklin","Hamilton"),
                                options = list(maxItems = 8)),
                 sliderInput("MAdays",
                             "Days averaged:",
                             min = 2,
                             max = 30,
                             value = 7),
                 dateRangeInput("daterange",
                                "Date range:",
                                start = "2020-04-01",
                                end   = TODAY),
                 downloadButton("tab2Download", "Download the graph")),
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
                                label = "Type in the Counties you wish to compare: ",
                                choices = unique(OhioCountyTimeDF$County),
                                multiple = TRUE,
                                selected = c("Butler", "Cuyahoga","Franklin","Hamilton"),
                                options = list(maxItems = 8)),
                 downloadButton("tab3Download", "Download the graph")
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
                             label = "Select a County to display: ",
                             choices = unique(OhioAgeTimeDF$County),
                             selected = "Butler"),
                 dateRangeInput("daterangeAge",
                                "Date range:",
                                start = "2020-04-01",
                                end   = TODAY),
                 downloadButton("tab4Download", "Download the graph")),
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
                             choices = varnames,
                             selected="Case Counts"),
                 selectizeInput(inputId = "sex",
                                label = "Click in the box to select sexes to display: ",
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
                                start = "2020-04-01",
                                end   = TODAY),
                 downloadButton("tab5Download", "Download the graph")),
               mainPanel(
                 plotOutput("BarSex")
               )
             )
    ),
    ###################### Tab 6 - top counties table ####################
    tabPanel("Data Table", 
             tags$h5("Click on a column title to reorder based on a chosen category"),
             tags$br(),
             tags$h6("* Rates are counts per 10,000 residents by county."),
             tags$br(),
             downloadButton("tab6Download", "Download the table"),
             dataTableOutput("table"),
             tags$a("Source: Ohio Department of Health Dashboard Data",
                    href="https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")
    ),
    ###################### Tab 7- Angela's reference page ####################
    tabPanel("Acknowledgements & References", 
             tags$div(
               tags$br(),
               tags$h4("Authors"),
               tags$h6("Austin Chamroontaneskul, Darek Davis, John Doll, 
                        Angela Famera, Ashley Lefebvre, Madison McMillen,
                        Deirdre Sperry, Abby Tietjen"), 
               tags$br(),
               tags$h4("Advisor"),
               tags$h6("Dr. A. John Bailer"), 
               tags$br(),
               tags$h4("Date of Construction"),
               tags$h6("2020-12-20"),
               tags$br(),
               tags$h4("Sources"),
               tags$h6("Data"),
               tags$ul(
                 tags$li("The Ohio COVID-19 data was obtained from a ", 
                         tags$a("CSV data set",
                                href = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv"), "downloaded from the ",
                         tags$a("Ohio Department of Health COVID-19 Dashboard",
                                href = "https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards")),
                 tags$li("The Population Data was obtained from a ",
                         tags$a("CSV data set", href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"),
                         "downloaded from the Census Bureau")
               ),
               tags$h6("R Libraries"),
               tags$ul(
                 tags$li("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework
  for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny"),
                 tags$li(" Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2.
  https://CRAN.R-project.org/package=shinythemes"),
                 tags$li("Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software,
4(43), 1686, https://doi.org/10.21105/joss.01686"),
                 tags$li("Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with
lubridate. Journal of Statistical Software, 40(3), 1-25. URL
http://www.jstatsoft.org/v40/i03/"),
                 tags$li("C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida,
  2020."),
                 tags$li(" Matt Dancho and Davis Vaughan (2020). tidyquant: Tidy Quantitative Financial Analysis. R package version 1.0.1.
  https://CRAN.R-project.org/package=tidyquant")
               )
             ))
  )
)


# define server logic required to draw output
server <- function(input, output, session) {
  
  # define reactive functions for tabs 2-5
  Time_DF <- reactive({
    OhioCountyTimeDF %>%
      filter(County %in% c(input$countytime))})
  
  tab2 = reactive({
    OhioCountyDF%>%
      filter(County %in% c(input$countytime))
  })
  
  County_DF <- reactive({
    OhioCountyDF %>%
      filter(County %in% c(input$countycounty))})
  
  Age_DF <- reactive({
    OhioAgeTimeDF %>%
      filter(County %in% c(input$countyage))})
  
  Sex_DF <- reactive({
    OhioSexDF %>% 
      filter(Sex %in% c(input$sex))})
  
  ###################### Tab 1 - choropleth map ############################
  output$Map <- renderPlotly({
    MapGG <- ggplot(data=MapData, aes(x=long,
                                      y=lat,
                                      group=group,
                                      fill=CountCatC,
                                      
                                      text = paste(County, "<br>Cases: ", ncases, "<br>Deaths: ", ndead, "<br>Hospitalizations: ", nhosp, "<br>Case Rate: ", caseRate10K, "<br>Death Rate: ", deathRate10K,"<br>Hospitalization Rate: ", hospRate10K, sep = ""))) +
      scale_fill_brewer(palette = "Blues", name = "Case Counts") +
      geom_polygon(colour = "black", size = .1) +
      coord_map("polyconic") +
      theme_light() +
      theme(axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            plot.title = element_text(face = "bold", size = 16),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank()) +
      labs(title = paste("Interactive Cumulative Counts/Rates* from",FirstCase,"to",LastCase))
    ggplotly(MapGG, tooltip = c("text"))
  })
  
  
  ###################### Tab 1 - download button ######################
  output$tab1Download=downloadHandler(
    filename=function(){"OhioChoropleth.png"},
    content=function(file){
      png(file)
      plot1 = ggplot(data=MapData, aes(x=long,
                                       y=lat,
                                       group=group,
                                       fill=CountCatC,
                                       text = paste(County, "<br>Cases: ", ncases, "<br>Deaths: ", ndead, "<br>Hospitalizations: ", nhosp, "<br>Case Rate: ", caseRate10K, "<br>Death Rate: ", deathRate10K,"<br>Hospitalization Rate: ", hospRate10K, sep = ""))) +
        scale_fill_brewer(palette = "Blues", name = "Case Counts") +
        geom_polygon(colour = "black", size = .1) +
        coord_map("polyconic") +
        theme_light() +
        theme(axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              plot.title = element_text(face = "bold", size = 16),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank()) +
        labs(title = paste("Interactive Cumulative Counts/Rates* from",FirstCase,"to",LastCase))
      print(plot1)
      dev.off()
    },
    contentType = "image/png"
  )
  ###################### Tab 2 - bar graphs over time ######################
  output$BarTime <- renderPlot({
    tempVecTab2=c(tab2()$County)
    if(input$yvar=="caseRate10K" || input$yvar== "deathRate10K" || input$yvar == "hospRate10K"){
      if(length(tempVecTab2)==1){ 
        tempPlot2 =  ggplot() +
          labs(x="Date", y="Rate(per 10,000 Residents)",
               title=paste(names(varnames)[varnames==input$yvar],
                           "* by County - ", input$MAdays,
                           "d Moving Average", sep = ""),
               subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                              switch(input$yvar,
                                     "ncases" = OhioTotalDF$ncases,
                                     "ndead" = OhioTotalDF$ndead,
                                     "nhosp" = OhioTotalDF$nhosp,
                                     "caseRate10K" = OhioTotalDF$caseRate10K,
                                     "deathRate10K" = OhioTotalDF$deathRate10K,
                                     "hospRate10K" = OhioTotalDF$hospRate10K),
                              "\nCumulative ", tab2()$County, " County ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ",
                              switch(input$yvar,
                                     "ncases" = tab2()$ncases,
                                     "ndead" = tab2()$ndead,
                                     "nhosp" = tab2()$nhosp,
                                     "caseRate10K" = tab2()$caseRate10K,
                                     "deathRate10K" = tab2()$deathRate10K,
                                     "hospRate10K" = tab2()$hospRate10K),
                              "\n","Updated: ",TODAY, sep = ""),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
      }else{
        tempPlot2 = ggplot() +
          labs(x="Date", y="Rate(per 10,000 Residents)",
               title=paste(names(varnames)[varnames==input$yvar],
                           "* by County - ", input$MAdays,
                           "d Moving Average", sep = ""),
               subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                              switch(input$yvar,
                                     "ncases" = OhioTotalDF$ncases,
                                     "ndead" = OhioTotalDF$ndead,
                                     "nhosp" = OhioTotalDF$nhosp,
                                     "caseRate10K" = OhioTotalDF$caseRate10K,
                                     "deathRate10K" = OhioTotalDF$deathRate10K,
                                     "hospRate10K" = OhioTotalDF$hospRate10K),
                              "\n","Updated: ",TODAY, sep = ""),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
      }
    }else {
      if(length(tempVecTab2)==1){ 
        tempPlot2 = ggplot() +
          labs(x="Date", y="Count",
               title=paste(names(varnames)[varnames==input$yvar],
                           " by County - ", input$MAdays,
                           "d Moving Average", sep = ""),
               subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                              switch(input$yvar,
                                     "ncases" = OhioTotalDF$ncases,
                                     "ndead" = OhioTotalDF$ndead,
                                     "nhosp" = OhioTotalDF$nhosp,
                                     "caseRate10K" = OhioTotalDF$caseRate10K,
                                     "deathRate10K" = OhioTotalDF$deathRate10K,
                                     "hospRate10K" = OhioTotalDF$hospRate10K),
                              
                              "\nCumulative ", tab2()$County, " County ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ",
                              switch(input$yvar,
                                     "ncases" = tab2()$ncases,
                                     "ndead" = tab2()$ndead,
                                     "nhosp" = tab2()$nhosp,
                                     "caseRate10K" = tab2()$caseRate10K,
                                     "deathRate10K" = tab2()$deathRate10K,
                                     "hospRate10K" = tab2()$hospRate10K),
                              "\n","Updated: ",TODAY, sep = ""),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
      }else{
        tempPlot2 =  ggplot() +
          labs(x="Date", y="Count",
               title=paste(names(varnames)[varnames==input$yvar],
                           " by County - ", input$MAdays,
                           "d Moving Average", sep = ""),
               subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                              switch(input$yvar,
                                     "ncases" = OhioTotalDF$ncases,
                                     "ndead" = OhioTotalDF$ndead,
                                     "nhosp" = OhioTotalDF$nhosp,
                                     "caseRate10K" = OhioTotalDF$caseRate10K,
                                     "deathRate10K" = OhioTotalDF$deathRate10K,
                                     "hospRate10K" = OhioTotalDF$hospRate10K),
                              "\n","Updated: ",TODAY, sep = ""),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
        
      }
    }
    tempPlot2 +
      geom_col(data=Time_DF(),
               aes_string(x="Date",
                          y=input$yvar,
                          fill = "County"),
               alpha=0.5,
               position = "dodge") +
      geom_ma(data=Time_DF(),
              aes_string(x = "Date",
                         y = input$yvar,
                         color = "County"),
              n=input$MAdays, linetype=1, size=1.25) +
      scale_fill_manual("County",values=ColorCounties) +
      scale_color_manual("County",values=ColorCounties) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %d",
                   limits=input$daterange) +
      theme_minimal()+
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "bold", color = "darkred", size = 14)
      )
  })
  
  ###################### Tab 2 - download button ######################
  output$tab2Download=downloadHandler(
    filename=function(){"CountsOverTime.png"},
    content=function(file){
      png(file)
      tempVecTab2=c(tab2()$County)
      if(input$yvar=="caseRate10K" || input$yvar== "deathRate10K" || input$yvar == "hospRate10K"){
        if(length(tempVecTab2)==1){ 
          tempPlot2 =  ggplot() +
            labs(x="Date", y="Rate(per 10,000 Residents)",
                 title=paste(names(varnames)[varnames==input$yvar],
                             "* by County - ", input$MAdays,
                             "d Moving Average", sep = ""),
                 subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                                switch(input$yvar,
                                       "ncases" = OhioTotalDF$ncases,
                                       "ndead" = OhioTotalDF$ndead,
                                       "nhosp" = OhioTotalDF$nhosp,
                                       "caseRate10K" = OhioTotalDF$caseRate10K,
                                       "deathRate10K" = OhioTotalDF$deathRate10K,
                                       "hospRate10K" = OhioTotalDF$hospRate10K),
                                "\nCumulative ", tab2()$County, " County ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ",
                                switch(input$yvar,
                                       "ncases" = tab2()$ncases,
                                       "ndead" = tab2()$ndead,
                                       "nhosp" = tab2()$nhosp,
                                       "caseRate10K" = tab2()$caseRate10K,
                                       "deathRate10K" = tab2()$deathRate10K,
                                       "hospRate10K" = tab2()$hospRate10K),
                                "\n","Updated: ",TODAY, sep = ""),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                                "\n","* Rates are counts per 10,000 residents by county."))
        }else{
          tempPlot2 = ggplot() +
            labs(x="Date", y="Rate(per 10,000 Residents)",
                 title=paste(names(varnames)[varnames==input$yvar],
                             "* by County - ", input$MAdays,
                             "d Moving Average", sep = ""),
                 subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                                switch(input$yvar,
                                       "ncases" = OhioTotalDF$ncases,
                                       "ndead" = OhioTotalDF$ndead,
                                       "nhosp" = OhioTotalDF$nhosp,
                                       "caseRate10K" = OhioTotalDF$caseRate10K,
                                       "deathRate10K" = OhioTotalDF$deathRate10K,
                                       "hospRate10K" = OhioTotalDF$hospRate10K),
                                "\n","Updated: ",TODAY, sep = ""),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                                "\n","* Rates are counts per 10,000 residents by county."))
        }
      }else {
        if(length(tempVecTab2)==1){ 
          tempPlot2 = ggplot() +
            labs(x="Date", y="Count",
                 title=paste(names(varnames)[varnames==input$yvar],
                             " by County - ", input$MAdays,
                             "d Moving Average", sep = ""),
                 subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                                switch(input$yvar,
                                       "ncases" = OhioTotalDF$ncases,
                                       "ndead" = OhioTotalDF$ndead,
                                       "nhosp" = OhioTotalDF$nhosp,
                                       "caseRate10K" = OhioTotalDF$caseRate10K,
                                       "deathRate10K" = OhioTotalDF$deathRate10K,
                                       "hospRate10K" = OhioTotalDF$hospRate10K),
                                
                                "\nCumulative ", tab2()$County, " County ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ",
                                switch(input$yvar,
                                       "ncases" = tab2()$ncases,
                                       "ndead" = tab2()$ndead,
                                       "nhosp" = tab2()$nhosp,
                                       "caseRate10K" = tab2()$caseRate10K,
                                       "deathRate10K" = tab2()$deathRate10K,
                                       "hospRate10K" = tab2()$hospRate10K),
                                "\n","Updated: ",TODAY, sep = ""),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                                "\n","* Rates are counts per 10,000 residents by county."))
        }else{
          tempPlot2 =  ggplot() +
            labs(x="Date", y="Count",
                 title=paste(names(varnames)[varnames==input$yvar],
                             " by County - ", input$MAdays,
                             "d Moving Average", sep = ""),
                 subtitle=paste("Cumulative Ohio ", substring(names(varnames)[varnames==input$yvar], 0, nchar(names(varnames)[varnames==input$yvar]) - 1), ": ", 
                                switch(input$yvar,
                                       "ncases" = OhioTotalDF$ncases,
                                       "ndead" = OhioTotalDF$ndead,
                                       "nhosp" = OhioTotalDF$nhosp,
                                       "caseRate10K" = OhioTotalDF$caseRate10K,
                                       "deathRate10K" = OhioTotalDF$deathRate10K,
                                       "hospRate10K" = OhioTotalDF$hospRate10K),
                                "\n","Updated: ",TODAY, sep = ""),
                 caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                                "\n","* Rates are counts per 10,000 residents by county."))
          
        }
      }
      plot2 = tempPlot2 +
        geom_col(data=Time_DF(),
                 aes_string(x="Date",
                            y=input$yvar,
                            fill = "County"),
                 alpha=0.5,
                 position = "dodge") +
        geom_ma(data=Time_DF(),
                aes_string(x = "Date",
                           y = input$yvar,
                           color = "County"),
                n=input$MAdays, linetype=1, size=1.25) +
        scale_fill_manual("County",values=ColorCounties) +
        scale_color_manual("County",values=ColorCounties) +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%b %d",
                     limits=input$daterange) +
        theme_minimal()+
        theme(
          plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(face = "bold", color = "darkred", size = 14)
        )
      print(plot2)
      dev.off()
    },
    contentType = "image/png"
  )
  
  ###################### Tab 3 - bar graphs by county ######################
  output$BarCounty <- renderPlot({
    if(input$zvar=="caseRate10K" || input$zvar== "deathRate10K" || input$zvar == "hospRate10K"){
      tempPlot3=ggplot() +
        labs(x="Rate(per 10,000 Residents)", y="County",
             title=paste("Cumulative ",names(varnames)[varnames==input$zvar],"* from ",FirstCase," to ",LastCase, sep = ""),
             subtitle=paste("Updated: ",TODAY),
             caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                            "\n","* Rates are counts per 10,000 residents by county."))
    } else {
      tempPlot3=ggplot() +
        labs(x="Count", y="County",
             title=paste("Cumulative ",names(varnames)[varnames==input$zvar]," from ",FirstCase," to ",LastCase, sep = ""),
             subtitle=paste("Updated: ",TODAY),
             caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                            "\n","* Rates are counts per 10,000 residents by county."))
    }
    tempPlot3 +
      geom_col(data = County_DF(), aes_string(y = paste("reorder(County,",input$zvar,")"), 
                                              x = input$zvar,
                                              fill = "County")) + #bar graph
      geom_text(size=4,data=County_DF(),aes_string(x=input$zvar,y=paste("reorder(County,",input$zvar,")"),label=input$zvar,fill=NULL, hjust = -0.1))+
      xlim(0, switch(input$zvar,
                     "ncases" = max(County_DF()$ncases) + 15000,
                     "ndead" = max(County_DF()$ndead) + 100,
                     "nhosp" = max(County_DF()$nhosp) + 200,
                     "caseRate10K" = max(County_DF()$caseRate10K) + 150,
                     "deathRate10K" = max(County_DF()$deathRate10K) + 2,
                     "hospRate10K" = max(County_DF()$hospRate10K) + 10)) + 
      scale_fill_manual("County",values=ColorCounties) +
      theme_light() +
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            axis.text.x =element_blank(),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(face = "bold", size = 16),
            plot.subtitle = element_text(face = "bold", color = "darkred", size = 14)
      )
  })
  
  ###################### Tab 3 - download button ######################
  output$tab3Download=downloadHandler(
    filename=function(){"CountsByCounty.png"},
    content=function(file){
      png(file)
      if(input$zvar=="caseRate10K" || input$zvar== "deathRate10K" || input$zvar == "hospRate10K"){
        tempPlot3=ggplot() +
          labs(x="Rate(per 10,000 Residents)", y="County",
               title=paste("Cumulative ",names(varnames)[varnames==input$zvar],"* from ",FirstCase," to ",LastCase, sep = ""),
               subtitle=paste("Updated: ",TODAY),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
      } else {
        tempPlot3=ggplot() +
          labs(x="Count", y="County",
               title=paste("Cumulative ",names(varnames)[varnames==input$zvar]," from ",FirstCase," to ",LastCase, sep = ""),
               subtitle=paste("Updated: ",TODAY),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
      }
      plot3 = tempPlot3 +
        geom_col(data = County_DF(), aes_string(y = paste("reorder(County,",input$zvar,")"), 
                                                x = input$zvar,
                                                fill = "County")) + #bar graph
        geom_text(size=4,data=County_DF(),aes_string(x=input$zvar,y=paste("reorder(County,",input$zvar,")"),label=input$zvar,fill=NULL, hjust = -0.1))+
        xlim(0, switch(input$zvar,
                       "ncases" = max(County_DF()$ncases) + 15000,
                       "ndead" = max(County_DF()$ndead) + 100,
                       "nhosp" = max(County_DF()$nhosp) + 200,
                       "caseRate10K" = max(County_DF()$caseRate10K) + 150,
                       "deathRate10K" = max(County_DF()$deathRate10K) + 2,
                       "hospRate10K" = max(County_DF()$hospRate10K) + 10)) + 
        scale_fill_manual("County",values=ColorCounties) +
        theme_light() +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              axis.text.x =element_blank(),
              axis.text.y = element_text(size = 12))
      print(plot3)
      dev.off()
    },
    contentType = "image/png"
  )
  
  ###################### Tab 4 - bar graphs by age #########################
  output$BarAge <- renderPlot({
    ggplot() +
      labs(x="Date", y=paste(names(varnamesAge)[varnamesAge==input$avar]),
           title=paste(names(varnamesAge)[varnamesAge==input$avar],
                       "by Age Range"),
           subtitle = paste("\n", "Updated: ", TODAY))+
      geom_col(data=Age_DF(), 
               aes_string(
                 x="OnsetDate", 
                 y=input$avar, 
                 group="AgeRange"), 
               fill="#3498DB") +
      scale_x_date(date_breaks = "1 month",date_labels = "%b %d", limits=input$daterangeAge) +
      theme_classic() +
      facet_grid("AgeRange ~ .") +
      theme(
        axis.line.x = element_blank(),
        strip.background = element_rect(colour ="#E74C3C", fill ="#F39C12"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_line(color = "gray95"),
        panel.grid.major.y = element_line(color = "gray95"),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "bold", color = "darkred", size = 14)
      )
  }, height=800)
  
  ###################### Tab 4 - download #########################
  output$tab4Download=downloadHandler(
    filename=function(){"AgeComparisons.png"},
    content=function(file){
      png(file)
      plot4= ggplot() +
        labs(x="Date", y=paste(names(varnamesAge)[varnamesAge==input$avar]),
             title=paste(names(varnamesAge)[varnamesAge==input$avar],
                         "by Age Range"),
             subtitle = paste("\n", "Updated: ", TODAY))+
        geom_col(data=Age_DF(), 
                 aes_string(
                   x="OnsetDate", 
                   y=input$avar, 
                   group="AgeRange"), 
                 fill="#3498DB") +
        scale_x_date(date_breaks = "1 month",date_labels = "%b %d", limits=input$daterangeAge) +
        theme_classic() +
        facet_grid("AgeRange ~ .") +
        theme(
          axis.line.x = element_blank(),
          strip.background = element_rect(colour ="#E74C3C", fill ="#F39C12"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(size = 12),
          panel.grid.major.x = element_line(color = "gray95"),
          panel.grid.major.y = element_line(color = "gray95")
        )
      print(plot4)
      dev.off()
    },
    contentType = "image/png"
  )
  ###################### Tab 5 - bar graphs by sex #########################
  output$BarSex <- renderPlot({
    if(input$bvar=="caseRate10K" || input$bvar== "deathRate10K" || input$bvar == "hospRate10K"){
      tempPlot5= ggplot() +
        labs(x="Onset Date", y="Rate(per 10,000 Residents)",
             title=paste(names(varnames)[varnames==input$bvar],
                         "* by Sex - ", input$MAdaysSex,
                         "d Moving Average"),
             subtitle=paste("Cumulative Ohio Female", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                            switch(input$bvar,
                                   "ncases" = FemaleCountsDF$ncases,
                                   "ndead" = FemaleCountsDF$ndead,
                                   "nhosp" = FemaleCountsDF$nhosp,
                                   "caseRate10K" = FemaleCountsDF$caseRate10K,
                                   "deathRate10K" = FemaleCountsDF$deathRate10K,
                                   "hospRate10K" = FemaleCountsDF$hospRate10K),
                            "\nCumulative Ohio Male", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                            switch(input$bvar,
                                   "ncases" = MaleCountsDF$ncases,
                                   "ndead" = MaleCountsDF$ndead,
                                   "nhosp" = MaleCountsDF$nhosp,
                                   "caseRate10K" = MaleCountsDF$caseRate10K,
                                   "deathRate10K" = MaleCountsDF$deathRate10K,
                                   "hospRate10K" = MaleCountsDF$hospRate10K),
                            "\nUpdated: ",TODAY),
             caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                            "\n","* Rates are counts per 10,000 residents by county."))
    } else {
      tempPlot5= ggplot() +
        labs(x="Onset Date", y="Count",
             title=paste(names(varnames)[varnames==input$bvar],
                         "by Sex - ", input$MAdaysSex,
                         "d Moving Average"),
             subtitle=paste("Cumulative Ohio Female", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                            switch(input$bvar,
                                   "ncases" = FemaleCountsDF$ncases,
                                   "ndead" = FemaleCountsDF$ndead,
                                   "nhosp" = FemaleCountsDF$nhosp,
                                   "caseRate10K" = FemaleCountsDF$caseRate10K,
                                   "deathRate10K" = FemaleCountsDF$deathRate10K,
                                   "hospRate10K" = FemaleCountsDF$hospRate10K),
                            "\nCumulative Ohio Male", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                            switch(input$bvar,
                                   "ncases" = MaleCountsDF$ncases,
                                   "ndead" = MaleCountsDF$ndead,
                                   "nhosp" = MaleCountsDF$nhosp,
                                   "caseRate10K" = MaleCountsDF$caseRate10K,
                                   "deathRate10K" = MaleCountsDF$deathRate10K,
                                   "hospRate10K" = MaleCountsDF$hospRate10K),
                            "\nUpdated: ",TODAY),
             caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                            "\n","* Rates are counts per 10,000 residents by county."))
    }
    tempPlot5+
      geom_col(data=Sex_DF(),
               aes_string(x="Date",
                          y=input$bvar,
                          fill = "Sex"),
               alpha=0.5,
               position = "dodge") +
      geom_ma(data=Sex_DF(),
              aes_string(x = "Date",
                         y = input$bvar,
                         color = "Sex"),
              n=input$MAdaysSex, linetype=1, size=1.25) +
      scale_fill_manual("Sex",values = ColorSex) +
      scale_color_manual("Sex",values = ColorSex) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %d",
                   limits=input$daterangeSex) +
      theme_minimal()+
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "bold", color = "darkred", size = 14)
      )
  })
  # ###################### Tab 5 - download #########################
  output$tab5Download=downloadHandler(
    filename=function(){"SexComparisons.png"},
    content=function(file){
      png(file)
      if(input$bvar=="caseRate10K" || input$bvar== "deathRate10K" || input$bvar == "hospRate10K"){
        tempPlot5= ggplot() +
          labs(x="Onset Date", y="Rate(per 10,000 Residents)",
               title=paste(names(varnames)[varnames==input$bvar],
                           "* by Sex - ", input$MAdaysSex,
                           "d Moving Average"),
               subtitle=paste("Cumulative Ohio Female", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                              switch(input$bvar,
                                     "ncases" = FemaleCountsDF$ncases,
                                     "ndead" = FemaleCountsDF$ndead,
                                     "nhosp" = FemaleCountsDF$nhosp,
                                     "caseRate10K" = FemaleCountsDF$caseRate10K,
                                     "deathRate10K" = FemaleCountsDF$deathRate10K,
                                     "hospRate10K" = FemaleCountsDF$hospRate10K),
                              "\nCumulative Ohio Male", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                              switch(input$bvar,
                                     "ncases" = MaleCountsDF$ncases,
                                     "ndead" = MaleCountsDF$ndead,
                                     "nhosp" = MaleCountsDF$nhosp,
                                     "caseRate10K" = MaleCountsDF$caseRate10K,
                                     "deathRate10K" = MaleCountsDF$deathRate10K,
                                     "hospRate10K" = MaleCountsDF$hospRate10K),
                              "\nUpdated: ",TODAY),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
      } else {
        tempPlot5= ggplot() +
          labs(x="Onset Date", y="Count",
               title=paste(names(varnames)[varnames==input$bvar],
                           "by Sex - ", input$MAdaysSex,
                           "d Moving Average"),
               subtitle=paste("Cumulative Ohio Female", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                              switch(input$bvar,
                                     "ncases" = FemaleCountsDF$ncases,
                                     "ndead" = FemaleCountsDF$ndead,
                                     "nhosp" = FemaleCountsDF$nhosp,
                                     "caseRate10K" = FemaleCountsDF$caseRate10K,
                                     "deathRate10K" = FemaleCountsDF$deathRate10K,
                                     "hospRate10K" = FemaleCountsDF$hospRate10K),
                              "\nCumulative Ohio Male", substring(names(varnames)[varnames==input$bvar], 0, nchar(names(varnames)[varnames==input$bvar]) - 1), ": ",
                              switch(input$bvar,
                                     "ncases" = MaleCountsDF$ncases,
                                     "ndead" = MaleCountsDF$ndead,
                                     "nhosp" = MaleCountsDF$nhosp,
                                     "caseRate10K" = MaleCountsDF$caseRate10K,
                                     "deathRate10K" = MaleCountsDF$deathRate10K,
                                     "hospRate10K" = MaleCountsDF$hospRate10K),
                              "\nUpdated: ",TODAY),
               caption= paste("Source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                              "\n","* Rates are counts per 10,000 residents by county."))
      }
      plot5 = tempPlot5+
        geom_col(data=Sex_DF(),
                 aes_string(x="Date",
                            y=input$bvar,
                            fill = "Sex"),
                 alpha=0.5,
                 position = "dodge") +
        geom_ma(data=Sex_DF(),
                aes_string(x = "Date",
                           y = input$bvar,
                           color = "Sex"),
                n=input$MAdaysSex, linetype=1, size=1.25) +
        scale_fill_manual("Sex",values = ColorSex) +
        scale_color_manual("Sex",values = ColorSex) +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%b %d",
                     limits=input$daterangeSex) +
        theme_minimal()
      print(plot5)
      dev.off()
    },
    contentType = "image/png"
  )
  ###################### Tab 6 - top counties table ####################
  output$table <- renderDataTable(OhioTopCountyDF)
  
  ###################### Tab 6 - download ####################
  output$tab6Download=downloadHandler(
    filename=function(){"OhioCovidData.csv"},
    content=function(file){
      #names table5 the table of the desired amount of counties
      table6 = (OhioTopCountyDF)
      #writes a csv of table5
      write.csv(table6, file, row.names = FALSE)
    },
    contentType = ".csv"
  )
  
}

# run the application 
shinyApp(ui = ui, server = server)