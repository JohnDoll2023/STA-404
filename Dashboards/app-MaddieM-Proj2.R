# app-class-LIVEupdate-16nov20.R
# ../Ohio-COVID-MA
#
# Based: code developed for 
#    STA404-Module4PLUSHW-OhioCOVIDdataexploration-Part I.R
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load required packages ......................
library(shiny)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(shinythemes)
library(plotly)
library(dplyr)

#load covid data
OhioDF <- read_csv(file="https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")
TODAY <- Sys.Date()

#DATA CLEANING AND MAKING DATA FRAMES
OhioDF <- OhioDF %>% 
  filter(Sex != "Total") %>% 
  mutate(AgeFactor = factor(`Age Range`),
         OnsetDate = mdy(`Onset Date`))

#bring in data of ohio counties
map.county <- map_data('county')
ohio.county <- subset(map.county, region=="ohio")

#OHIO COUNTIES W POP/1000
#read in population dataset
pop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") 


#clean data frame, rename columns, select only ohio counties
#mutate population /1000, county as just the name, not "_____ county"
#keep only county and population columns
OhioPop=pop %>% 
  filter(STNAME == "Ohio") %>% 
  filter(CTYNAME != "Ohio") %>%
  mutate(Population = POPESTIMATE2019/1000,
         County = substr(CTYNAME, 1, nchar(CTYNAME)-7)) %>% 
  select(County, Population)

#used for choropleth in tab 1 and tables in tab 5
#make OhioCountySums compatable to ohio.county
#adds categories based on ncases
OhioCountySums=OhioDF %>% 
  group_by(County)%>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))%>%
  mutate(subregion = tolower(County),
         CaseCat = cut(ncases,
                       breaks = c(0, 1000, 2500, 5000, 
                                  15000, 30000, 50000, 75000)))

# data frame with counts by County and OnsetDate
#used for tab 2
OhioCountyDF <- OhioDF %>% 
  group_by(County, OnsetDate) %>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))

# data frame with counts by County and OnsetDate and AgeFactor
#used for tab 4
OhioCountyAgeDF <- OhioDF %>% 
  group_by(County, OnsetDate, AgeFactor) %>%
  filter(AgeFactor!="Unknown")%>%
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE)) 

#MERGE MAP AND new OhioCountySums
CaseMap=merge(OhioCountySums, ohio.county, by= "subregion")

#used for county_df() reactive
#MUTATE A NEW DF WITH COUNTY NAME AND rates
#merge OhioCountyDF and Ohio Pop by county
OhioCountyPop = merge(OhioPop, OhioCountyDF,
                      by="County")
OhioCountyPop=OhioCountyPop%>%
  group_by(County, OnsetDate) %>% 
  mutate(caseRate=round(ncases/Population,3),
         hospRate=round(nhosp/Population,3),
         deathRate=round(ndead/Population, 3))

#used in countyAge_DF() and Age_DF() reactive
#MUTATE A NEW DF WITH COUNTY NAME AND rates
#merge OhioCountyAgeDF and Ohio Pop by county
OhioCountyAgePop = merge(OhioPop, OhioCountyAgeDF,
                         by="County")
OhioCountyAgePop=OhioCountyAgePop%>%
  group_by(County, OnsetDate) %>% 
  mutate(caseRate=round(ncases/Population,3),
         hospRate=round(nhosp/Population,3),
         deathRate=round(ndead/Population, 3))

#used in tables in tab 5
#MUTATE A NEW DF WITH COUNTY NAME Sums AND rates
#merge OhioCountySumsDF and Ohio Pop by county
OhioCountySumsPop = merge(OhioPop, OhioCountySums,
                          by="County")
OhioCountySumsPop=OhioCountySumsPop%>%
  group_by(County) %>% 
  mutate(caseRate=round(ncases/Population,3),
         hospRate=round(nhosp/Population,3),
         deathRate=round(ndead/Population, 3))
OhioCountySumsPop=transform(OhioCountySumsPop,
                            ncases = as.integer(ncases),
                            ndead = as.integer(ndead),
                            nhosp = as.integer(nhosp))

# list of possible variables of interest
varnames <- c("Case Count" = "ncases",
              "Case Rate (per 1000 residents)" = "caseRate",
              "Death Count" = "ndead",
              "Death Rate (per 1000 residents)" = "deathRate",
              "Hospitalization Count" = "nhosp",
              "Hospitalization Rate (per 1000 residents)" = "hospRate")

#vector with counts or rates
crchoice = c("counts", "rate per 1000 residents")

# define FirstCase date for use in input
FirstCase <- min(OhioDF$OnsetDate)

# user interface
ui <- fluidPage(
  #shinythemes::themeSelector(), # can be used to explore different themes
  theme = shinytheme("readable"),
  
  # Application title
  titlePanel("Ohio County COVID-19 Exploration - McMillen"),
  
  # Sidebar panel with user controls
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "yvar", 
                  label= "Select response to explore: ", 
                  choices = varnames,
                  selected="cases"),
      selectInput(inputId = "county", 
                  label= "Select county to highlight: ", 
                  choices = unique(OhioDF$County),
                  selected="Butler"),
      selectInput(inputId = "age",
                  label= "Select an age range to highlight",
                  choices = unique(OhioDF$AgeFactor)),
      numericInput("topN", 
                   label = "Select top n Counties for table",
                   value = 5,
                   min=1,
                   max=88,
                   step=1),
      checkboxInput(inputId = "lowFirst", label = "Lowest Counts/Rates first"),
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
    
    # different tabs in the main panel
    # tabs 1-4 have buttons to download the graphs
    mainPanel(
      tabsetPanel(
        tabPanel("1) Ohio Map with Counties",
                 downloadButton("dwnld1", "Download the table"),
                 plotlyOutput("Choropleth", height = "500px")),
        tabPanel("2) County",
                 downloadButton("dwnld2", "Download the table"),
                 plotOutput("CountyPlot")),
        tabPanel("3) Ohio Counties",
                 downloadButton("dwnld3", "Download the table"),
                 plotOutput("BarPlot")),
        tabPanel("4) Age Comparisons",
                 downloadButton("dwnld4", "Download the table"),
                 plotOutput("AgePlot")),
        tabPanel("5) Table of Top Counts/Rates",
                 downloadButton("dwnld5", "Download the table"),
                 tableOutput("Table")),
        tabPanel("6) References",
                 verbatimTextOutput("Refs"))
      )
    )
  )
)

# server that computes the graphs/tables to user specifications
server <- function(input, output) {
  
  #REACTIVE DATA FRAMES BASED ON USER INPUT
  #reactive DF for tab 2
  County_DF <- reactive({
    OhioCountyPop %>%
      filter(County == input$county)
  })
  #reactive DF for tab 4
  CountyAge_DF <- reactive({
    OhioCountyAgePop %>%
      filter(County == input$county)
  })
  #reactive DF for tab 4
  Age_DF <- reactive({
    OhioCountyAgePop %>%
      filter(County== input$county,
             AgeFactor == input$age)
  })
  
  #TAB1
  output$Choropleth <- renderPlotly({
    #CHOROPLETH
    Choropleth = ggplot(CaseMap, aes(x=long,y=lat, group=group,
                                     fill=CaseCat, County = County, ncases=ncases,
                                     nhosp=nhosp, ndead=ndead)) +
      geom_polygon(colour="black") +
      scale_fill_manual(values= c("#f1eef6","#d0d1e6","#a6bddb",
                                  "#74a9cf", "#3690c0", "#0570b0", "#034e7b")) +
      #manually say which colors map to which categories
      coord_map("polyconic")+
      labs(x="",y="",
           title="County Map Cases")+
      guides(fill = FALSE)+#remove legend
      theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks = element_blank())#get rid of axis titles, ticks
    #plots choropleth with plotly without the legend
    ggplotly(Choropleth)%>%
      layout(showlegend=FALSE)
  })
  
  #TAB1 DOWNLOAD
  output$dwnld1=downloadHandler(
    filename=function(){"OhioChoropleth.png"},
    content=function(file){
      png(file)
      plot1 = ggplot(CaseMap, aes(x=long,y=lat, group=group,
                                  fill=CaseCat, County = County, ncases=ncases,
                                  nhosp=nhosp, ndead=ndead)) +
        geom_polygon(colour="black") +
        scale_fill_manual(values= c("#f1eef6","#d0d1e6","#a6bddb",
                                    "#74a9cf", "#3690c0", "#0570b0", "#034e7b")) +
        #manually say which colors map to which categories
        coord_map("polyconic")+
        labs(x="",y="",
             title="County Map Cases")+
        guides(fill = FALSE)+#remove legend
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks = element_blank())#get rid of axis titles, ticks
      print(plot1)
      dev.off()
    },
    contentType = "image/png"
  )
  
  #TAB2
  #COUNTY MOVING AVERAGES GRAPH
  output$CountyPlot <- renderPlot({
    ggplot() +
      labs(x="Onset Date", y="Number",
           title=paste(names(varnames)[varnames==input$yvar],
                       " - ",
                       input$MAdays, "d Moving Average"),
           subtitle=paste("Updated: ",TODAY),
           tag = paste("Only\n", input$county, "\nCounty"),
           caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
      geom_col(data=County_DF(),
               aes_string(x="OnsetDate",
                          y=input$yvar),
               fill= "#d0d1e6") +
      geom_ma(data=County_DF(),
              aes_string(x="OnsetDate",
                         y=input$yvar),
              n=input$MAdays, linetype=1, color="#0570b0",
              size=1.25) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %d",
                   limits = input$daterange) +
      theme_minimal()
  })
  
  #TAB2 DOWNLOAD
  output$dwnld2=downloadHandler(
    filename=function(){"CountyBarGraphs.png"},
    content=function(file){
      png(file)
      plot2 = ggplot() +
        labs(x="Onset Date", y="Number",
             title=paste(names(varnames)[varnames==input$yvar],
                         " - ",
                         input$MAdays, "d Moving Average"),
             subtitle=paste("Updated: ",TODAY),
             tag = paste("Only\n", input$county, "\nCounty"),
             caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
        geom_col(data=County_DF(),
                 aes_string(x="OnsetDate",
                            y=input$yvar),
                 fill= "#d0d1e6") +
        geom_ma(data=County_DF(),
                aes_string(x="OnsetDate",
                           y=input$yvar),
                n=input$MAdays, linetype=1, color="#0570b0",
                size=1.25) +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%b %d",
                     limits = input$daterange) +
        theme_minimal()
      print(plot2)
      dev.off()
    },
    contentType = "image/png"
  )
  
  #TAB3
  #BAR PLOTS COMPARING COUNTY COUNTS/RATES
  output$BarPlot <- renderPlot({
    plot = ggplot(OhioCountySumsPop, aes_string(y= paste("fct_reorder(County, ncases)"),
                                                x=input$yvar,
                                                fill = "CaseCat"))+
      geom_col()+
      scale_fill_brewer()+
      labs(x ="", y = "", title = paste(names(varnames)[varnames==input$yvar]), subtitle = "Ordered by number of Cases")+
      #add the exact count on each bar
      scale_x_continuous(expand = c(0,0))+
      #scoots the bars over to touch the y axis
      guides(fill = FALSE)+ #removes legend
      theme(axis.text.y = element_text(size=10),
            axis.text.x=element_blank(),
            axis.ticks = element_blank())#get rid of axis titles, ticks
    #if else block to adjust the annotation depending on variable of interest
    if(input$yvar=="ncases"){
      plot+
        geom_text(aes_string(label = input$yvar), size=4, nudge_x = 1200)
    } else{
      if(input$yvar=="nhosp"){
        plot+
          geom_text(aes_string(label = input$yvar), size=4, nudge_x = 100)
      } else{
        if(input$yvar=="ndead"){
          plot+
            geom_text(aes_string(label = input$yvar), size=4, nudge_x = 20)
        } else{
          if(input$yvar=="caseRate"){
            plot+
              geom_text(aes_string(label = input$yvar), size=4, nudge_x = 3)
          } else{
            if(input$yvar=="hospRate"){
              plot+
                geom_text(aes_string(label = input$yvar), size=4, nudge_x = .3)
            } else{
              if(input$yvar=="deathRate"){
                plot+
                  geom_text(aes_string(label = input$yvar), size=4, nudge_x = .1)
                
              }
            }
          }
        }
      }
    }
    
  },
  height = 1500,
  width= 1000
  )
  #TAB3 DOWNLOAD
  output$dwnld3=downloadHandler(
    filename=function(){"CountyBarGraphs.png"},
    content=function(file){
      png(file)
      plot3=ggplot(OhioCountySumsPop, aes_string(y= paste("fct_reorder(County, ncases)"),
                                                 x=input$yvar,
                                                 fill = "CaseCat"))+
        geom_col()+
        scale_fill_brewer()+
        labs(x ="", y = "", title = paste(names(varnames)[varnames==input$yvar]), subtitle = "Ordered by number of Cases")+
        scale_x_continuous(expand = c(0,0))+
        #scoots the bars over to touch the y axis
        guides(fill = FALSE)+ #removes legend
        theme(axis.text.y = element_text(size=10),
              axis.text.x=element_blank(),
              axis.ticks = element_blank())#get rid of axis titles, ticks
      #if else block to adjust the annotation depending on variable of interest
      if(input$yvar=="ncases"){
        plot3+
          geom_text(aes_string(label = input$yvar), size=4, nudge_x = 1200)
      } else{
        if(input$yvar=="nhosp"){
          plot3+
            geom_text(aes_string(label = input$yvar), size=4, nudge_x = 100)
        } else{
          if(input$yvar=="ndead"){
            plot3+
              geom_text(aes_string(label = input$yvar), size=4, nudge_x = 20)
          } else{
            if(input$yvar=="caseRate"){
              plot3+
                geom_text(aes_string(label = input$yvar), size=4, nudge_x = 3)
            } else{
              if(input$yvar=="hospRate"){
                plot3+
                  geom_text(aes_string(label = input$yvar), size=4, nudge_x = .3)
              } else{
                if(input$yvar=="deathRate"){
                  plot3+
                    geom_text(aes_string(label = input$yvar), size=4, nudge_x = .1)
                  
                }
              }
            }
          }
        }
      }
      print(plot3)
      dev.off()
    },
    contentType = "image/png"
  )
  
  #TAB4
  #COMPARSE AGE RANGES IN A SPECIFIC COUNTY
  output$AgePlot <- renderPlot({
    ggplot()+
      #titles and labels
      labs(x="Onset Date", y="Number",
           title=paste(names(varnames)[varnames==input$yvar],
                       " - ",
                       input$age, " age range in blue"),
           subtitle=paste("Updated: ",TODAY),
           caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
      #adds grey lines of the trends in yvar over time for each age
      geom_line(data=CountyAge_DF(),
                aes_string(x="OnsetDate",
                           y=input$yvar, group = "AgeFactor"), color="#d0d1e6")+
      #adds a blue line of the trend in the specified age range
      geom_line(data=Age_DF(),
                aes_string(x="OnsetDate",
                           y=input$yvar), color="#0570b0") +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b %d",
                   limits = input$daterange) +
      theme_minimal()
    
  })
  
  #TAB4 DOWNLOAD
  output$dwnld4=downloadHandler(
    filename=function(){"AgeComparisons.png"},
    content=function(file){
      png(file)
      plot4=ggplot()+
        #titles and labels
        labs(x="Onset Date", y="Number",
             title=paste(names(varnames)[varnames==input$yvar],
                         " - ",
                         input$age, " age range in blue"),
             subtitle=paste("Updated: ",TODAY),
             caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
        geom_line(data=CountyAge_DF(),
                  aes_string(x="OnsetDate",
                             y=input$yvar, group = "AgeFactor"), color="#d0d1e6")+
        geom_line(data=Age_DF(),
                  aes_string(x="OnsetDate",
                             y=input$yvar), color="#0570b0") +
        scale_x_date(date_breaks = "1 month",
                     date_labels = "%b %d",
                     limits = input$daterange) +
        theme_minimal()
      print(plot4)
      dev.off()
    },
    contentType = "image/png"
  )
  
  #TAB5
  #table with top n counties with the highest response for yvar
  output$Table <- renderTable({
    #vector of all column names
    cols = c(names(OhioCountySumsPop))
    #the index of the variable of interest in the vector of column names
    index=which(cols==input$yvar)
    #vector of county names
    County=c(OhioCountySumsPop[,1])
    #vector of variable of interest
    Var=c(OhioCountySumsPop[,index])
    #data frame with the rank, county names and variable of interest
    df = data.frame(County,Var)
    #checks if user wants the highest n county counts/rates or the lowest
    if(input$lowFirst){
      #rank vector
      Rank = c(88: 1)
      #data frame arranged lowest first
      dfArranged=df%>%
        arrange(Var)
      #final DF with rank added to arranged table
      dfFinal = data.frame(Rank, dfArranged)
    }else{
      #rank vector
      Rank = c(1: 88)
      #data frame arranged highest first
      dfArranged=df%>%
        arrange(desc(Var))
      #final DF with rank added to arranged table
      dfFinal = data.frame(Rank, dfArranged)
    }
    
    #hard coding to change the name of the "Var" to the desired variable of interest
    if(index==3){
      dfFinal=dfFinal%>%
        rename("Case Count"=Var)
    } else {
      if(index==4){
        dfFinal=dfFinal%>%
          rename("Death Count"=Var)
      } else {
        if(index==5){
          dfFinal=dfFinal%>%
            rename("Hospitalization Count"=Var)
        }else {
          if(index==8){
            dfFinal=dfFinal%>%
              rename("Case Rate (per 1000 residents)"=Var)
          }else {
            if(index==10){
              dfFinal=dfFinal%>%
                rename("Death Rate (per 1000 residents)"=Var)
            }else {
              if(index==9){
                dfFinal=dfFinal%>%
                  rename("Hospitalization Rate (per 1000 residents)"= Var)
              }
            }
          }
        }
      }
    }
    #names table5 the table of the desired amount of counties
    table5 = head(dfFinal, input$topN)
    #prints table5
    table5
    
  },
  digits = 3
  )
  
  output$dwnld5=downloadHandler(
    filename=function(){"TopCountyResponse.csv"},
    content=function(file){
      #vector of all column names
      cols = c(names(OhioCountySumsPop))
      #the index of the variable of interest in the vector of column names
      index=which(cols==input$yvar)
      #vector of county names
      County=c(OhioCountySumsPop[,1])
      #vector of variable of interest
      Var=c(OhioCountySumsPop[,index])
      #data frame with the rank, county names and variable of interest
      df = data.frame(County,Var)
      #checks if user wants the highest n county counts/rates or the lowest
      if(input$lowFirst){
        #rank vector
        Rank = c(88: 1)
        #data frame arranged lowest first
        dfArranged=df%>%
          arrange(Var)
        #final DF with rank added to arranged table
        dfFinal = data.frame(Rank, dfArranged)
      }else{
        #rank vector
        Rank = c(1: 88)
        #data frame arranged highest first
        dfArranged=df%>%
          arrange(desc(Var))
        #final DF with rank added to arranged table
        dfFinal = data.frame(Rank, dfArranged)
      }
      
      #hard coding to change the name of the "Var" to the desired variable of interest
      if(index==3){
        dfFinal=dfFinal%>%
          rename("Case Count"=Var)
      } else {
        if(index==4){
          dfFinal=dfFinal%>%
            rename("Death Count"=Var)
        } else {
          if(index==5){
            dfFinal=dfFinal%>%
              rename("Hospitalization Count"=Var)
          }else {
            if(index==8){
              dfFinal=dfFinal%>%
                rename("Case Rate (per 1000 residents)"=Var)
            }else {
              if(index==10){
                dfFinal=dfFinal%>%
                  rename("Death Rate (per 1000 residents)"=Var)
              }else {
                if(index==9){
                  dfFinal=dfFinal%>%
                    rename("Hospitalization Rate (per 1000 residents)"= Var)
                }
              }
            }
          }
        }
      }
      #names table5 the table of the desired amount of counties
      table5 = head(dfFinal, input$topN)
      #writes a csv of table5
      write.csv(table5, file, row.names = FALSE)
    },
    contentType = ".csv"
  )
  
  #TAB6
  #list of references
  output$Refs = renderPrint({
    #each string is a citation
    str00 = "Maddie McMillen"
    str0 = "12/3/2020"
    str1 = "Brewer, Cynthia, and Mark Harrower. COLORBREWER 2.0. ColorBrewer, The Pennsylvania State University, colorbrewer2.org/"
    str2 = "C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020"
    str3 = "dommer. Shiny: Download Table Data and Plot. Stack Overflow, 18 Nov. 2016, stackoverflow.com/questions/40666542/shiny-download-table-data-and-plot"    
    str4 = "Ganesh, Tinniam  V., and Erdem Akkas. How to Call Reorder within aes_string of Ggplot. Stack Overflow, 16 May 2017, stackoverflow.com/questions/43999317/how-to-call-reorder-within-aes-string-of-ggplot"
    str5 = "Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL http://www.jstatsoft.org/v40/i03/"
    str6 = "Hadley Wickham, Romain Francois, Lionel Henry and Kirill Muller (2020). dplyr: A Grammar of Data Manipulation. R package version 1.0.2. https://CRAN.R-project.org/package=dplyr"
    str7 = "Matt Dancho and Davis Vaughan (2020). tidyquant: Tidy Quantitative Financial Analysis. R package version 1.0.1. https://CRAN.R-project.org/package=tidyquant"
    str8 = "R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/"
    str9 = "Sellum, Jill, and Adam Birenbaum. Shiny Allowling Users to Choose Which Columns to Display. 22 Apr. 2016, stackoverflow.com/questions/36784906/shiny-allowling-users-to-choose-which-columns-to-display"
    str10 = "Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686"
    str11 = "Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2. https://CRAN.R-project.org/package=shinythemes"
    str12 = "Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny"
    
    #concatenation of all citations seperated by two lines
    cat(paste(str00, str0, str1, str2, str3, str4, str5,str6, str7, str8, 
              str9, str10, str11, str12, sep="\n\n"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
