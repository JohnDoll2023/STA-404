#Deirdre Sperry
#app.R
#dir:C:\Users\deird\OneDrive\Documents\STA404_Classnotes
#Revised 03 DEC 2020
#Sources: Ohio Department of Health & United States Census Bureau
#Created for: STA404 "Project 2: Dynamic Data Display (Final Project)", Instructor: Dr. Bailer
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(plotly)
library(tidyquant)
library(shiny)
library(shinythemes)
#Read in COVID19 Dataset....................
OhioMapInfo <- read_csv(file = "https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")

#Tab One Dataframe.....................................................
OhioCty <- OhioMapInfo %>% #creates count of cases, makes county lower case, and creates categories based on the number of cases
  filter(Sex != "Total") %>% 
  group_by(County) %>% 
  summarize(Cases = sum(`Case Count`),
            Hospitalizations = sum(`Hospitalized Count`,na.rm = TRUE),
            Deaths = sum(`Death Due to Illness Count`,na.rm = TRUE), 
            Recovered = (sum(`Case Count`)-sum(`Death Due to Illness Count`))) %>% 
  mutate(County = str_to_upper(County)) %>% 
  mutate(CaseCat = cut(Cases,
                       breaks = c(0, 1000, 2500, 5000, 
                                  15000, 30000, 50000, max(Cases)+1000))) 

CountyPop <-read_csv(file = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv") #enter in population data from csv to dataframe
CountyPop <- CountyPop %>% #remove irrelevant rows and filter out meaningless county 
  filter(STNAME == "Ohio") %>%   
  subset(select=-c(1:6,8:18,20:164)) %>%   
  filter(CTYNAME != "Ohio")
CountyPop <- CountyPop %>%   
  mutate(County = str_to_upper(str_remove_all(CTYNAME, " County")),
         Pop2019 = POPESTIMATE2019)  #remove spaces from observation names and make them lowercase
CountyPop <- CountyPop[c("County","Pop2019")] #select necessary variables

OhioCty <- merge(OhioCty, CountyPop) #merge covid data and population data
map.county <- map_data('county') #enter map dataframe
ohio.county <- subset(map.county, region=="ohio") #narrow dataframe down to ohio
ohio.county <- ohio.county %>% #remove spaces from subregion 
  mutate(County = str_to_upper(str_trim(subregion, side = "both")))

OhioTab1 <- merge(ohio.county, OhioCty, by=c("subregion"="County")) #merge map and COVID data

OhioTab1 <- OhioTab1 %>%
  mutate(`Case Rate` = round((Cases/Pop2019)*10000, digits = 2),
         `Death Rate` = round((Deaths/Pop2019)*10000, digits = 2),
         `Hospitalization Rate` = round((Hospitalizations/Pop2019)*10000, digits = 2))

  

#Tabs Two and Four DataFrame...........................................................................

OhioTab2 <- OhioMapInfo %>% #filter out meaningless variable responses
  filter(Sex != "Total") %>% 
  group_by(County) %>% 
  mutate(AgeFactor = factor(`Age Range`), #creates columns for dates and ages readable in lubridate
    OnsetDate = mdy(`Onset Date`),
    DeathDate = mdy(`Date Of Death`),
    HospitalDate = mdy(`Admission Date`),
    RecoveryDate = mdy(`Onset Date`) + days(21)) 

OhioTab2 <- OhioTab2 %>% 
  group_by(County,OnsetDate,DeathDate,HospitalDate,RecoveryDate,AgeFactor)%>% 
  summarize(Cases = sum(`Case Count`), #counts responses
            Hospitalizations = sum(`Hospitalized Count`,na.rm = TRUE),
            Deaths = sum(`Death Due to Illness Count`,na.rm = TRUE), 
            Recovered = (sum(`Case Count`)-sum(`Death Due to Illness Count`))) %>% 
  mutate(County = str_to_upper(County)) #makes county uppercase

OhioTab2 <-merge(OhioTab2, CountyPop) #merges datasets

OhioTab2 <- OhioTab2 %>% #creates rate columns
  mutate(`Case Rate` = round((Cases/Pop2019)*10000, digits = 2),
         `Death Rate` = round((Deaths/Pop2019)*10000, digits = 2),
         `Hospitalization Rate` = round((Hospitalizations/Pop2019)*10000, digits = 2))



# #Tabs Three and Five DataFrame........................................................................
OhioTab3 <- OhioCty[c("County","Cases","Hospitalizations","Deaths","Recovered")] #select specific columns
OhioTab3 <- merge(OhioTab3, CountyPop)
OhioTab5 <- OhioTab3 %>%#add rate columns
  mutate(`Case Rate` = round((Cases/Pop2019)*10000, digits = 2),
         `Death Rate` = round((Deaths/Pop2019)*10000, digits = 2),
         `Hospitalization Rate` = round((Hospitalizations/Pop2019)*10000, digits = 2))
OhioTab3 <- OhioTab5 %>% #select top 30 counties by case count
  top_n(30, Cases)

OhioTab3 <- OhioTab3 %>% 
  mutate(County = fct_reorder(County,Cases))


TODAY <- Sys.Date() #create object of today's date
TODAY

varnames <-c("Cases"="Cases", #create named vector
              "Deaths"="Deaths",
              "Hospitalizations"="Hospitalizations",
              "Recovered"="Recovered",
              "Case Rate"="`Case Rate`",
              "Death Rate"="`Death Rate`",
              "Hospitalization Rate"="`Hospitalization Rate`") #create smaller named vector
threenames <- c("Cases"="Cases",
                "Deaths"="Deaths",
                "Hospitalizations"="Hospitalizations",
                "Recovered"="Recovered")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("united"), #adapts shiny theme
  # Application title
  titlePanel(paste("Ohio COVID-19 Data | Updated:", TODAY)), #apptitle


    
    # Show a plot of the generated distribution
      tabsetPanel( #creates separate tabs for each figure/table/graph
        tabPanel("Ohio Map",fluid=TRUE, #tab title
                 mainPanel(plotlyOutput("COVIDmap"))
        
        ),
        tabPanel("County Data Over Time", fluid=TRUE,
                 sidebarLayout( #creates user input options
                   sidebarPanel(
                     selectInput(inputId = "response", label= "Select a Parameter:", #creates selection bar
                                 choices = varnames,
                                 selected="Cases",
                                 width = "300px"),
                     selectInput(inputId = "county", 
                                 label= "Select county to highlight: ", 
                                 choices = c(unique(OhioTab2$County)),
                                 selected="BUTLER",
                                 width = "300px"),
                     sliderInput("MAdays", #creates slider bar
                                 "Days averaged:",
                                 min = 2,
                                 max = 30,
                                 value = 7,
                                 width = "400px"), width = 3
                   ),
                   mainPanel(plotOutput("Countygraph")) #connects ui with output on server
                 
                     )
                   ),

      tabPanel("County Data in Bar Graph", fluid=TRUE,
               sidebarLayout(
                 sidebarPanel(
                       selectInput(inputId = "xvar", label= "Select a Parameter:",
                               choices = varnames,
                               selected="Cases",
                               width = "300px"), width = 2
                 ),
                 mainPanel(plotOutput("Countybar"),width = "200%")
                 )
               ),
      tabPanel("County Data by Age", fluid=TRUE,
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "parameter", label = "Select a Parameter",
                               choices = varnames,
                               selected = "Cases",
                               width = "300px"),
                   selectInput(inputId = "region",
                               label= "Select county to highlight: ",
                               choices = c(unique(OhioTab2$County)),
                               selected="BUTLER",
                               width = "300px"), width = 2
                 ),
                 mainPanel(plotOutput("Ageline"))
                 )
               ),
      tabPanel("Download Data",fluid=TRUE,
               sidebarLayout(
                 sidebarPanel(
                   downloadButton("downloadData","Download Full County Data"), width=2
                 ), 
                 mainPanel(tableOutput("table"))
                 )
               
                 ),
      tabPanel("Acknowledgments and References", fluid=TRUE,
                   mainPanel( #creates formatted text/paragraphs
                     h3(strong("Creator:"),"Deirdre Sperry"),
                     h3(strong("Date of Construction:"),"Dec. 3, 2020"),
                     h3(strong("References")),
                     p(strong("Data Sources:"),"Ohio Department of Health and The US Census Bureau"),
                     p(strong("R Packages:"),"C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020."),
                     p("Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. http://www.jstatsoft.org/v40/i03/."),
                     p("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016."),
                     p("Jeffrey B. Arnold (2019). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.0. https://CRAN.R-project.org/package=ggthemes"),
                     p("Matt Dancho and Davis Vaughan (2020). tidyquant: Tidy Quantitative Financial Analysis. R package version 1.0.1.https://CRAN.R-project.org/package=tidyquant"),
                     p("R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/."),
                     p("Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686"),
                     p("Winston Chang (2018). shinythemes: Themes for Shiny. R package version 1.1.2. https://CRAN.R-project.org/package=shinythemes"),
                     p("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R package version 1.5.0.https://CRAN.R-project.org/package=shiny"),
                     p(strong("Additional Resources:")),
                     p("Creating a Table and Downloadable dataset: https://shiny.rstudio.com/gallery/file-download.html , https://shiny.rstudio.com/reference/shiny/1.0.5/downloadHandler.html"),
                     p("Creating a Table and Downloadable dataset: https://shiny.rstudio.com/gallery/file-download.html , https://shiny.rstudio.com/reference/shiny/1.0.5/downloadHandler.html"),
                     p("Making a plotly output rather than a plot: https://stackoverflow.com/questions/37663854/convert-ggplot-object-to-plotly-in-shiny-application"),
                     p("Hiding the legend on a ggplotly object: https://rdrr.io/cran/plotly/man/hide_legend.html"),
                     p("How to use hide_legend: https://github.com/ropensci/plotly/issues/842"),
                     p("Different sidebars for each tab: https://community.rstudio.com/t/different-inputs-sidebars-for-each-tab/1937"),
                     p("Functions in tidyquant: https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ04-charting-with-tidyquant.html"),
                     p("Top Thirty Observations: https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/"),
                     p("Select Input/Sidepanel Size: https://shiny.rstudio.com/reference/shiny/latest/selectInput.html, https://shiny.rstudio.com/reference/shiny/0.11/sidebarPanel.html"),
                     p("Maintaining subtitle text in ggplotly: https://datascott.com/blog/subtitles-with-ggplotly/"),
                     p("R colors: https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf"),
                     p("Changing facet labels: https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/"),
                     p("https://datavizpyr.com/how-to-remove-facet_wrap-title-box-in-ggplot2/#:~:text=removing%20facet_wrap()'s%20grey%20title%20box&text=theme()%20function%20in%20ggplot2,argument%20to%20theme()%20function.&text=Voila%2C%20now%20the%20default%20grey%20box%20is%20gone%20in%20each%20facet."),
                     p("Adding a caption to a table: https://gist.github.com/cmishra/87bad81d4eed495272ae"),
                     p("Plot sizing: https://www.rdocumentation.org/packages/plotly/versions/4.9.2.1/topics/ggplotly"),
                     p("Adjusting location geom_text: https://www.gl-li.com/2017/08/18/place-text-at-right-location/,https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control")

                   )
                 )

))
  
      
      
    



  



# Define server logic required to draw a histogram
server <- function(input, output) {
  County_DF <- reactive({
    OhioTab2 %>%
      filter(County %in% c(input$county))
  })
   #makes dataset reactive
  tab <- OhioTab3 #creates object for use as table
  data <- OhioTab5 #creates object to download as csv
  
  County_Age <- reactive({ #creates reactive dataset for Tab 4
    OhioTab2 %>%
      filter(County %in% c(input$region),
             AgeFactor != "Unknown")
    })

# MAX <- reactive({max(input$response)})

  output$COVIDmap <-
    renderPlotly({
        
          map <-ggplot(OhioTab1,aes(x=long,y=lat,group=County,cc=Cases,cd=Deaths,ch=Hospitalizations,cr=Recovered, ca=`Case Rate`,dr=`Death Rate`,hr=`Hospitalization Rate`),show.legend=FALSE) + #creates plot and dummy variables for tool tip
            geom_polygon(aes(fill=CaseCat), color="black",show.legend = FALSE) +
            scale_fill_brewer(palette="OrRd") + #blue color scheme
            coord_map() + #mapscale
            labs(title = "Ohio COVID19 Statistics by County",
                 caption = "Darker color indicates higher case count")+#plot labels
             theme(text=element_text(size=9),
                  axis.text = element_blank(), #adjusts theme elements
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank())
          map1 <- ggplotly(map,tooltip = c("group","cc","cd","ch","cr","ca","dr","hr"),width= 700, height=600) %>% #creates plotly object and selects what user sees during interaction
            layout(title = list(text = paste0('Ohio COVID19 Statistics by County', #adds title to ggplotly object
                                              '<br>',
                                              '<sup>',
                                              'Darker color indicates higher case count                                  ',
                                              '</sup>')))
          
          hide_legend(map1) #hides the legend on the ggplotly object
          
  })
  output$Countygraph <-
    renderPlot({
      
  casetotal <- ggplot(County_DF(),aes_string(x="OnsetDate", y=input$response)) + #creates plot
    geom_col(color="grey")+
    geom_ma(data=County_DF(), #creates reactive moving average
           aes_string(x = "OnsetDate",
                     y = input$response),
         color="darkorange",
        n=input$MAdays, linetype=1, size=1) +
    scale_x_date(breaks = seq(as.Date("2020-01-01"), as.Date(paste(TODAY)), by="1 months"), #sets x-axis
                 date_labels = "%b %d",
                 limits = as.Date(c('2020-01-01',paste(TODAY)))) +
   # scale_y_discrete(limits = c(0,max(input$response)))+
    labs(title=paste(input$county,"COUNTY"), #title
        subtitle=paste( names(varnames)[varnames==input$response],
                     " - ", input$MAdays,
                     "Day Moving Average"),
         
           x="", #creates labels
         y="") +
    scale_y_continuous(expand = c(0,0))+ #removes space between bars and axis
    theme(text=element_text(size=12), #adjusts theme
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  casetotal}, width = 1000, height = 700 #changes size of produced plot
)
  output$Countybar <- renderPlot({

    county <-ggplot(OhioTab3, aes_string(y="fct_reorder(County,Cases)", x=input$xvar)) +
      geom_col(width=0.9, position = position_dodge(width=1)) + #bar graph
      geom_text(size=3,data=OhioTab3,aes_string(x=input$xvar,y="fct_reorder(County,Cases)",label=input$xvar,fill=NULL, hjust = -0.02))+
      scale_fill_brewer() + #blue color scheme
      labs(title = paste(names(varnames)[varnames==input$xvar],"in Top 30 Counties"),
           subtitle = "Displayed Counties Determined by Case Count",
        x="", #creates labels
           y="")+
      theme_classic() +
      #scale_x_continuous(expand = c(0,0))+
      theme(text=element_text(size=12), #adjusts elements of theme
            legend.position = "none",
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line = element_blank())
    county }, width=700, height=700
  )
  output$Ageline <- 
    renderPlot({
    ages <- ggplot(County_Age(),aes_string(x="OnsetDate", y=input$parameter)) + 
      geom_col() +
      facet_wrap(~AgeFactor)+
      theme_classic()+
      labs(title= paste(input$region,"COUNTY"),
           subtitle = paste(names(varnames)[varnames==input$parameter]),
           x="Date",
           y="")+
      theme(plot.title=element_text(size=12),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line = element_blank(),
            strip.background = element_blank(), #adjust aspect of facet labeling
            strip.text = element_text(size = 10, face = "bold",color = "darkorange"))
    ages
  }, width = 1000, height=700)
  output$table <- renderTable ({
    tab}, caption=paste("Top 30 Counties by Case Count"),caption.placement = getOption("xtable.caption.placement", "top")
  )
  output$downloadData <- downloadHandler( #creates dowload button of table
    filename = function() { #how to is listed in references
      paste("data-", Sys.Date(), ".csv", sep="") #makes download a csv object
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )


 

}

# Run the application 
shinyApp(ui = ui, server = server)


