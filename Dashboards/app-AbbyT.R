
#load in library data sets
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(maps)
library(tidyquant)
library(ggthemes)
##########################################################################################

#load in data set
OhioDF <-read_csv(file = "https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")
# read ohio county population data from the US Census
USCountyPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")


# reminder that the data changes daily and make Today
TODAY <- Sys.Date()
TODAY
####################### Data for Ohio Map ######################

# process data
OhioDF <- OhioDF %>%
    filter(Sex != "Total") %>%
    mutate(AgeFactor = factor(`Age Range`),
           OnsetDate = mdy(`Onset Date`))


OhioCountyPop <- USCountyPop %>%
    filter(STNAME == "Ohio") %>%
    filter(CTYNAME != "Ohio") %>%
    select(CTYNAME, POPESTIMATE2019) %>%
    mutate(countyName =
               str_remove_all(CTYNAME," County"))

# data frame with counts by County 
OhioCountySums <- OhioDF %>%
    group_by(County) %>%
    summarize(ncases = sum(`Case Count`,na.rm=TRUE),
              ndead = sum(`Death Due to Illness Count`,
                          na.rm=TRUE),
              nhosp = sum(`Hospitalized Count`,
                          na.rm=TRUE))

# Ohio Totals 
OhioTotalSums <- OhioCountySums %>% 
    summarize(OHncases = sum(ncases),
              OHndead = sum(ndead),
              OHnhosp = sum(nhosp))


# Ohio County Map information 

map.county <- map_data('county')  
# ggplot2 function - turns maps pkg data into DF

# restrict to Ohio 
ohio.county <- subset(map.county,
                      region=="ohio")


# need to change case before merging ...
OhioCountySums$County <-
    str_to_lower(OhioCountySums$County)
OhioCountyPop$countyName <-
    str_to_lower(OhioCountyPop$countyName)

# merge Case Count AND population
#        and calculate rates ........


OhioCasePop <- OhioCountySums %>%
    inner_join(OhioCountyPop,
               by=c("County" = "countyName")) %>%
    mutate(CaseRate =
               ncases/POPESTIMATE2019*1000,
           HospRate =
               nhosp/POPESTIMATE2019*1000,
           DeathRate =
               ndead/POPESTIMATE2019*1000)

ohio.county.Plot.data <- merge(
    ohio.county,
    OhioCasePop,
    by.x="subregion", by.y="County")

#https://www.geeksforgeeks.org/convert-first-letter-of-every-word-to-uppercase-in-r-programming-str_to_title-function/#:~:text=Matrix%20in%20R-,Convert%20First%20letter%20of%20every%20word%20to,R%20Programming%20%E2%80%93%20str_to_title()%20Function&text=str_to_title()%20Function%20in%20R,are%20converted%20to%20lower%20case.
# capitalize first letter
ohio.county.Plot.data$subregion<-
str_to_title(ohio.county.Plot.data$subregion)

# making a cataglicalvariabke for ohio map coloring
ohio.county.Plot.data<- ohio.county.Plot.data %>%
    mutate(Casescat=cut(ncases, breaks=c(0,1000, 6000,10000, 30000, 100000)))%>%
    mutate(County=subregion,Cases=ncases,Deaths=ndead, Hospitalizations=nhosp, HospitalizationRate=HospRate)
    


####################### Data for Ohio County Over Time tab ################

# this is making the cases data set 
OhioC<- OhioDF %>%
    mutate (OnsetDate = mdy(`Onset Date`))%>%
    group_by(OnsetDate, County )%>%
    summarize(Cases = sum(`Case Count`, na.rm = TRUE))

OhioD<- OhioDF %>%
    mutate (OnsetDate = mdy(`Onset Date`))%>%
    group_by(OnsetDate, County )%>%
    summarize(Deaths = sum(`Death Due to Illness Count`, na.rm = TRUE))

OhioH<- OhioDF %>%
    mutate (OnsetDate = mdy(`Onset Date`))%>%
    group_by(OnsetDate, County )%>%
    summarize(Hospitalizations = sum(`Hospitalized Count`, na.rm = TRUE))
# join togther
OhioCH<-inner_join (OhioC, OhioH,  by = c("County", "OnsetDate"))
OhioCDH<-inner_join (OhioCH, OhioD,  by = c("County", "OnsetDate")) 
# needed for merging other wise was getting a weird error
OhioCountyPop$countyName <-
    str_to_lower(OhioCountyPop$countyName)
OhioCDH$County <-
    str_to_lower(OhioCDH$County)
OhioCountyPop1<-OhioCountyPop

#making rate data
OhioF <- inner_join(OhioCDH, OhioCountyPop1, by=c("County" = "countyName")) %>%
    mutate(CaseRate = 
               Cases/POPESTIMATE2019*1000,
           HospRate = 
               Hospitalizations/POPESTIMATE2019*1000,
           DeathRate = 
               Deaths/POPESTIMATE2019*1000)
# caplitalize first letetr
OhioF$County<-
    str_to_title(OhioF$County)
#alphabtizing
OhioF <- OhioF[order(OhioF$County),]


####################### Data for Ohio County Totals tab ################
# this is making the cases data set 
CountyC<- OhioDF %>%
    group_by(County)%>%
    summarize(Cases = sum(`Case Count`))
CountyD<- OhioDF %>%
    group_by(County)%>%
    summarize(Deaths = sum(`Death Due to Illness Count`))
CountyH<- OhioDF %>%
    group_by(County)%>%
    summarize(Hospitalizations = sum(`Hospitalized Count`))

# then inter join the first two
CountyCD<-inner_join (CountyC, CountyD, by = "County")
CountyCD
# inter join the new one and the final one to the final data fram
CountyF<-inner_join (CountyCD, CountyH, by = "County")
CountyF

# neede to make sure meger is correct all lower case
OhioCountyPop$countyName <-
    str_to_lower(OhioCountyPop$countyName)
OhioCountyPop2<-OhioCountyPop
CountyF$County <-
    str_to_lower(CountyF$County)

# merging and keepng to 40 by case count and rounding rates to decimal points
CountyT <- inner_join(CountyF, OhioCountyPop2, by=c("County" = "countyName")) %>%
    mutate(CaseRate = 
               Cases/POPESTIMATE2019*1000,
           HospRate = 
               Hospitalizations/POPESTIMATE2019*1000,
           DeathRate = 
               Deaths/POPESTIMATE2019*1000)
CountyT<-CountyT %>%
top_n(40,Cases) %>%
mutate(DeathRate=round(DeathRate, digits = 2),
       HospRate=round(HospRate, digits = 2),
       CaseRate=round(CaseRate, digits = 2))
# capitalize fisrt letetr
CountyT$County<-
    str_to_title(CountyT$County)

####################### Data for Ohio Age Comparisons ################
# grouings and sumeruizing will aloow for faceting by age latter on 
OhioCountyAge <- OhioDF %>% 
    group_by(County, OnsetDate, `Age Range`) %>%
    summarize(Cases = sum(`Case Count`,na.rm=TRUE),
              Deaths = sum(`Death Due to Illness Count`,
                          na.rm=TRUE),
              Hospitalizations = sum(`Hospitalized Count`,
                          na.rm=TRUE))%>%
    mutate(AgeRange= `Age Range`)

####################### Data for COVID Table ################
# simplifying table for clarity and readability just selected vriables of intrest
CountyTable<-CountyT %>%
    select("County","Cases", "Hospitalizations", "Deaths", "CaseRate", "HospRate", "DeathRate")
############################################################################################

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Interactive Ohio COVID Dashboard"),

        
        # Creating inputs and tabs
            tabsetPanel(
                        tabPanel("Ohio Map", plotlyOutput("OM")), # first page no interactions
                        
                        tabPanel("Ohio County Over Time", fluid=TRUE, #fuild allos for diffrent sidebars 
                            sidebarLayout(   
                            sidebarPanel(selectInput(inputId = "yvars", label=  " Y Variable:", 
                                            choices = c("Cases", "Deaths", "Hospitalizations","CaseRate", "HospRate", "DeathRate"),
                                            selected="Cases"),
                
                            selectInput(inputId = "County", label=  " County of Intrest:", 
                                                     choices = unique(OhioF$County),
                                                     selected="Butler"),
                            sliderInput("ma", "Select Number of Days for Moving Avrage:",
                                        min = 0, max = 31, value = 0, animate = TRUE),
                            animationOptions( interval = 10,loop = FALSE, playButton = NULL, # this is animated
                                             pauseButton = NULL)
                            ),
                        
                            mainPanel(
                            plotOutput("OCoT")
                                     )
                            )
                        ),
                        tabPanel("Ohio County Totals",  fluid=TRUE, 
                            sidebarLayout(
                            sidebarPanel(selectInput(inputId = "yvar", label=  " X Variable:", 
                                            choices = c("Cases", "Deaths", "Hospitalizations","CaseRate", "HospRate", "DeathRate"),
                                            selected="Cases")
                            ),
                            mainPanel(
                                plotOutput("OCT")
                            )
                        )
            ),
            
                        tabPanel("Ohio Age Comparisons", fluid=TRUE,
                                 sidebarLayout(   
                                     sidebarPanel(selectInput(inputId = "yvarss", label=  " Y Variable:", 
                                                              choices = c("Cases", "Deaths", "Hospitalizations"),
                                                              selected="Cases"),
                                                  
                                                  selectInput(inputId = "Countys", label=  " County of Intrest:", 
                                                              choices = unique(OhioCountyAge$County),
                                                              selected="Butler")
                                     ),  
                                     mainPanel(
                                         plotOutput("OAC")
                                     )
                                 )
                        ),
                        tabPanel("COVID Table",tableOutput("CT")), # this is a table !
                             
                        tabPanel("Acknowledgments and References", # this allos for text only
                                 verbatimTextOutput("txt") )
                                 
            )
        )


# Define server logic required to draw plots
server <- function(input, output) {
    
    output$OM <- renderPlotly({
    #this is the plot of counties in the map of ohio with amount of covid cases uses plotly
        OMplot <- ggplot(ohio.county.Plot.data,
                     aes_string(x="long",y="lat",
                                group="subregion", #this will feed in to the tooltip
                                fill = "Casescat",
                                County = "County",
                                Cases = "Cases",
                                Deaths = "Deaths",
                                Hospitalizations = "Hospitalizations",
                                CaseRate = "CaseRate",
                                DeathRate = "DeathRate",
                                HospRate = "HospitalizationRate")) +
            geom_polygon(color="black") +
            scale_fill_brewer(palette = "Blues")+ # colose color pallet
            coord_map("polyconic") +
            ggtitle("COVID Cases in Ohio Counties")+
            theme_map() +
            guides(fill=FALSE)
        
        ggplotly(OMplot,
                 tooltip = 
                     c("County","Cases","Deaths",
                       "Hospitalizations","CaseRate",
                       "DeathRate","HospRate")) %>% 
            layout(showlegend=FALSE)
        
    })
    
    output$OCoT <- renderPlot({
        
        OCoTplot <-ggplot(subset(OhioF, County %in% c(input$County)), mapping = aes_string( y=input$yvars,  x= "OnsetDate"))+ 
            geom_col(color="#95acc7", fill="#95acc7") +
            geom_ma(n=input$ma,  na.rm = TRUE, aes(linetype="a", color= "darkred", size="2")) + # this was being a bt weird on line size i hope it works for you
            ggtitle(input$yvars) +
            theme(plot.title = element_text(),
                  legend.position = "none",
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  panel.background = element_blank())
        
        OCoTplot
    })
    
    
 
    output$OCT <- renderPlot({ 
        #there is a cordinate flip because it wont work other wise may be a datat fram issue
        OCTplot <-ggplot() + # the fac reorer is what makes ordnized by cases
        geom_col(data = CountyT,
                 mapping = aes_string( y=input$yvar, 
                                       x= "fct_reorder(County,Cases)",
                                       fill=input$yvar))+
            coord_flip()+
            ggtitle(input$yvar)+
           scale_fill_gradient(low="#93BEEC94", high="#366092")+  #custom colors
        theme(plot.title = element_text(hjust = 0.5), # center title and remove undeded clutter
                   legend.position = "none",
                   panel.grid = element_blank(),
                   axis.title = element_blank(),
                   axis.text.x = element_blank( ),
                   axis.ticks = element_blank(),
                   panel.background = element_blank()) +
             geom_text(data = CountyT, mapping = aes_string(label=input$yvar, 
                                                            y=input$yvar, 
                                                            x= "fct_reorder(County,Cases)"),
                       hjust=-.1,  size=4)
        OCTplot
        
    })
 
    output$OAC <- renderPlot({
        
        OACplot <-ggplot(subset(OhioCountyAge, County %in% c(input$Countys)), mapping = aes_string( y=input$yvarss,  
                                                             x= "OnsetDate")) +
            geom_col(color="#95acc7", fill="#95acc7") +
            facet_grid(AgeRange ~ .) +
            ggtitle(input$yvarss)+
            theme(plot.title = element_text(),
                  legend.position = "none",
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  panel.background = element_blank())
        
        OACplot  
    })   

    #https://stackoverflow.com/questions/21548843/r-shiny-different-output-between-rendertable-and-renderdatatable
    #https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
    output$CT <- renderTable({ CountyTable }) 

          output$txt <-  renderText({ invisible("Created by Abigail Tietjen on 12/4/2020
          
          \nThank you to Dr. John A. Bailer for their suport on this project 
                                                \nData was sourced from the following resources
                                                
                    https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv
                    
                    https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
                    
                    Original S code by Richard A. Becker, Allan R. Wilks. R version by Ray Brownrigg. Enhancements by Thomas P Minka
                    and Alex Deckmyn. (2018). maps: Draw Geographical Maps. R package version 3.3.0.https://CRAN.R-project.org/package=maps
                    
                    
                    
        Rstudio and packages also used include
        
                    R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical
                    Computing, Vienna, Austria. URL https://www.R-project.org/.
                    
                    Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework
                    for R. R package version 1.5.0. https://CRAN.R-project.org/package=shiny
                    
                    Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,https://doi.org/10.21105/joss.01686
                    
                    Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical
                    Software, 40(3), 1-25. URL http://www.jstatsoft.org/v40/i03/.
                    
                    H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
                    
                    C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida,2020.
                    
                    Matt Dancho and Davis Vaughan (2020). tidyquant: Tidy Quantitative Financial Analysis. R package version 1.0.1.
                    https://CRAN.R-project.org/package=tidyquant
                    
                    Jeffrey B. Arnold (2019). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.0.
                    https://CRAN.R-project.org/package=ggthemes


        Other Resources used are as follows 
                    
                    https://www.geeksforgeeks.org/convert-first-letter-of-every-word-to-uppercase-in-r-programming-str_to_title-function/#:~:text=Matrix%20in%20R-,Convert%20First%20letter%20of%20every%20word%20to,R%20Programming%20%E2%80%93%20str_to_title()%20Function&text=str_to_title()%20Function%20in%20R,are%20converted%20to%20lower%20case.
                    
                    https://stackoverflow.com/questions/21548843/r-shiny-different-output-between-rendertable-and-renderdatatable
                    
                    https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
                    
                    https://shiny.rstudio.com/reference/shiny/1.4.0/renderPrint.html
                                                " ) })
        
         
          
}
# Run the application 
shinyApp(ui = ui, server = server)
