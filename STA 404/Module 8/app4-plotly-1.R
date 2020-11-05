# app4.R
# same as app4.R but with added plotly interaction
# REF:  https://plot.ly/r/shiny-tutorial/
#
# Issue:  some problems with labs and title with plotly implementation

# A shiny app always needs shiny + load other libraries 
library(shiny)
library(devtools)
library(ggplot2)
library(plotly)
library(tidyverse)

# correspondence vector between ui variable names and server variable names
varnames <- c("Age (years)" = "age.yrs",
              "FEV (L)" = "fev.L",
              "Height (in)" = "ht.in")

# read in the data and do any modifications
#fev_DF <- read.table("/home/baileraj/sta404/Data/fev_data.txt", header=T)
fev_DF <- read.table("https://www.users.miamioh.edu/baileraj/classes/sta363/fev_data.txt", header=T)

fev_DF <- fev_DF %>% 
  mutate(gender = ifelse(ind.Male==0,"Female","Male"))

### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "FEV Scatterplot Explorer"),
  sidebarLayout(
    
    # Sidebar typically used to house input controls
    sidebarPanel(
      
      selectInput(inputId = "yvar", label= "Select an y-variable", 
                  choices = varnames),
      
      # selectInput(inputId = "xvar", label= "Select an x-variable", 
      #             choices = varnames),

      uiOutput(outputId = "xvar"),
      
      checkboxInput(inputId= "color_gender", label = "Color by Gender")
    ),
    
    # Main panel typically used to display outputs
    mainPanel(
      plotlyOutput(outputId = "myscatterplot")
    )
    
  )
)

### Define server behavior for application here
#  Expressions such as in renderPlot MUST be in {} 

server <- function(input, output) {
  
  output$xvar <- renderUI({
    xvarsubset <- varnames[varnames!=input$yvar]
    selectInput(inputId = "xvar",
                label = "Select an X-variable",
                choices = xvarsubset)
  })
  
  output$myscatterplot <- renderPlotly({
      p1 <- ggplot() 
        labs(x=names(varnames)[varnames==input$xvar],
             y=names(varnames)[varnames==input$yvar]
             ) +
        # ,
        #      title(paste("Scatterplot of ",input$yvar,"by",input$xvar))) +
        theme_bw()
      if(input$color_gender) {
         p1 +
          geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"),
                     data=fev_DF)
      } else {
        p1 +
          geom_point(aes_string(x=input$xvar,y=input$yvar),data=fev_DF)
      }


    }
  )
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)


