# app1.R

# A shiny app always needs shiny + load other libraries 
library(shiny)
library(tidyverse)

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
      selectInput(inputId = "xvar", label= "Select an x-variable", 
                  choices = c("age.yrs", "fev.L", "ht.in"),
                  selected="age.yrs"),
      
      selectInput(inputId = "yvar", label= "Select an y-variable", 
                  choices = c("age.yrs", "fev.L", "ht.in"),
                  selected="ht.in"),
      
      #add alpha
      #numericInput(inputId = "myalpha", label = "Alpha Transparency: " ,
       #            value = .5, min = .1, max = 1, step = .1),
      
      sliderInput(inputId = "myalpha", label = "Alpha Transparency: " ,
                   value = .5, min = .1, max = 1, step = .1),
      
      sliderInput(inputId = "mysize", label = "Size: " ,
                  value = .5, min = .1, max = 1, step = .1),
      
      checkboxInput(inputId= "color_gender", label = "Color by Gender")
    ),
    
    # Main panel typically used to display outputs
    mainPanel(
      plotOutput(outputId = "myscatterplot")
    )
    
  )
)

### Define server behavior for application here
#  Expressions such as in renderPlot MUST be in {} 

server <- function(input, output) {
  output$myscatterplot <-
    renderPlot({
       if(input$color_gender) {
         ggplot() + 
           geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender", alpha = input$myalpha, size = input$mysize ),
                 data=fev_DF)
       } else {
         ggplot() + 
           geom_point(aes_string(x=input$xvar,y=input$yvar, size = input$mysize), alpha = input$myalpha, data=fev_DF)
       }
    })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)
