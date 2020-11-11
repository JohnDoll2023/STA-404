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
      
      sliderInput(inputId = "myalpha", label = "Alpha Transparency: " ,
                   value = .5, min = .1, max = 1, step = .1),
      
      sliderInput(inputId = "mysize", label = "Size: " ,
                  value = 2, min = .1, max = 5, step = .1),
      
      checkboxInput(inputId= "color_gender", label = "Color by Gender"),
      
      #jitter option
      sliderInput(inputId= "jpoint", label = "Jitter Points: ",
                  value = 0, min = 0, max = 1, step = .2),
      
      #loess smooth
      checkboxInput(inputId= "lsmooth", label = "Loess Smooth"),
      
      #facet
      checkboxInput(inputId= "facet", label = "Facet")
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
         if(input$facet) {
           
           #plot if color gender, facet, and loess smooth are activated
           if(input$lsmooth) {
             ggplot(aes_string(x=input$xvar,y=input$yvar,color="gender"), data=fev_DF) + 
               geom_point(alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint)) +
               geom_smooth(method="loess", se=FALSE) +
               facet_wrap( ~ ind.Male)
           } else {
             
             #plot if color gender and facet are activated
             ggplot(aes_string(x=input$xvar,y=input$yvar,color="gender"), data=fev_DF) + 
               geom_point(alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint)) +
               facet_wrap( ~ ind.Male)
           }
         
         } else {
           
           #plot if color gender and loess smooth are activated
           if(input$lsmooth) {
            ggplot(aes_string(x=input$xvar,y=input$yvar,color="gender"), data=fev_DF ) + 
              geom_point(alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint)) +
              geom_smooth(method="loess", se=FALSE)
           } else {
             
             #plot if color gender is activated
             ggplot() + 
               geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"), alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint), data=fev_DF)
           }
         }
         
       } else {
         if(input$facet) {
           
           #plot if facet and loess smooth are activated
           if(input$lsmooth) {
             ggplot(aes_string(x=input$xvar,y=input$yvar), data = fev_DF) + 
               geom_point( alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint)) +
               geom_smooth(method="loess", se=FALSE) +
               facet_wrap( ~ ind.Male)
           } else {
             
             #plot if facet is activated
             ggplot(aes_string(x=input$xvar,y=input$yvar), data = fev_DF) + 
               geom_point( alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint)) +
               facet_wrap( ~ ind.Male)
           }
          }else {
            
            #plot if loess smooth is activated
            if(input$lsmooth) {
             ggplot(aes_string(x=input$xvar,y=input$yvar), data=fev_DF) + 
               geom_point( alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint, height = 0)) +
               geom_smooth(method="loess", se=FALSE)
            } else {
              
              #plot if nothing is activated
              ggplot() + 
                geom_point(aes_string(x=input$xvar,y=input$yvar), alpha = input$myalpha, size = input$mysize, position = position_jitter(width = input$jpoint, height = 0), data=fev_DF)
            }
          }
       }
    })
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)