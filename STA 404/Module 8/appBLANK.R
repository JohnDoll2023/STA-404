# appBLANK.R
# Source:  Karsten Maurer

# A shiny app always needs shiny
library(shiny)

# Load any support libraries and data outside of shinyServer function
library(tidyverse)

### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "My Shiny App Name Here!"),
  sidebarLayout(
    
    # Sidebar typically used to house input controls
    sidebarPanel(
      
    ),
    
    # Main panel typically used to display outputs
    mainPanel(

    )
    
  )
)


### Define server behavior for application here
server <- function(input, output) {
  
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)