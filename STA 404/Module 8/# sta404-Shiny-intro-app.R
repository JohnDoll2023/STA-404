# sta404-Shiny-intro-app.R
# DIR: ../STA404class
# Acknowledgments: 
#    Short Courses / Webinars - G. Grolemund, M. Cetinkaya-Rundel
#    Discussions with Karsten Maurer
#       related to framing Shiny apps

# ............................................................
# Quick look at a Shiny app (and the web page behind it)
# http://dataviz.miamioh.edu/Butler_County_Overdose_Deaths/
# (inspect what is going on in a web page)

# ............................................................
# R concept useful in Shiny development
#   functions (also passing parameters)
#   lists 
#   conditional execution as preliminary

# Example 0:  We are always using functions in data viz and R
rnorm(10,50,10)             # you can pass function parameters
#    by position
rnorm(mean=50, n=10, sd=1)  # of by name

# Example 1:  Coefficient of Variation
#             relative measure of variability = sd/mean

CoeffVar <- function(x) {}   # basic function

CoeffVar <- function(x) {
  sd(x)/mean(x)
}
CoeffVar(rnorm(n=100, mean=50, sd=10))

# Example 2:  Coefficient of Variation
CoeffVar2 <- function(x) {
  std.dev = sd(x)
  xbar = mean(x)
  cv = std.dev/xbar
  list(Sample = x, SD = std.dev, XBAR = xbar, CV = cv)
}
CV.RES <- CoeffVar2(rnorm(n=100, mean=50, sd=10))

str(CV.RES)   # a list is returned from the function

CV.RES$CV     # you can extract components from a list
CV.RES$Sample[1:2]

# ............................................................


#
# dynamic viz - animation is common example, not much user control (play/pause)
# interactive viz - handle over user control, buttons for control, tool tips
#
# Shiny package - Java script translator (from R code to Javascript)
#   other tools - d3.js; Javascript
#
# Caution:  Shiny has steep learning curve
#
# Check for unbalanced () or {}
# RStudio does nice indenting of nested code
#  
# MOTIVATION (on board)
# - consider 'fev' data
# - sketch scatterplot and color gender 
# - describe user interface - drop-down menu to select X and Y variable
#                             check box to select 'color by gender'
#
# SHINY
# app.R 
#   ui: user interface function 
#   server.: server side function, where all of the 'thinking' occurs
#   (can also have 2 scripts ui.R, server.R)
#   (more of an older Shiny style)
#
# 'listener' loop where where an input object is changed,
#    then information gets sent to the server for action and display
#    sent to user 
#

# in RStudio ...
#   File > New File > Shiny Web App ...



# building to iris Shiny app...
# Server side
# - read FEV data and construct data frame 
# - myplot <= ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
#                 geom_point()
#   OR
# - myplot <= ggplot() + 
#                 geom_point(data=fev_DF,aes(x=age.yrs, y=ht.in))
#
# INPUT of x and y ==> aes(x= , y= )
# INPUT of color checkbox ==> if checkbox clicked
# ** All changeable elements need to be stored as an INPUT list!
#
# need object for storing INPUT information ==> INPUT LIST
# input <- list(xvar="age.yrs", yvar="ht.in", color_gender=TRUE)

library(tidyverse)
fev_DF <- read.table("/home/baileraj/sta404/Data/fev_data.txt", header=T)


input <- list(xvar="age.yrs", yvar="ht.in", color_gender=TRUE)
input
input$xvar

# put together plot with list elements specified BEFORE building app

# build plot first ....................................
library(ggplot2)
ggplot() +
  geom_point(aes(x=age.yrs,y=ht.in,color=ind.Male),data=fev_DF)

# fixing the color scheme?
fev_DF <- fev_DF %>% 
  mutate(gender = ifelse(ind.Male==0,"Female","Male"))

ggplot() +
  geom_point(aes(x=age.yrs,y=ht.in,color=gender),data=fev_DF)


# replace with list ....................................
input <- list(xvar="age.yrs", yvar="ht.in", color_gender=TRUE)
ggplot() +
  geom_point(aes(x=input$xvar,y=input$yvar,color=color_gender),data=fev_DF)

ggplot() +
  geom_point(aes(x=input$xvar,y=input$yvar),data=fev_DF)
# doesn't work! x and y are interpreted as character variable

ggplot() +
  geom_point(aes(x=input$xvar,y=input$yvar,color='green'),data=fev_DF)

# need to recognize as a variable name NOT value - need aes_string .....
ggplot() +
  geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"),data=fev_DF)

# if change input
input <- list(xvar="age.yrs", yvar="fev.L", color_gender=TRUE)
ggplot() +
  geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"),data=fev_DF)

# species part is a little tricky - how do we build that layer
# OPTION 1 if - then ... 

input <- list(xvar="age.yrs", yvar="ht.in", color_gender=TRUE)
if(input$color_gender) {
  ggplot() + 
    geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"),
               data=fev_DF)
} else {
  ggplot() + 
    geom_point(aes_string(x=input$xvar,y=input$yvar),data=fev_DF)
}

# change to FALSE
input <- list(xvar="age.yrs", yvar="ht.in", color_gender=FALSE)

if(input$color_gender) {
  ggplot() + 
    geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"),
               data=fev_DF)
} else {
  ggplot() + 
    geom_point(aes_string(x=input$xvar,y=input$yvar),data=fev_DF)
}


# OPTION 2 ifelse ...
input <- list(xvar="age.yrs", yvar="ht.in", color_gender=TRUE)
#input <- list(xvar="age.yrs", yvar="ht.in", color_gender=FALSE)

ifelse(input$color_gender,
       p1 <- ggplot() + 
         geom_point(aes_string(x=input$xvar,y=input$yvar,color="gender"),
                    data=fev_DF),
       p1 <- ggplot() + 
         geom_point(aes_string(x=input$xvar,y=input$yvar),data=fev_DF)
)
p1

# now more to app.R to build this!

# SUGGESTION:  good idea to have one folder per app you build

# SUGGESTION:  make one change, see if Shiny app still works


# A shiny app always needs shiny
library(shiny)

# Load any support libraries and data outside of shinyServer function
library(tidyverse)

### Define UI for application
# fluidPage = web site itself, sets up page
# sidebarLayout = cuts up page for controls in one section, output in other
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

#
#  open app1.R
