# module-07PLUS-plotly-animation.R
# 19 Oct 2020

#  https://plot.ly/ggplot2/
# "Plotly is an R package for creating interactive web-based graphs via the 
# open source JavaScript graphing library plotly.js. As of version 2.0 (November 17, 2015), 
# Plotly graphs are rendered locally through the htmlwidgets framework."

# Examples from previous class work ...
# sta404-R-exploring-relationships-BLANK-09sep17.R
# sta404-R-categorical-data-BLANK-14sep17.R

library(dplyr)
library(ggplot2)
library(plotly)

# example with relationships
fev_DF <- read.table("http://www.users.miamioh.edu/baileraj/classes/sta404/fev_data.txt",
                     header=T)
head(fev_DF)

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_point(alpha=.1)

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_bin2d()

p1 <- ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_point(alpha=.1)

p1c <- ggplot(fev_DF,aes(x=age.yrs, y=ht.in, 
                         color=as.factor(ind.Male))) + 
  geom_point(alpha=.1)

p1c

p2 <- ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_bin2d()

ggplotly(p1)

ggplotly(p1c)

fev_DF2 <- fev_DF %>% 
  mutate(Gender = ifelse(ind.Male==1,"Male","Female"))

table(fev_DF2$Gender, fev_DF2$ind.Male)

p1c2 <- ggplot(fev_DF2,aes(x=age.yrs, y=ht.in, 
                           color=Gender)) + 
  geom_point(alpha=.1)

ggplotly(p1c2)

str(fev_DF2)

# with maps =====================================================

library(maps)             # Chang 13.17
library(ggplot2)
library(ggmap)            # note citation('ggmap') if you use it
library(mapproj)
library(ggthemes)
library(plotly)

college.grad <- data.frame(statename=c("ohio","kentucky","indiana",
                                       "michigan","west virginia",
                                       "pennsylvania"),
                           rate=c(26.1, 22.3, 24.1, 26.9, 19.2, 28.6))

mw_map <- map_data("state", region=c("ohio","kentucky","indiana",
                                     "michigan","west virginia",
                                     "pennsylvania"))

grad_map <- merge(mw_map, college.grad, by.x="region", by.y="statename")

head(grad_map)

gm <- ggplot(grad_map, aes(x=long,y=lat, group=group, fill=rate)) +
  geom_polygon(colour="black") +
  coord_map("polyconic")

ggplotly(gm)

# example with gapminder ===========================================

library(gapminder)
myGapData <- gapminder %>% 
  mutate(TotalGDP = pop*gdpPercap)

# calculate continent-specific annual GDP ...................
GDPsummaryDF <- myGapData %>% 
  group_by(continent, year) %>% 
  summarise(ContinentTotalGDP = sum(TotalGDP), ncountries = n())

# calculate world annual GDP ................................

GDPyearDF <- myGapData %>% 
  group_by(year) %>% 
  summarise(YearTotalGDP = sum(TotalGDP))

# combine the table with continent annual GDP with the world annual GDP

GDPcombo <- left_join(GDPsummaryDF, GDPyearDF, by="year")

GDPcombo <- GDPcombo %>% mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
                                PctWorldGDP = 100*PropWorldGDP,
                                ContGDPBillions = ContinentTotalGDP/1000000000)

# extract 2007 data ......................................
gapminder2007 <- GDPcombo %>% 
  filter(year==2007)

# add factor to order continents ..........................
gapminder2007 <- gapminder2007 %>% 
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

GDPcombo <- GDPcombo %>% 
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

# STACK this response
ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "top")

plotGDP <- ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                                fill=order_continent)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "top")

plotGDP

ggplotly(plotGDP)

# recall: highlighting countries
# ================================================================================ggplot() +geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +guides(color="none")# create dataframe with Rwanda and Japan#   What is a data frame?#   What does it mean to pipe commands?#        Reading " %>% " as " and then "rwanda_japan <- gapminder %>%filter(country %in% c("Rwanda", "Japan"))rwanda_japanggplot() +geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +geom_line(data=rwanda_japan, aes(year,lifeExp, color=country), lwd=1.1) +theme_minimal()# cleaning up annotationsggplot() +geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +geom_line(data=rwanda_japan, aes(year,lifeExp, color=country), lwd=1.1) +guides(color="none") +labs(x="Year", y="Life Expectancy",caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version0.2.0.") +annotate("text", x=1985, y=80, label="Japan") +annotate("text", x=1985, y=30, label="Rwanda") +theme_minimal()

# revisiting the plot from module 1
library(gapminder)
library(tidyverse)

rwanda_japan <- gapminder %>%
  filter(country %in% c("Rwanda", "Japan"))

myplot <- ggplot() +
  geom_line(data=gapminder, 
            aes(x=year, y=lifeExp, group=country),
            color="lightgrey") +
  geom_line(data=rwanda_japan, 
            aes(year,lifeExp, color=country), 
            lwd=1.1) +
  guides(color="none") +
  labs(x="Year", y="Life Expectancy",
       caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version0.2.0.") +
  annotate("text", x=1985, y=80, label="Japan") +
  annotate("text", x=1985, y=30, label="Rwanda") +
  theme_minimal()

myplot
# what if we could do this interactively?


# 
# Interacting with existing ggplot object - PLOTLY
#
# REF: https://plotly.com/ggplot2/extending-ggplotly/
# REF: C. Sievert (2020) Interactive Web-Based Data
#        Visualization with R, plotly, and shiny
#        CRC Press
#
# Packages that are needed:
#    plotly
#    DT
#    crosstalk

library(plotly)
ggplotly(myplot)

# check out what you can do 
library(crosstalk)
library(DT)

# examples from Sievert or help
m <- highlight_key(mpg)
p <- ggplot(m, aes(displ, hwy)) + geom_point()
gg <- highlight(ggplotly(p), 
                on = "plotly_selected",
               off = "plotly_selected")
crosstalk::bscols(gg, DT::datatable(m))

d <- highlight_key(txhousing, ~city)
p <- ggplot(d, aes(date, median, 
                   group = city)) + 
  geom_line()
gg <- ggplotly(p, tooltip = "city") 
highlight(gg, dynamic = TRUE)

# apply example to Gapminder
m <- highlight_key(gapminder, ~country)
p <- ggplot(m, 
            aes(x=year, y=lifeExp, 
                group=country)) +
  geom_line()
gg <- ggplotly(p, tooltip = "country")
highlight(gg, dynamic = TRUE)

# possible to add table with values
gg1 <- highlight(gg, dynamic = TRUE)
crosstalk::bscols(gg1, DT::datatable(m))

#
# CLASS EXERCISE
# 
#
# 1. Read in country-continent data
#    from https://www.kaggle.com/statchaitya/country-to-continent
#   countryContinent.csv on Canvas
# 2. Create new data set by merging with 
#    gapminder to add continent
#    to each country
# 3. Generate a scatterplot for the 2007
#    data with 
#      color = continent
#      size = sqrt(population)
#      x = log10(income)
#      y = life expectancy
# 4. Wrap this is ggplotly() to query data
#      for countries on the plot





# .......................................
# animation can be another interesting 
#           tools

# packages needed:
#    gganimate
#    gifski


library(gapminder)
library(ggplot2)
library(gganimate)
library(gifski)

# Example 1: https://gganimate.com/ .....

ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

# Example 2: https://gganimate.com/ .....

library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# lots of other controls such as shadow trail
# https://gganimate.com/reference/shadow_trail.html

anim <- ggplot(airquality, aes(Day, Temp, colour = factor(Month))) +
  geom_point() +
  transition_time(Day)
anim

# Change distance between points
anim1 <- anim +
  shadow_trail(0.02)
anim1

# Style shadow differently
anim2 <- anim +
  shadow_trail(alpha = 0.3, shape = 2)

# Restrict the shadow to 10 frames
anim3 <- anim +
  shadow_trail(max_frames = 10)
anim3

# may want to add other aes() such as 
#               color=four_regions),

library(gapminder)
library(gganimate)

gapAnim <- ggplot(gapminder, aes(gdpPercap, lifeExp, 
                      size = pop, 
                      colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', 
       x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

gapAnim

# NOTE:
#   You can remove files from your 
#   directory using the file.remove() fcn
#   E.g. to delete all files with a pattern
# file.remove(dir(pattern="gganim_plot"))


#
# CLASS EXERCISE
# 
# 1. Create an animation with countries
#    colored by continent
# 2. Try at least one option in gganimate
#    that we haven't explored yet in class


