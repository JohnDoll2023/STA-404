# sta404-R-categorical-data-BLANK.R
# Revised: 13 Sep 20
# Revision history: 26sep17 / 18sep19
# /home/baileraj/sta404

# References
# Wilke Ch 6 Visualizing Amounts; 
#       Ch 10 Visualizing Proportions; 
#       Ch 11 Visualizing Nested Proportions
# Chang - Ch 3 Bar Graphs, Ch 4 Line Graphs AND 13.6, 13.15, 13.16
# Yau - Ch. 5
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#
# Datasets
#   gapminder - data from gapminder package
#   myGapData = gapminder + TotalGDP
#   GDPsummaryDF = myGapData summarized into
#                year-continent values with
#                new variables ContinentTotalGDP, ncountries
#   GDPyearDF = myGapData summarized into
#                year values with new variable YearTotalGDP
#   GDPcombo = left join GDPsummary AND GDPyearDF
#                for continent specific visualization
#                new variables:
#                  PropWorldGDP, PctWorldGDP, ContGDPBillions


# CODE ALONG THROUGHOUT! .........

library(tidyverse)
library(gapminder)

# Questions to motivate our categorical data / proportional data display discussion
#  How is a country's GDP related to its population size?
#  What continent has the largest share of the world's GDP?
#  What country in each continent has the largest share of the continent's GDP?

# =====================================================================
# Challenge 1:  We don't have country GDP - only per capita GDP. Let's
#               construct this.

# What were the variables in the gapminder data frame?

names(gapminder)
str(gapminder)
glimpse(gapminder)
head(gapminder)

# Can you construct country GDP from these variables?
mpGapData <- gapminder %>% 
  mutate(TotalGDP = pop*gdpPercap) %>% 
  head()

# work with the GDP data - total GDP from pop*gdpPercapita

head(gapminder)

# construction total GDP for each country for data provided
myGapData <- gapminder %>%
  mutate(TotalGDP = pop*gdpPercap)

head(myGapData)

# Review:  Module 3 ideas
# How is country total GDP related to population size?

ggplot(data = myGapData, aes(x = pop, y = TotalGDP)) +
  geom_point()

# plot total GDP vs. population size - does it differ from per capita GDP vs pop?

ggplot(data = myGapData, aes(x = pop, y = TotalGDP)) +
  geom_point()

# change axes to log10

ggplot(data = myGapData, aes(x = pop, y = TotalGDP)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()


ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_point()

ggplot(myGapData, aes(x=pop,y=gdpPercap,color=continent)) +
  geom_point()

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_line(aes(group=country))

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_line(aes(group=country)) +
  scale_x_log10() +
  scale_y_log10()

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_line(aes(group=country)) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(vars(continent))

#workflow sequence
# scatterplot
# log10 axes
# changed geom
#separate lines per country
#dividing plot into strata

#arrange/summarize

myGapData %>% 
  arrange(TotalGDP)

myGapData %>%
  arrange(desc(TotalGDP)) %>%
  select(country, continent, TotalGDP, year)

# look population trends in a particular country over time .................
# e.g. Somalia

myGapData %>% 
  filter(country == "Somalia") %>% 
  ggplot(aes(x=year, y=pop)) +
  geom_point() +
  # geom_smooth(method = "lm", se = F) +
  # geom_smooth(method = "loess", se = F, color = "red")



myGapData %>%
  filter(country=="Somalia")  %>%
  ggplot(aes(x=year,y=pop)) +
  geom_point()

myGapData %>%
  filter(country=="United States")  %>%
  ggplot(aes(x=year,y=pop)) +
  geom_point()

str(myGapData)

# consider all countries with a facet by year ..............


ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_point() +
  facet_wrap(~year)

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_line(aes(group = country)) +
  facet_wrap(~year)

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_point() +
  facet_wrap(~year) +
  scale_x_log10() +
  scale_y_log10()

ggplot(myGapData, aes(x=log2(pop),
                      y=log2(TotalGDP),
                      color=continent)) +
  geom_point() +
  facet_wrap(~year)


# let's clean this up ....................

# look at summary of the data frame to get a sense of the
#  size of values of each of the variables.

summary(myGapData)

ggplot(myGapData, aes(x=pop,y=TotalGDP,color=continent)) +
  geom_point() +
  facet_wrap(~year) +
  scale_x_log10(name="Population Size (Millions)",
                breaks=1000000*c(1,10,100),
                labels=c("1","10","100")) +
  scale_y_log10(name="Gross Domestic Product (Billions)",
                breaks=1000000000*c(1,100,1000),
                labels=c("1000","10000","100000"))

# side question:  if a log-log relationship is linear, what does that
#  suggest about the relationship between the original variables?

#Y=a*x^b => log(Y) = lob(a) + b*log(x) <- power law

# =====================================================================
# Challenge 2:  how can we determine total annual continent and world GDP?

# calculate continent-specific annual GDP ...................

GDPsummaryDF <- myGapData %>%
  group_by(continent, year) %>%
  summarise(ContinentTotalGDP = sum(TotalGDP),
            ncountries = n())

head(GDPsummaryDF)
tail(GDPsummaryDF)

# calculate world annual GDP ................................

GDPyearDF <- myGapData %>%
  group_by(year) %>%
  summarise(YearTotalGDP = sum(TotalGDP))
head(GDPsummaryDF)

head(GDPyearDF)

# how do you combine the two data sets so that the annual world GDP is
#    associated with each continent
# combine the table with continent annual GDP with the world annual GDP

GDPcombo <- left_join(GDPsummaryDF, GDPyearDF,
                      by="year")  # what does left_join do?

head(GDPcombo)
tail(GDPcombo)

GDPcombo %>%
  filter(year==1977)

# 
# calculate the proportion of the world GDP associated with each continent in each year

GDPcombo <- GDPcombo %>% mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
                                PctWorldGDP = 100*PropWorldGDP,
                                ContGDPBillions = ContinentTotalGDP/1000000000)
head(GDPcombo)

# =====================================================================
# Challenge 3:  Display continent-specific GDP over time?

# Start with a single year ...................................
# Start with looking at the 2007 data ........................

gapminder2007 <- GDPcombo %>%
  filter(year==2007)

# display of the number of countries in each continent

ggplot(gapminder2007, aes(x=ncountries, y=ncountries, color=continent)) +
  geom_bar()   # what happened here?

ggplot(gapminder2007, aes(x=ncountries, y=ncountries, color=continent)) +
  geom_bar(stat="identity")   # how about here?

ggplot(gapminder2007, aes(x=factor(ncountries), y=ncountries, fill=continent)) +
  geom_bar(stat="identity")  

# recent release of ggplot2 [ca 2019] include geom_col() for bar graph construction
ggplot(gapminder2007, aes(x=factor(ncountries), y=ncountries, fill=continent)) +
  geom_col()  

ggplot(gapminder2007, aes(x=continent, y=ncountries, fill=continent)) +
  geom_col()

library(forcats) # https://forcats.tidyverse.org/
ggplot(gapminder2007, aes(x=fct_reorder(continent,ncountries),
                          y=ncountries, fill=continent)) +
  geom_col() +
  coord_flip() +
  labs(y="Number of Countries", x="") +
  guides(fill = FALSE)


# informal assignment:  generate a better bar chart for these data
#     can you get rid of the continent legend?
# Ref: https://stackoverflow.com/questions/35618260/remove-legend-ggplot-2-2


# consider:  is color needed?
myGapData %>%
filter(country=="Somalia")  %>%
  ggplot(aes(x=year,y=pop)) +
  geom_point()


ggplot(gapminder2007, aes(x=fct_reorder(continent,ncountries),
                          y=ncountries, fill=continent)) +
  geom_col(fill="grey") +
  ylab("Number of Countries") +
  xlab("") +
  coord_flip() +
  #  theme(legend.position="none")   # remove legend
  theme_minimal()

## visualizing GDP

ggplot(gapminder2007, aes(x=continent,y=ContinentTotalGDP,fill=continent)) +
  geom_bar(stat="identity")  

ggplot(gapminder2007, aes(x=continent,y=ContinentTotalGDP,fill=continent)) +
  geom_bar(stat="identity") +
  theme(legend.position = "top")

# change order of bars - can do with factor orderings
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
# https://blog.rstudio.com/2016/08/31/forcats-0-1-0/
#
gapminder2007 <- gapminder2007 %>%
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

with(gapminder2007, table(continent, order_continent))  # silly check of recoding?


ggplot(gapminder2007, aes(x=order_continent,y=ContinentTotalGDP,
                          fill=order_continent)) +
  geom_bar(stat="identity") +
  theme(legend.position = "top")

# highlight Asia
gapminder2007 <- gapminder2007 %>%
  mutate(indAsia = ifelse(continent=="Asia",1,0))

with(gapminder2007, table(continent, indAsia))

View(gapminder2007)

ggplot(gapminder2007, aes(x=order_continent,y=ContinentTotalGDP,
                          fill=indAsia)) +
  geom_bar(stat="identity") +
  theme(legend.position = "top")

ggplot(gapminder2007, aes(x=order_continent,
                          y=ContinentTotalGDP,
                          fill=as.factor(indAsia))) +
  geom_bar(stat="identity") +
  theme(legend.position = "top")

# changing colors?

p <- ggplot(gapminder2007, aes(x=order_continent,
                               y=ContinentTotalGDP,
                               fill=as.factor(indAsia))) +
  geom_bar(stat="identity") +
  theme(legend.position = "top")

cols <- c("0" = "lightgrey", "1" = "blue" )
p + scale_fill_manual(values = cols) +
  guides(fill=FALSE) +
  coord_flip() +
  theme_minimal()


# alternative using the forcat::fct_reorder function
# library(forcats)
#
# ggplot(gapminder2007, aes(x=fct_reorder(continent,ContinentTotalGDP),
#                           y=ContinentTotalGDP)) +
#   geom_bar(stat="identity")

# Now, let's look over time ...................................

GDPcombo <- GDPcombo %>%
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

# default is to STACK this response
ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity") +
  theme(legend.position = "top")

# you can DODGE the position of this response

ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity",position="dodge") +
  theme(legend.position = "top")

# you can FILL the position of this response

ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity",position="fill") +
  theme(legend.position = "top")

ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity",position="fill") +
  theme(legend.position = "top") +
  coord_flip()

# reverse the year order? [01 oct 2019]
ggplot(GDPcombo, aes(x=desc(year),
                     y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity",position="fill") +
  theme(legend.position = "top") +
  coord_flip()

ggplot(GDPcombo, aes(x=fct_rev(as.factor(year)),
                     y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity",position="fill") +
  theme(legend.position = "top") +
  xlab("") +
  coord_flip()



# factor recoding issue
yr <- seq(from=1950,to=2000,by=10)
yr1 <- yr[-3]

yr1also <- factor(yr1, levels=c("1950","1960","1970","1980","1990","2000"))

yr
yr1
yr1also

# which do you like best?  why?
# what else might you like to change?

# pie charts are a variant of a fill
#  pie chart = stacked bar chart + polar coordinates
# http://ggplot2.tidyverse.org/reference/coord_polar.html

ggplot(gapminder2007, aes(x="1",y=ContinentTotalGDP,
                          fill=order_continent)) +
  geom_bar(stat="identity", width=1) +
  theme(legend.position = "top") +
  coord_polar(theta="y")

# using geom_col [2019]
ggplot(gapminder2007, aes(x="1",y=ContinentTotalGDP,
                          fill=order_continent)) +
  geom_col() +
  theme(legend.position = "top") +
  coord_polar(theta="y")

# if use 'fill' then you have proportions in wedges
ggplot(gapminder2007, aes(x="1",y=ContinentTotalGDP,
                          fill=order_continent)) +
  geom_bar(stat="identity", position="fill", width=1) +
  theme(legend.position = "top") +
  coord_polar(theta="y")

# Pie charts showing change in share of world GDP over time
ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity", width=1) +
  theme(legend.position = "top") +
  coord_polar(theta="y") +
  facet_wrap(~year)

# but want to look at 'share' within year ...
ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity", width=1, position="fill") +
  #  theme(legend.position = "top") +
  coord_polar(theta="y") +
  facet_wrap(~year)

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity", width=1, position="fill") +
  #  theme(legend.position = "top") +
  coord_polar(theta="y") +
  facet_wrap(~year) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())

# what if you want to add components of theme_minimal?

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity", width=1, position="fill") +
  #  theme(legend.position = "top") +
  coord_polar(theta="y") +
  facet_wrap(~year) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank()) +
  theme_minimal()

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity", width=1, position="fill") +
  coord_polar(theta="y") +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())  

# modifying the colors a little
# using scale_brewer
#       Color Brewer provides color schemes and advice
# REF:  http://colorbrewer2.org/

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity", width=1, position="fill") +
  scale_fill_brewer(type="qual", palette="Pastel1") +
  coord_polar(theta="y") +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())  

# Design advice for designs addressing color blind viewers ...
# https://99designs.com/blog/tips/designers-need-to-understand-color-blindness/

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent)) +
  geom_bar(stat="identity", width=1, position="fill") +
  scale_fill_brewer(type="seq", palette="Blues") +
  coord_polar(theta="y") +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())  

# get help for scale_fill_brewer and then modify
# this plot to consider other palettes - which do
# you like the best?

ggplot(GDPcombo, aes(x=factor("1"),y=ContinentTotalGDP,
                     fill=order_continent),color="black") +
  geom_bar(stat="identity", width=1, position="fill") +
  #  scale_fill_brewer(type="seq", palette="Oranges") +
  scale_fill_brewer(type="qual",palette="Set2") +
  coord_polar(theta="y") +
  facet_wrap(~year) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())  

# Cleveland Dot Plot - Chang 3.10 ==============================================
library(forcats)

GDPcombo %>%
  filter(year==2007) %>%
  ggplot(aes(ContinentTotalGDP, continent)) + geom_point()

GDPcombo %>%
  filter(year==2007) %>%
  ggplot(aes(ContinentTotalGDP,
             fct_reorder(continent, ContinentTotalGDP))) +
  geom_point() +
  coord_flip()

# clean up axes
GDPcombo %>%
  filter(year==2007) %>%
  ggplot(aes(ContinentTotalGDP, fct_reorder(continent, ContinentTotalGDP))) +
  geom_point() +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Total GDP ($Trillion))",
                     breaks=1000000000000*c(5,10,15,20),
                     labels = c("5","10","15","20"))+
  labs(title="Total GDP in each continent in 2007",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_bw()

# alternative where you don't need to modify the factor

GDPcombo %>%
  filter(year==2007) %>%
  ggplot(aes(ContinentTotalGDP, reorder(continent, ContinentTotalGDP))) +
  geom_point() +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Total GDP ($Trillion)",
                     breaks=1000000000000*c(5,10,15,20),
                     labels = c("5","10","15","20"))+
  labs(title="Total GDP in each continent in 2007",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_bw()

# Cairo likes a version with a segment to the name
#  - he calls this a lollipop plot

lp_graph <- GDPcombo %>%
  filter(year==2007) %>%
  ggplot(aes(ContinentTotalGDP, fct_reorder(continent, ContinentTotalGDP))) +
  geom_point(size=3) +
  geom_segment(aes(yend=continent), xend=0) +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Total GDP ($Trillion))",
                     breaks=1000000000000*c(5,10,15,20),
                     labels = c("5","10","15","20"))+
  labs(title="Total GDP in each continent in 2007",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_bw()

lp_graph

lp_graph + coord_flip()

lp_graph + coord_flip() + theme_minimal()

# countries within a continent .........................

myGapData %>%
  filter(year==2007) %>%
  ggplot(aes(TotalGDP, country)) + geom_point()

myGapData %>%
  filter(year==2007) %>%
  ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) +
  geom_point() +
  facet_grid(continent ~ .) +
  theme(text = element_text(6))

# need to modify y axis text size

myGapData %>%
  filter(year==2007) %>%
  ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) +
  geom_point() +
  facet_wrap(~ continent, scales="free_y") +
  theme(text = element_text(6),
        axis.text.y = element_text(6)) +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Total GDP ($Trillion))",
                     breaks=1000000000000*c(5,10,15,20),
                     labels = c("5","10","15","20"))+
  labs(title="Total GDP in each continent in 2007",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_bw() +
  theme(axis.text.y=element_text(size=6))

# better to restrict to a particular continent ....
myGapData %>%
  filter(continent=="Asia") %>%
  filter(year==2007) %>%
  ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) +
  geom_point()

myGapData %>%
  filter(continent=="Asia") %>%
  filter(year==2007) %>%
  ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) +
  geom_point() +
  #  geom_segment(aes(yend=country), xend=0) +
  theme(text = element_text(6),
        axis.text.y = element_text(6)) +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Total GDP ($Trillion))",
                     breaks=1000000000000*c(5,10,15,20),
                     labels = c("5","10","15","20"))+
  labs(title="Total GDP in each continent in 2007 in Asia",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_bw() +
  theme(axis.text.y=element_text(size=9))

# compare to per capita GDP

myGapData %>%
  filter(continent=="Asia") %>%
  filter(year==2007) %>%
  ggplot(aes(gdpPercap, fct_reorder(country,gdpPercap))) +
  geom_point() +
  theme(text = element_text(6),
        axis.text.y = element_text(6)) +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Per Capita GDP (USD)",
                     breaks=c(1000,10000,20000,40000),
                     labels = c("1000","10000","20000","40000")) +
  labs(title="Per capita GDP in each continent in 2007 in Asia",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_bw() +
  theme(axis.text.y=element_text(size=9))

# IN CLASS:  
#    Modify this code block to produce similar displays
#    for Africa and for the Americas

# =====================================================================
# Stacked area chart
# Chang 4.7

# error in previous code - used YearTotalGDP vs. ContinentTotalGDP!
# ggplot(GDPcombo, aes(x=year, y=YearTotalGDP, fill=continent)) +
#   geom_area()

# corrected from here and below
ggplot(GDPcombo, aes(x=year, y=ContinentTotalGDP, fill=continent)) +
  geom_area()



# IN CLASS - modify this code to remove the legend title,
#     to change the color scheme using color brewer and
#     to improve the axis labels

ggplot(GDPcombo, aes(x=year, y=ContinentTotalGDP, fill=continent)) +
  geom_area() +
  scale_fill_brewer(type="seq", palette="Oranges") +
  scale_y_continuous(name="Annual GDP (Trillion USD)",
                     breaks=100000000000000*c(1,2,3),
                     labels = c("100","200","300")) +
  theme_minimal() +
  theme(legend.title = element_blank())  

# How has the share of the world's economy changed between
#    1952 and 2007?
# Ref:  Chang 4.8:   Proportional Stacked Area Graph

# proportion of world economy over time ..................

ggplot(GDPcombo, aes(x=year, y=PropWorldGDP, fill=continent)) +
  geom_area()

library(forcats)
ggplot(GDPcombo, aes(x=year, y=PropWorldGDP, fill=fct_reorder(continent,PropWorldGDP))) +
  geom_area()

# clean this up
ggplot(GDPcombo, aes(x=year, y=PropWorldGDP, fill=fct_reorder(continent,PropWorldGDP))) +
  geom_area() +
  scale_fill_brewer(type="qual") +
  theme_minimal() +
  theme(legend.title = element_blank())


#to get to same line number as bailer




























# move text into figure and clean up some more
library(ggthemes)

ggplot(GDPcombo, aes(x=year, y=PropWorldGDP, fill=fct_reorder(continent,PropWorldGDP))) +
  geom_area() +
  scale_fill_brewer(type="qual") +
  scale_x_continuous(breaks = c(1960,1970,1980,1990,2000),
                     labels = paste(c(1960,1970,1980,1990,2000))) +
  scale_y_continuous(breaks=c(.25,.50,.75,1),
                     labels=c("25%","50%","75%","100%")) +
  annotate(geom="text",x=1980,y=.23,label="Americas",color="white") +
  annotate(geom="text",x=1990,y=.62,label="Europe",color="darkblue") +
  annotate(geom="text",x=2000,y=.87,label="Asia",color="darkblue") +
  annotate(geom="text",x=1990,y=.96,label="Africa",color="white") +
  annotate(geom="text",x=1980,y=.99,label="Oceania",color="white") +
  guides(fill=FALSE) +
  labs(title="Continent Share of World GDP",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_fivethirtyeight()

# exploring some themes from the ggthemes package
# theme_few()
# theme_tufte()
# theme_wsj()

GDPcombo %>% 
  filter(year==1952 | year==2007) %>% 
  ggplot(aes(x=year, y=PropWorldGDP, color=continent)) +
  geom_line(size=2) + 
  theme_minimal()

# 1. Move continent names to graph
# 2. Identify 1952 and 2007 on x-axis
# 3. Check y-axis label

# heat maps ... 
str(GDPcombo)

ggplot(GDPcombo, aes(x=year, y=continent, fill=PropWorldGDP)) +
  geom_tile() 

ggplot(GDPcombo, aes(x=year, y=continent, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightblue", high="darkblue")

ggplot(GDPcombo, aes(x=year, y=continent, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  coord_flip()

# clean-up factor levels

GDPcombo <- GDPcombo %>% 
  mutate(n.cont = factor(continent, 
                         levels = c("Oceania", "Africa", "Asia", "Europe","Americas")))

with(GDPcombo, table(n.cont, continent))

ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  coord_flip()

# redo the graphs
ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  guides(fill=FALSE) +
  labs(title="Continent Share of World GDP",
       subtitle="[<0.1 (light grey) to >0.4 (black)]",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_fivethirtyeight()

# reminder about the data frame used .................
GDPcombo %>% 
  group_by(year) %>% 
  summarize(total = sum(PropWorldGDP))

GDPcombo %>% 
  filter(year=="2007")

GDPcombo %>% 
  filter(year=="2002")

# class exercise ....................................

# Produce a heat map of proportion of GDP share 
#    associated with countries in Asia

library(treemapify)

gapminderPLUS <- gapminder %>% 
  mutate(GDP = pop*gdpPercap)

gapminder2007 <- gapminderPLUS %>% 
  filter(year == 2007)

ggplot(gapminder2007, aes(area = GDP, fill = continent, label = country, subgroup = continent)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, color = "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(color = "white", place = "topleft", reflow = T) +
  guides(fill = FALSE)

library(ggmosaic)

ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  coord_flip()

ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  guides(fill=FALSE) +
  labs(title="Continent Share of World GDP",
       subtitle="[<0.1 (light grey) to >0.4 (black)]",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_fivethirtyeight()

ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  guides(fill=FALSE) +
  annotate(geom = "text", x = 1980, y = "Americas", label = "Americas", color = "white") +
  annotate(geom = "text", x = 1980, y = "Europe", label = "Europe", color = "white") +
  annotate(geom = "text", x = 1980, y = "Asia", label = "Asia", color = "white") +
  annotate(geom = "text", x = 1980, y = "Africa", label = "Africa", color = "black") +
  annotate(geom = "text", x = 1980, y = "Oceania", label = "Oceania", color = "black") +
  labs(title="Continent Share of World GDP",
       subtitle="[<0.1 (light grey) to >0.4 (black)]",
       caption="Data source: Jennifer Bryan (2015) Gapminder package",
       x = "Year", y = "") +
  scale_x_continuous(breaks = c(1952, 2007)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  