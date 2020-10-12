# sta404-R-dashboard-data-BLANK.R
# Dell: C:\Users\John Bailer\Documents\Classes\STA404class
# updated:    04oct20
# created:    15oct19

# References
#
# Arranging multiple grobs on a page
#    https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# Arranging ggplot
#    https://github.com/baptiste/gridextra/wiki/arranging-ggplot
# 

library(tidyverse)
library(gapminder)

library(gridExtra)
library(ggmosaic)
library(ggrepel)
library(patchwork)

# A dashboard is often constructed with multiple graphs and other other information. These
# can be combined together is a single display.
#
# Q:  How does this differs from faceting?

# First rebuild the data frames for plotting ... 

myGapData <- gapminder %>% 
  mutate(TotalGDP = pop*gdpPercap) %>% 
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

GDPsummaryDF <- myGapData %>% 
  group_by(continent, year) %>% 
  summarise(ContinentTotalGDP = sum(TotalGDP), 
            ncountries = n())

GDPyearDF <- myGapData %>% 
  group_by(year) %>% 
  summarise(YearTotalGDP = sum(TotalGDP))

GDPcombo <- left_join(GDPsummaryDF, GDPyearDF, by="year")
GDPcombo <- GDPcombo %>% mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
                                PctWorldGDP = 100*PropWorldGDP,
                                ContGDPBillions = ContinentTotalGDP/1000000000)
GDPcombo <- GDPcombo %>% 
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

#
# next, generate the plots for diplaying - note that ggplot grob = graphical object can be saved
#

# trace of graph of individual countries with continent colors
g1_trace <- ggplot(myGapData, 
                   aes(x=pop,y=TotalGDP,color=order_continent)) +
  geom_line(aes(group=country)) +
  labs(x ="Population",
       y="GDP") +
  theme_minimal() +
  theme(legend.title = element_blank())
g1_trace
str(g1_trace)

# scatterplot with facets
g2_scatter <- ggplot(myGapData, aes(x=pop,y=TotalGDP,color=order_continent)) +
  geom_point() +
  facet_wrap(~year) +
  scale_x_log10(name="Population Size (Millions)",
                breaks=1000000*c(1,10,100),
                labels=c("1","10","100")) +
  scale_y_log10(name="Gross Domestic Product (Billions)",
                breaks=1000000000*c(1,100,1000),
                labels=c("1000","10000","100000")) + 
  theme_minimal() +
  theme(legend.title = element_blank())

# stacked bar graph
g3_stackbar <- ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                                    fill=order_continent)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Cleveland dot chart
g4_dot <- myGapData %>% 
  filter(continent=="Asia") %>% 
  filter(year==2007) %>% 
  ggplot(aes(TotalGDP, fct_reorder(country,TotalGDP))) + 
  geom_point() +
  theme(text = element_text(6),
        axis.text.y = element_text(6)) +
  scale_y_discrete(name=element_blank()) +
  scale_x_continuous(name="Total GDP ($Trillion))",
                     breaks=1000000000000*c(5,10,15,20),
                     labels = c("5","10","15","20"))+
  labs(title="Total GDP in each continent in 2007 in Asia",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_minimal() +
  theme(axis.text.y=element_text(size=9))

# stacked area graph
g5_stackarea <- ggplot(GDPcombo, 
                       aes(x=year, y=ContinentTotalGDP, 
                           fill=order_continent)) +
  geom_area() + 
  theme_minimal() +
  theme(legend.title = element_blank())

# TRY THIS:  rewrite this code using function from forcats package

# grid.arrange = "rectangular layouts with non-overlapping 
#                 cells" - baptiste / gridextra
# how can we place these displays on the same graph? ==========================
# g1_trace, g2_scatter, g3_stackbar, g4_dot, g5_stackarea
#
#
# A little history of R graphics .........................
# Wickham H, Navarro D and Pedersen TL 
# https://ggplot2-book.org/introduction.html#how-does-ggplot2-fit-in-with-other-r-graphics
#   online version of work-in-progress 3rd edition of ggplot2
# base graphics (ca 1983) - 'pen on paper model' - 'draw on top of plot,
#      can't modify or delete existing content'
# grid graphics (ca 2000) - 'grid grobs (graphical objects)
#      can be represented independently of the plot and can
#      be modified later ... system of viewports for layout of
#      complex plots
# lattice (ca 2008) := grid graphics to implement 
#      trellis (ca 1983) - easier to produce conditioning plots
# ggplot2 (ca 2005) compact syntax and grammar for producing plots
# ggvis (ca 2014) similar syntax to ggplot2
# htmlwidgets () accessing web viz tools from R (leaflet,
#     dygraph, networkD3)



# parameters of the grid.arrange function?
#
?grid.arrange



grid.arrange(g1_trace, g2_scatter, g3_stackbar, 
             g4_dot, g5_stackarea)

grid.arrange(g2_scatter, g3_stackbar, g4_dot, g5_stackarea)

grid.arrange(g5_stackarea, g4_dot)

grid.arrange(g5_stackarea, g4_dot, nrow = 1)

# can change widths ===========================================================

grid.arrange(g1_trace, g2_scatter, g3_stackbar, 
             g4_dot, 
             g5_stackarea,
             widths=c(1,2))

# and heights
grid.arrange(g1_trace, g2_scatter, g3_stackbar, 
             g4_dot, g5_stackarea,
             widths=c(1,2),heights=c(1,2,2))

# can change layout ===========================================================
#                   NA = placeholder
grid.arrange(g1_trace, g2_scatter, g3_stackbar, 
             g4_dot, g5_stackarea,
             widths=c(1,2),heights=c(1,2,2),
             layout_matrix = rbind(c(1,NA),
                                   c(3,2),
                                   c(4,5)))


# .................................................
# patchwork - relatively new package for 
#             displaying 

# grid.arrange(g1_trace, g2_scatter, g3_stackbar, 
#              g4_dot, g5_stackarea)

library(patchwork)

# 3 plots in 1 row of 3 columns ..................
g1_trace + g2_scatter + g3_stackbar

# ... collect legends together ...................
g1_trace + g2_scatter + g3_stackbar + 
  plot_layout(guides = 'collect')

# 3 plots in 3 rows in 1 column ..................
g1_trace / g2_scatter / g3_stackbar

# 4 plots in 2 rows of 2..............................  
(g1_trace + g2_scatter) / (g3_stackbar + g5_stackarea)

# assign the same amount of space to each plot by default, but this
# can be controlled with the `widths` and `heights` argument in `plot_layout()`.

# ============================================================================

# alternatives using grid viewports ==========================================
# ref:  baptiste / gridextra GitHub gridextra Wiki

library(grid)
pushViewport(viewport(layout=grid.layout(2,2)))
# vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)
print(g1_trace, vp=viewport(layout.pos.row=1, 
                            layout.pos.col=1))
print(g5_stackarea, vp=viewport(layout.pos.row=1, 
                                layout.pos.col=2))
print(g4_dot, vp=viewport(layout.pos.row=2, 
                          layout.pos.col=1:2))

# plot insets ================================================================
# http://ggplot2.tidyverse.org/reference/annotation_custom.html

g1_trace

summary(myGapData)
# pop Q1 = 2.8e+06
# TotalGDP = 5.9e+09

# construct inset plot
# For changing x or y axis limits without dropping data 
#         observations, see coord_cartesian
g1_trace +
  scale_x_continuous(limits=c(0,2.8e+06))+
  scale_y_continuous(limits=c(0,5.9e+09))

g1_trace +
  coord_cartesian(xlim=c(0,2.8e+06),ylim=c(0,5.9e+09))

g1_trace_inset <- g1_trace +
  coord_cartesian(xlim=c(0,2.8e+06),ylim=c(0,5.9e+09)) +
  guides(color=FALSE)

g <- ggplotGrob(g1_trace_inset)
g1_trace +
  annotation_custom(grob=g,
                    xmin=5e+08,xmax=11e+08, 
                    ymin=6e+12, ymax=12e+12) +
  theme(legend.title=element_blank())

#############################################################################
# Two final options for categorical data
# waffle plots - https://www.rdocumentation.org/packages/waffle/versions/0.7.0
# mosaic plots - https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html
library(waffle)
library(ggmosaic)

GDPshare07 <- GDPcombo %>%  
  filter(year==2007) %>% 
  select(continent,PctWorldGDP)

GDPshare07

waffle(parts=GDPshare07$PctWorldGDP,
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

pctGDP <- GDPshare07$PctWorldGDP 
names(pctGDP) <- as.vector(GDPshare07$continent)
pctGDP

waffle(parts=pctGDP,
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

# not quite correct ... 

pctGDP
round(pctGDP)

sum(pctGDP)
sum(round(pctGDP))

pctGDP2 <- pctGDP
pctGDP2[4] <- 26
rpctGDP2 <- round(pctGDP2)
waffle(parts=rpctGDP2,
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

# you may want to order this from the smallest to largest responses
#  use order() to do this
oo <- order(rpctGDP2)
waffle(parts=rpctGDP2[oo],
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

dd <- order(rpctGDP2, decreasing=TRUE)
waffle(parts=rpctGDP2[dd],
       xlab="1 sq == 1% world GDP in 2007",
       rows=10)

# ----------------------------------------------------------------

# Extra stuff:  What else can we do with contingency table data?
#               Mosaic plots let us look at contingency table data.

#
# mosaic plots - ggmosaic is one option with ggplot2 world
# vcd::mosaic is another 
# Chang 13.15 - categorical, contingency data visualization

# Basics of ggmosaic
#   designed to create visualizations of categorical data
#   can produce bar charts, stacked bar charts, mosaic plots, and double decker plots
#   plots are constructed hierarchically, so the ordering of the variables is very important.
#   integrated in ggplot2 as a geom which allows for facetting and layering
# from: https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html

# Limitation (also from page above)
#   ggplot2 is not capable of handling a variable number of variables
#   current solution: read in the variables x1 and x2 as x = product(x1, x2)
#   product function:
#      a wrapper function for a list
#   allows for it to pass check_aesthetics



# Aside: tables

library(forcats)
quantile(myGapData$gdpPercap,probs=c(0,.25,.75,1), na.rm=TRUE) #not as smart since we know there will be a trend over time

myGapData <- myGapData %>% 
  mutate(GDPpcQuart = ifelse(gdpPercap<1202.1,"Lowest 25%",
                              ifelse(gdpPercap < 9325.27,"Middle 50%",
                                      ifelse(gdpPercap < 113523.2,"Highest 25%","NA")))) %>% 
  mutate(GDPfact = factor(GDPpcQuart, levels = c("Lowest 25%","Middle 50%","Highest 25%")))

myGapData2007 <- myGapData %>% 
  filter(year == 2007)

QPG <- quantile(myGapData2007$gdpPercap,probs=c(0,.25,.75,1), na.rm=TRUE) #smarter since we know its only over a single year

myGap2007 <- myGapData2007 %>% 
  mutate(GDPpcQuart = ifelse(gdpPercap<QPG[2],"Lowest 25%",
                             ifelse(gdpPercap < QPG[3],"Middle 50%",
                                    ifelse(gdpPercap < QPG[4],"Highest 25%","NA")))) %>% 
  mutate(GDPfact = factor(GDPpcQuart, levels = c("Lowest 25%","Middle 50%","Highest 25%")))


# Table
table(myGap2007$GDPpcQuart)
table(myGap2007$GDPfact)       # look at the factor version of this

with(myGapData, table(year, continent, GDPfact))

GDP2007.table <- with(myGapData2007, table(continent, GDPfact))
GDP2007.table

GDP2007.df <- as.data.frame(GDP2007.table)
GDP2007.df

# what happens if we don't wrap continent below ...
ggplot() +
  geom_mosaic(data=GDP2007.df, 
              aes(weight=Freq, x=product(continent), fill=GDPfact))

ggplot(data=GDP2007.df) +
  geom_mosaic(aes(weight=Freq, x=product(continent), 
                  fill=GDPfact)) 
  

  
# Check out the Titanic data set
# from geom_mosaic help
data(Titanic)                       #  Titanic is a table, not data.frame 
Titanic                             #  Check out the object by typing its name

titanic <- as.data.frame(Titanic)   #  need to convert to data frame for ggplot-ting
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))

head(titanic)

ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived)) +
  labs(x="Class")

ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class, Age), fill=Survived)) +
  labs(y="Class",x="Age")

ggplot(data=titanic) +
  geom_mosaic(aes(weight=Freq, x=product(Class), 
                  conds=product(Age), fill=Survived))

# from geom_mosaic Help ...
# good practice: use the 'dependent' variable (or most important variable)
# as fill variable

# example from vcd::mosaic
library(vcd)

# example from mosaic help
mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, 
       legend = TRUE)

# extending example from mosaic help

mosaic(~ Class + Sex + Age + Survived, data=Titanic, 
       shade=TRUE)

# what does the ~ mean? 
# what is the p-value that is reported here?
# what is a Pearson residual?

