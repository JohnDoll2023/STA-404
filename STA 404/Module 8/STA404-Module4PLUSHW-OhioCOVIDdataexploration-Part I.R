# STA404-Module4PLUSHW-OhioCOVIDdataexploration-Part I.R
# Using Data from the Ohio COVID dashboard
# https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards/overview (Links to an external site.)
# CSV -  https://coronavirus.ohio.gov/static/COVIDSummaryData.csv (Links to an external site.)
# Data Dictionary - https://coronavirus.ohio.gov/static/docs/COVID-19-Data-Term-Definitions.pdf (Links to an external site.)
# 
# Dashboards -  https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards/overview (Links to an external site.)
# 
# 1. Create a plot where you show the moving average number of cases for all Ohio Counties in light grey and Butler County in blue
# 
# 2. Create a dot plot to show the number of cases age <=30 differs between counties. Order counties from largest number to smallest number. Remove 'Unknown' ages from data before displaying. What might explain the pattern you observed?
#   
# 3. Create another plot for these data that you think is interesting and generates insight.
# 
# NOTES:
#   
#   i) don't forget to include your code with comments or as part of a knitted Markdown file. In addition, I'm requesting either PDF or DOCX for your solutions since I can add comments directly to these documents in the Canvas grader.
# ii) Cite your code sources as comments in your program.
#iii) Comment and document your code as needed - I will include this when doing the grading.

library(tidyverse)
library(lubridate)

OhioDF <- read_csv(file="https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")
TODAY <- Sys.Date()

OhioDF <- OhioDF %>% 
  filter(Sex != "Total") %>% 
  mutate(AgeFactor = factor(`Age Range`),
         OnsetDate = mdy(`Onset Date`))

# data frame with counts by County and OnsetDate
OhioCountyDF <- OhioDF %>% 
  group_by(County, OnsetDate) %>% 
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`,
                        na.rm=TRUE)) 

# data frame with Blue County Data
BC_DF <- OhioCountyDF %>% 
  filter(County == "Butler") 

# 1. Create a plot where you show the moving average number 
#    of cases for all Ohio Counties in light grey and 
#    Butler County in blue
library(tidyquant)
ggplot() +
  labs(x="Onset Date", y="Number of New Cases",
       title="Ohio County Case Counts - 7d Moving Average",
       subtitle=paste("Updated: ",TODAY),
       caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
    geom_ma(data=OhioCountyDF, 
            aes(x=OnsetDate, y=ncases, group=County),
            color="lightgrey",
            n=7, linetype=1, size=1) +
    geom_ma(data=BC_DF, aes(x=OnsetDate, y=ncases),
            n=7, linetype=1, color="blue", size=1.25) +
    scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d") +
  theme_minimal()


# simple line graph .........................

G1 <- ggplot() +
  labs(x="Onset Date", y="Number of New Cases",
       title="Ohio County Case Counts",
       subtitle=paste("Updated: ",TODAY),
       caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
  geom_line(data=OhioCountyDF, 
          aes(x=OnsetDate, y=ncases, group=County),
          color="lightgrey",
          linetype=1, size=1) +
  geom_line(data=BC_DF, aes(x=OnsetDate, y=ncases),
          linetype=1, color="blue", size=1.25) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d") +
  coord_cartesian(ylim=c(0,400)) +
  theme_minimal()

G1

# geom_ma NOT yet implemented in package: plotly
library(plotly)
ggplotly(G1)




# 2. Create a dot plot to show the number of 
# cases age <=30 differs between counties. 
# Order counties from largest number to smallest 
# number. Remove 'Unknown' ages from data before 
# displaying. What might explain the pattern you 
# observed?

names(OhioDF)
table(OhioDF$AgeFactor)

OhioDF_noAgeUnk <- OhioDF %>% 
  filter(AgeFactor != "Unknown")

table(OhioDF_noAgeUnk$AgeFactor)

OhioDF_noAgeUnk <- OhioDF_noAgeUnk %>% 
  mutate(AgeGrp = ifelse(AgeFactor == "0-19" |
                           AgeFactor == "20-29",
                         1,0),
         AgeF = factor(AgeGrp, levels=c(1,0),
                      labels=c("Under 30",
                               "Over 30")))

table(OhioDF_noAgeUnk$AgeF)

OhioYoungCases <- OhioDF_noAgeUnk %>% 
  group_by(County, AgeF) %>% 
  summarize(ncases = sum(`Case Count`))

dim(OhioYoungCases)         

library(forcats)
ggplot(OhioYoungCases, 
       aes(y=fct_reorder(County,ncases), 
           x=ncases,
           col=AgeF)) +
  geom_point() + 
#  labs(y=NULL) +
  theme(legend.title = element_blank()) +
  theme_minimal()


#   
# 3. Create another plot for these data that you think is interesting and generates insight.
