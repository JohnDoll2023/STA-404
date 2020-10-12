library(tidyverse)

ohioDF <- read_csv("https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")

head(ohioDF)
tail(ohioDF)
glimpse(ohioDF)

# remove Total row from file
ohioDF <- ohioDF %>% 
  filter(Sex != "Total")

tail(ohioDF)
head(ohioDF)

# need to get date function
library(lubridate)
ohioDF <- ohioDF %>% 
  mutate(AgeFactor = factor(`Age Range`),
         OnsetDate = mdy(`Onset Date`))

# info about day I ran this

Today <- Sys.Date()

#view(ohioDF)

ohiocountyDF <- ohioDF %>% 
  group_by(County, OnsetDate) %>% 
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`, na.rm = TRUE))

dim(ohioDF)
dim(ohiocountyDF)
head(ohiocountyDF)
tail(ohiocountyDF)

butlerDF <- ohiocountyDF %>% 
  filter(County == "Butler")

unique(butlerDF$County)
totalCases <- sum(butlerDF$ncases)


head(butlerDF)
tail(butlerDF)
totalCases
totalDead <- sum(butlerDF$ndead)
totalDead
firstCase <- min(butlerDF$OnsetDate)
lastCase <- max(butlerDF$OnsetDate)

totalDead
firstCase
lastCase

ggplot(butlerDF, aes(x = OnsetDate, y = ncases)) +
  geom_col(color = "grey") +
  labs(x = "Onset Date", y = "Number of New Cases", title = "Butler County Case Counts", subtitle = paste("Updated: ", Today, source = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")) +
  theme_minimal() +
  annotate(geom = "text", x = date("2020-02-27"), y = 125, label = paste("Total Cases: ", totalCases), hjust = 0)

ggplot(butlerDF, aes(x = OnsetDate, y = ncases)) +
  geom_bar(stat = "identity") 

#Add annotation text for total dead, first case, last case
#find geom that adds moving averages

#brings in moving averages function
library(tidyquant)

#annotations and moving average included
ggplot(butlerDF, aes(x = OnsetDate, y = ncases)) +
  geom_col(color = "grey") +
  labs(x = "Onset Date", y = "Number of New Cases", title = "Butler County Case Counts", subtitle = paste("Updated: ", Today, source = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")) +
  theme_minimal() +
  annotate(geom = "text", x = date("2020-02-27"), y = 125, label = paste("Total Cases: ", totalCases), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 115, label = paste("Total Dead: ", totalDead), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 105, label = paste("First Case: ", firstCase), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 95, label = paste("Last Case: ", lastCase), hjust = 0) +
  geom_ma(ma_fun = SMA, n = 7)

#source https://www.rdocumentation.org/packages/tidyquant/versions/1.0.1

ggplot(butlerDF, aes(x = OnsetDate, y = ncases)) +
  geom_col(color = "grey") +
  labs(x = "Onset Date", y = "Number of New Cases", title = "Butler County Case Counts", subtitle = paste("Updated: ", Today, source = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")) +
  theme_minimal() +
  annotate(geom = "text", x = date("2020-02-27"), y = 125, label = paste("Total Cases: ", totalCases), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 115, label = paste("Total Dead: ", totalDead), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 105, label = paste("First Case: ", firstCase), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 95, label = paste("Last Case: ", lastCase), hjust = 0) +
  geom_ma(n = 7, linetype = 1, color = "blue", size = 1.25)

format(Sys.time(), "%a %b %d %X %Y %Z")

ggplot(butlerDF, aes(x = OnsetDate, y = ncases)) +
  geom_col(color = "grey") +
  labs(x = "Onset Date", y = "Number of New Cases", title = "Butler County Case Counts", subtitle = paste("Updated: ", Today, source = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")) +
  theme_minimal() +
  annotate(geom = "text", x = date("2020-02-27"), y = 125, label = paste("Total Cases: ", totalCases), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 115, label = paste("Total Dead: ", totalDead), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 105, label = paste("First Case: ", firstCase), hjust = 0) +
  annotate(geom = "text", x = date("2020-02-27"), y = 95, label = paste("Last Case: ", lastCase), hjust = 0) +
  geom_ma(n = 7, linetype = 1, color = "blue", size = 1.25) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")

OhioBC <- ohiocountyDF %>% 
  filter(County == "Butler") %>% 
  arrange(OnsetDate) %>% 
  mutate(CumulCase = cumsum(ncases))

ggplot(OhioBC, aes(x=OnsetDate, y=CumulCase)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d") +
  labs(x="Onset Date", y="Case Count",
       title="Figure 2. Cumulative Cases Reported to Butler County by Date Reported",
       subtitle=paste("Updated: ",Today),
       caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
  theme_minimal()

