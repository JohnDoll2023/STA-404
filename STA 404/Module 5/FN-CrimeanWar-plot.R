# FN-CrimeanWar-plot.R
# 03-Oct-2020 (updated)
# 26 Sept 2020

# REF: https://www.rdocumentation.org/packages/HistData/versions/0.8-6/topics/Nightingale

# grab the Crimean War Data that Florence Nightingale
#    made famous in her rose plot
#install.packages("HistData")

library(HistData)
data("Nightingale")

head(Nightingale)

# load other packages ..............................
library(tidyverse)

# remove the columns with rate data
#     note use of '-contains'
NightSub <- Nightingale %>%
  select(-contains(".rate"))

head(NightSub)

# need to convert to LONG format - separate
#    rows for Disease, Wounds, Other

NightSubLong <- pivot_longer(data=NightSub,
                             cols=c(Disease, Wounds, Other),
                             names_to = "Cause",
                             values_to = "Freq")

names(NightSubLong)
#[1] "Date"  "Month" "Year"  "Army"  "Cause" "Freq"

head(NightSubLong)

unique(NightSubLong$Year)
str(NightSubLong)

library(lubridate)
NightSubLong <- NightSubLong %>%
  mutate(FDate = date(Date))  # create variable with date attr

str(NightSubLong)

# now we can do some plotting

ggplot(data=NightSubLong,
       aes(x=FDate, y=Freq, fill=Cause)) +
  geom_col(position="stack") # default

# ..................................................
# comparing position options for bar graphs
#     stacked (default), dodge, fill, identity
NightSubLong %>%
  filter(FDate <= "1854-06-01") %>%
  ggplot(aes(x=Date, y=Freq, fill=Cause)) +
  geom_col()

NightSubLong %>%
  filter(FDate <= "1854-06-01") %>%
  ggplot(aes(x=Date, y=Freq, fill=Cause)) +
  geom_col(position="dodge")

NightSubLong %>%
  filter(FDate <= "1854-06-01") %>%
  ggplot(aes(x=Date, y=Freq, fill=Cause)) +
  geom_col(position="fill")

NightSubLong %>%
  filter(Date <= "1854-06-01") %>%
  ggplot(aes(x=Date, y=Freq, fill=Cause)) +
  geom_col(position="identity",
           alpha=.5) +
  scale_y_continuous(breaks=c(1,5,10),
                     labels=c("1","5","10"))


# ..................................................


# issues:
#   1:  wedge area prop. to  radius^2 (transform y?)
#   2.  start at 9 o'clock (3*pi/2 radians)

NightSubLong %>%
  ggplot(aes(x=FDate, y=Freq, fill=Cause)) +
  geom_bar(stat="identity",
           position="identity",
           alpha=.5) +
  scale_y_sqrt() +
  coord_polar(start=3*pi/2)

# famous plot has 2 facets - after April 1855 vs
#                            before March 1855

NightSubLong <- NightSubLong %>%
  mutate(indBeforeApri1855 = Date<"1855-04-01",
         DateGrp = factor(indBeforeApri1855,
                          levels=c(FALSE,TRUE),
                          labels=c("APRIL 1855 to MARCH 1856",
                                   "APRIL 1854 to MARCH 1855")))

NightSubLong <- NightSubLong %>%
  mutate(indBeforeApri1855 = Date<"1855-04-01",
         DateGrp = factor(indBeforeApri1855,
                          levels=c(FALSE,TRUE),
                          labels=c("APRIL 1855 to MARCH 1856",
                                   "APRIL 1854 to MARCH 1855")))


NightSubLong %>%
  ggplot(aes(x=FDate, y=Freq, fill=Cause)) +
  geom_col(position="identity",
           alpha=.5) +
  scale_y_sqrt() +
  coord_polar(start=3*pi/2) +
  facet_grid(. ~ DateGrp)

# start to clean up axes

NightSubLong <- NightSubLong %>%
  mutate(MonYr = paste(Month,Year))

NightSubLong %>%
  ggplot(aes(x=FDate, y=Freq, fill=Cause)) +
  geom_col(position="identity",
           alpha=.5) +
  scale_y_sqrt() +
  coord_polar(start=3*pi/2) +
  facet_grid(. ~ DateGrp) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())

#    note that all date values in two panels
#    use patchwork package?

# need to keep comparable scales .
sqrtM <- max(sqrt(NightSubLong$Freq))
sqrtM
FMax <- max(NightSubLong$Freq) # limits before scale transformation
FMax

glater <- NightSubLong %>%
  filter(indBeforeApri1855 == FALSE) %>%
  ggplot(aes(x=FDate, y=Freq, fill=Cause)) +
  geom_col(position="identity",
           alpha=.5,
           color="black") +
  scale_y_sqrt(limits=c(0, FMax)) +
  coord_polar(start=3*pi/2) +
  facet_grid(. ~ DateGrp) +
  labs(title="APRIL 1855 to MARCH 1856") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_blank())

gearly <- NightSubLong %>%
  filter(indBeforeApri1855 == TRUE) %>%
  ggplot(aes(x=FDate, y=Freq, fill=Cause)) +
  geom_col(position="identity",
           alpha=.75,
           color="black") +
  scale_y_sqrt(limits=c(0, FMax)) +
  coord_polar(start=3*pi/2) +
  facet_grid(. ~ DateGrp) +
  labs(title="APRIL 1854 to MARCH 1855") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_blank())


glater
gearly

library(patchwork)
FN_PW <- glater | gearly

FN_PW

FN_PW +
  plot_annotation(
    title="DIAGRAM of the CAUSES of MORTALITY",
    subtitle = "in the ARMY in the EAST"
  )


# BONUS? pies? .....................................

NightSubLong %>%
  ggplot(aes(x=FDate, y=Freq, fill=Cause)) +
  geom_col(position="identity",
           alpha=.5)

NightSubLong %>%
  ggplot(aes(x=FDate, y=Freq, fill=Cause)) +
  geom_col(position="fill",
           alpha=.5)

NightSubLong %>%
  ggplot(aes(x="1", y=Freq, fill=Cause)) +
  geom_col(position="fill",
           alpha=.5) +
  coord_polar(theta="y") +
  facet_wrap(~FDate)

NightSubLong %>%
  ggplot(aes(x="1", y=Freq, fill=Cause)) +
  geom_col(position="fill",
           alpha=.5) +
  coord_polar(theta="y") +
  facet_wrap(~FDate) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())


# 'partial' pie more appealing here ..............
NightSubLong %>%
  ggplot(aes(x="1", y=Freq, fill=Cause)) +
  geom_col(alpha=.5) +
  coord_polar(theta="y") +
  facet_wrap(~FDate) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_blank())

#################
#################
#################
