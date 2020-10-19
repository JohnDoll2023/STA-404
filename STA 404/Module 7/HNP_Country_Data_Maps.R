# HNP_Country_Data_Maps.R
# Revised: 17 Oct 2020

# packages needed for reading, reshaping and plotting
library(tidyverse)

# bring in new data set from the World Bank
#   Health Nutrition and Population Statistics

data_URL <- "https://www.users.miamioh.edu/baileraj/workshop-Bali/HNP_Data.csv"
HNP_DF <- read_csv(data_URL)

HNP_DF_subset <- HNP_DF %>% 
  select(-X61) %>% 
  filter(`Indicator Name` %in% 
           c("Birth rate, crude (per 1,000 people)",
             "Death rate, crude (per 1,000 people)",
             "Fertility rate, total (births per woman)",
             "Life expectancy at birth, female (years)",
             "Life expectancy at birth, male (years)",
             "Mortality rate, under-5 (per 1,000)",
             "Rural population (% of total population)",
             "Survival to age 65, female (% of cohort)",
             "Survival to age 65, male (% of cohort)",
             "Urban population (% of total)"))

HNP_DF_long <- HNP_DF_subset %>% 
  pivot_longer(cols =`1960`:`2015`,
               names_to = "cyear",
               values_to = "number") %>% 
  mutate(year=as.numeric(cyear)) %>% 
  select(-cyear) 

HNP_DF_expand <- HNP_DF_long %>%  
  select(-`Indicator Code`) %>% 
  pivot_wider(names_from = `Indicator Name`,
              values_from = number) 

names(HNP_DF_expand) <- c(
  "Country_Name",
  "Country_Code",
  "year",
  "birth_Rate",
  "death_Rate",
  "fertility_PC_rate",
  "life_exp_female",
  "life_exp_male",
  "child_mortality",
  "rural_pop_pct",
  "surv_age65_female_pct",
  "surv_age65_male_pct",
  "urban_pop_pct"
)

# remove aggregrate 'countries'
uniqueNames <- unique(HNP_DF_expand$Country_Name)
HNP_DF_expand_no_groups <- HNP_DF_expand %>% 
  filter(!(Country_Name %in% uniqueNames[1:41]))

#####################################-------------------------------
# package for countrycode conversions (ASIDE)
#####################################-------------------------------

install.packages("countrycode")
library(countrycode)
help(package=countrycode)

names(countrycode_data)
countrycode_data$country.name.en

View(countrycode_data)

# map example

# You can play around with limits and other variables. Urban population percentage
# of population looked kind of cool. I think the best thing to do here would be to
# look at percentage increases and decreases from different years (maybe a 10 year
# gap) to tell a better story. The code is:

#library(tidyverse) 
#install.packages("maps")
library(maps)

HNP_DF_Ranking_2014 <- HNP_DF_expand_no_groups %>%
  filter(year==2014)

chloroMapALL <- map_data('world') %>%
  filter(region != 'Antarctica') %>%
  inner_join(HNP_DF_Ranking_2014, 
             by=c("region" = "Country_Name")) %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group=group, fill=urban_pop_pct),colour="black",size=0.1) +
  coord_equal() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() +
  scale_fill_gradient("Urban Population (%)", high="darkgrey",
                      breaks = seq(0, 100, 20), limits = c(0, 100))

chloroMapALL

# Some country codes don't match!

HNP_DF_Ranking_2014_Plus <- HNP_DF_Ranking_2014 %>% 
  mutate(Country_Name = ifelse(Country_Name == "United States", "USA",
                               ifelse(Country_Name == "United Kingdom", "UK",
                                      ifelse(Country_Name == "Russian Federation", "Russia",
                                             ifelse(Country_Name == "Venezuela, RB", "Venezuela",
                                                    ifelse(Country_Name == "Congo, Rep.", "Republic of Congo",
                                                           ifelse(Country_Name == "Congo, Dem. Rep.","Democratic Republic of the Congo",
                                                                  ifelse(Country_Name == "Iran, Islamic Rep.", "Iran",
                                                                         ifelse(Country_Name == "Egypt, Arab Rep.", "Egypt",
                                                                                ifelse(Country_Name == "Cote d'Ivoire", "Ivory Coast",
                                                                                       ifelse(Country_Name == "Yemen, Rep.", "Yemen",
                                                                                              ifelse(Country_Name == "Sint Maarten (Dutch part)", "Sint Maarten",
                                                                                                     ifelse(Country_Name == "Korea, Rep.", "South Korea",
                                                                                                            ifelse(Country_Name == "PRK", "North Korea",
                                                                                                                   ifelse(Country_Name == "Micronesia, Fed. Sts.", "Micronesia",
                                                                                                                          ifelse(Country_Name == "Gambia, The", "Gambia",
                                                                                                                                 Country_Name))))))))))))))))
# break urban % into quartiles                               
HNP_DF_Ranking_2014_Plus <- HNP_DF_Ranking_2014_Plus %>% 
  mutate(urbanCat = cut(urban_pop_pct,
                        quantile(urban_pop_pct, 
                                 na.rm=TRUE)))

chloroMapALL <- map_data('world') %>%
  filter(region != 'Antarctica') %>%
  inner_join(HNP_DF_Ranking_2014_Plus, 
             by=c("region" = "Country_Name")) %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group=group, fill=urbanCat),
               colour="black",size=0.1) +
  coord_equal() +
  labs(x='Longitude', y='Latitude') +
  theme_bw() +
  scale_fill_brewer(palette="Oranges")

chloroMapALL

chloroMapSubset <- map_data('world') %>%
  filter(region != 'Antarctica') %>%
  inner_join(HNP_DF_Ranking_2014_Plus, 
             by=c("region" = "Country_Name")) %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group=group, fill=urbanCat),
               colour="black",size=0.1) +
  coord_equal() +
  scale_x_continuous(expand=c(0,0), limits = c(80, 175)) +
  scale_y_continuous(expand=c(0,0), limits = c(-50, 35)) +
  labs(x='Longitude', y='Latitude') +
  theme_bw() +
  scale_fill_brewer(palette="Oranges")

chloroMapSubset

