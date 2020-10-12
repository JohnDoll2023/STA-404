#  module-06-case-study-ForHW.R
#  hw6-notes-16oct17.R
#
#  Read of Excel file
#    deleted 5 rows 
#    deleted last column ("notes")
#    read first row (country, series, 1995--2014) as

library(readxl)
library(dplyr)
Arrivals_NR_tourists_visitors <- 
  read_excel("Arrivals of non resident tourists_visitors, depart.xlsx", 
             skip = 5) %>% 
  select(-NOTES)




#
# Deleted rows at bottom of the spreadsheet
# The information is the same as that provided in the "Compendium of Tourism Statistics"
# Inbound tourism:	
#   TF	Arrivals of non-resident tourists at national borders
#   VF	Arrivals of non-resident visitors at national borders
#   THS	Arrivals of non-resident tourists in hotels and similar establishments
#   TCE	Arrivals of non-resident tourists in all types of accommodation establishments
# Outbound tourism:	
#   TF	Departures - trips abroad by resident tourists
#   VF	Departures - trips abroad by resident visitors
#   IMF	International Monetary Fund
#   CB	Central Bank
#   ..	Data not available
# Source: World Tourism Organization (UNWTO)


# read in data ================================================================
library(readr)
hw6_subset_csv <- read_csv("C:/Users/baileraj/Downloads/hw6-subset-csv.csv",
                           col_names = FALSE)

#On Rstudio server
hw6_subset_csv <- read_csv("/home/baileraj/sta404/Data/hw6-subset-csv.csv",
                           col_names = FALSE)


View(hw6_subset_csv)

# add country name to each row =================================================

hw6_out <- data.frame(NULL)

for (ii in 1:nrow(hw6_subset_csv))  {
  if (!is.na(hw6_subset_csv[ii,"X1"])) {
    country <- hw6_subset_csv[ii,"X2"]
  }
  else {
    hw6_out <- rbind(hw6_out,
                     cbind(country, hw6_subset_csv[ii,2:25]) )
  } # end of 'else'
}   # end of 'for'

# clean up names ================================================================

names(hw6_out) <- c("country", "v2", "group", "series", paste0("X",1995:2014))  

#  issues: ======================================================================
#       countries have different numbers of rows
#       inbound and outbound tourism have different numbers of ros

hw6_out %>% 
  group_by(country) %>% 
  summarize(nvars = n())

# search for presence of patterns - grep
#      http://www.statmethods.net/management/functions.html
# chaining if-else statements
#      http://www.dummies.com/programming/r/how-to-chain-ifelse-statements-in-r/ 

hw6_out2 <- data.frame(NULL)

for (ii in 1:nrow(hw6_out)) {
  if (length(grep("Inbound", hw6_out[ii,"v2"]) )) {
    prefix <- "Inbound"
  } else if (length(grep("Outbound", hw6_out[ii,"v2"]))) {
    prefix <- "Outbound"
  } else {
    vars <- paste(prefix, hw6_out[ii,"v2"], sep="_")
    hw6_out2 <- rbind(hw6_out2,
                      cbind(vars, hw6_out[ii,c(1,3:24)])) 
  }
}

# examining a couple of example cases ===================================================================
as.data.frame(hw6_out2 %>% 
                group_by(country) %>% 
                summarize(nvars = n())
)

hw6_out2 %>% 
  filter(country=="POLAND")

hw6_out2 %>% 
  filter(country=="ZIMBABWE")

# clean up variable names ================================================================================
hw6_out2 <- hw6_out2 %>% 
  mutate(new_vars = sub(" - ","_",vars)) %>% 
  mutate(new_vars = gsub(" ","_",new_vars)) %>% 
  mutate(new_vars = paste(new_vars,series,sep="_")) 

unique(hw6_out2$new_vars)


hw6_wide <- hw6_out2 %>% 
  select(country, new_vars, X1995:X2014)

View(hw6_wide)

# now can think about restructuring the data ============================================================

library(tidyr)
hw6_long <- hw6_wide %>% 
  group_by(new_vars) %>% 
  gather(key=cyear, value=cmeasure, X1995:X2014) %>% 
  mutate(year = as.numeric(substring(cyear,2)), 
         measure = as.numeric(sub(',','',cmeasure))) %>%          # commas in cell entries!   
  select(-cyear, -cmeasure)  

View(hw6_long)

hw6_analysis <- hw6_long%>% 
  spread(key=new_vars, value=measure)

View(hw6_analysis)

as.data.frame(hw6_analysis %>% 
                filter(country=="UNITED KINGDOM") 
)