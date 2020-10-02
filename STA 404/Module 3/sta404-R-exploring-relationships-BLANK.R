# sta404-R-exploring-relationships-BLANK.R
# directory:  
#    MacOS:  home/Desktop/Classes/sta404-fall-2020
# Revised: 01sep2020

# ====================================================================
# load packages we will need
library(tidyverse)
library(readr)   

# 
# ====================================================================
# let's import a couple of data files 

# this command will work on a local installation of RStudio but not on the server

fev_DF <- read.table("http://www.users.miamioh.edu/baileraj/classes/sta404/fev_data.txt")
head(fev_DF)
str(fev_DF)

# what happened?
the top row wasnt interpreted as headers



# take a look at the arguments to read.table
?read.table

# now read this again in a useful way!
fev_DF <- read.table("http://www.users.miamioh.edu/baileraj/classes/sta404/fev_data.txt",
                     header=T)
head(fev_DF)
str(fev_DF)
# ASIDE:  can read text files directly with readr::read_delim
# fev_DF_new <- read_delim("fev_data.txt", delim=" ")
# fev_DF_new2 <- read_table("fev_data.txt")
# 
# str(fev_DF_new)
# str(fev_DF_new2)


# read data about Florida boats registered and manatee deaths over years
#     Credit:  Dr. T. Fisher for adding to original Moore and McCabe data
manatee_DF <- read_csv("http://www.users.miamioh.edu/baileraj/classes/sta404/manatee-updateTF.csv")


# ====================================================================
# basic scatter plots

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_point()

# PAUSE to reflect:  what do you notice?
#positive linear relationship, overplotting, discrete age values

# PAUSE to reflect:  what else might impact this pattern?
#

# PAUSE to reflect:  what concerns might you have with this display?










# OVERPLOTTING of data values .......................................

# what if you have a large data set with lots of overplotting?
# for example, diamonds from ggplot2 package has 50K observations

# TRANSPARENCY setting (alpha) ......................................

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_point(alpha=.1)


# PAUSE to reflect:  try different settings for alpha, say .1, .2, .33, .5
#     which did you prefer and why? .2 or .33, could see more variations


# JITTER ...........................................................

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(width=.5)

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(width=.25)

# PLOTTING SYMBOL - e.g. open circle - shape=1 ....................

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(shape=1,width=.25)

# BOXPLOT - how about your old friend .............................
#
ggplot(fev_DF,aes(x=as.factor(age.yrs), 
                  y=ht.in)) +
  geom_point() +
  geom_boxplot() 

ggplot(fev_DF,aes(x=as.factor(age.yrs), 
                  y=ht.in)) +
  geom_boxplot() +
  geom_jitter(width=.25, alpha=.2) 

# BINNING Displays ................................................
# plot some binned values - think histogram in 2 dimensions

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_bin2d()

ggplot(fev_DF,aes(x=as.factor(age.yrs), y=ht.in)) + 
  geom_bin2d()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_bin2d() +
  scale_fill_continuous(low = "lightgrey", high="black") +
  theme_minimal()

ggplot(fev_DF,aes(x=as.factor(age.yrs), y=ht.in)) + 
  geom_bin2d() +
  scale_fill_continuous(low = "lightgrey", high="black") +
  theme_minimal()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_bin2d() +
  scale_fill_continuous(low = "lightgrey", high="black") +
  theme_minimal()

install.packages("hexbin")
library(hexbin)
ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  stat_binhex()   # probably better for continuous vs. integer values
# COMMENT: with discrete x, more granularity in the scale

set.seed(922020)
indepN2 <- data.frame(x=rnorm(10000), y=rnorm(10000))

ggplot(indepN2,aes(x=x, y=y)) + 
  geom_bin2d() +
  scale_fill_continuous(low = "lightgrey", high="black") +
  theme_minimal()

ggplot(indepN2,aes(x=x, y=y)) + 
  stat_binhex() +
  scale_fill_continuous(low = "lightgrey", high="black") +
  theme_minimal()


# ADDING VARIABLES ..................................................

# COLOR, SIZE or other aesthetic for other variable
ggplot(fev_DF,aes(x=as.factor(age.yrs), 
                  y=ht.in,
                  color=as.factor(ind.Male))) +
  geom_point() +
  theme_minimal()

# add variable with more informative labels
fev_DF <- fev_DF %>% 
  mutate(Gender = factor(ind.Male, labels=c("Female","Male")))
# ... always check and verify new variable construction
head(fev_DF)
with(fev_DF, table(ind.Male,Gender))

ggplot(fev_DF,aes(x=as.factor(age.yrs), 
                  y=ht.in,
                  color=Gender)) +
  geom_point() +
  theme_minimal() +
  labs(x="Age", y="Height (in)")


ggplot(fev_DF,aes(x=as.factor(age.yrs), 
                  y=ht.in,
                  color=Gender)) +
  geom_jitter(width=.25, alpha=.5) +
  theme_minimal() +
  labs(x="Age", y="Height (in)")


# FACETS (small multiples) .....
ggplot(fev_DF,aes(x=as.factor(age.yrs), 
                  y=ht.in)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap(. ~ Gender) +
  labs(x="Age", y="Height (in)")

ggplot(fev_DF,aes(x=as.factor(age.yrs), 
                  y=ht.in)) +
  geom_boxplot() +
  geom_jitter(width=.25, alpha=.3) +
  facet_wrap(. ~ Gender) +
  labs(x="Age", y="Height (in)")


# CURVE FITTING ......................................................
#    adding visual references and summaries to association plots

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x =gdpPercap, y = lifeExp)) +
    geom_point()
# adding a statistical layer - model fits
# linear regression

# using existing geom_ to add fit ...................................
ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(width=.25) +
  geom_smooth(method="lm", se=FALSE) +
  labs(x="Age", y="Height (in)") + 
  theme_minimal()

# fitting a linear model and adding this to plot ....................

fev_linreg <- lm(ht.in ~ age.yrs, data=fev_DF)

fev_DF <- fev_DF %>% 
  mutate(yhat=predict(fev_linreg), 
         r = residuals(fev_linreg))

ggplot(fev_DF, aes(x = age.yrs, y = ht.in)) +
  geom_point()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(width=.25) +
  geom_line(aes(y=yhat)) +
  labs(x="Age", y="Height (in)") + 
  theme_minimal()

# residual plot

ggplot(fev_DF, aes(x=age.yrs, y=r) ) +
  geom_jitter(width=.25, alpha=.4) +
  geom_smooth(method="loess") + 
  labs(x="Age",y="Residual") +
  geom_hline(yintercept = 0, color="red") +
  theme_minimal()


# ==================================================================
# options to the parametric models
# loess - local polynomial regression fitting

xx <- seq(-8*pi,8*pi,length=100)
yy <- 3 + 2+xx + 4*sin(xx) + rnorm(100, sd = 1.5)

curvy_DF <- data.frame(x=xx, y=yy)
ggplot(curvy_DF, aes(x,y)) +
  geom_point()

ggplot(curvy_DF, aes(x,y)) +
  geom_point() +
  geom_smooth(method="lm")

ggplot(curvy_DF, aes(x,y)) +
  geom_point() +
  geom_smooth(method="loess")

# you can change the span of the data considered for the local fits
ggplot(curvy_DF, aes(x,y)) +
  geom_point() +
  geom_smooth(method="loess",span=.4)

ggplot(curvy_DF, aes(x,y)) +
  geom_point() +
  geom_smooth(method="loess",span=.2)

ggplot(curvy_DF, aes(x,y)) +
  geom_point() +
  geom_smooth(method="loess",span=.1)

# let's look at the ht-age relationship data again

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(width=.25) +
  geom_smooth(method="loess",se=F, span=.5) +
  labs(x="Age", y="Height (in)") + 
  theme_minimal()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(width=.25) +
  geom_smooth(method="loess",se=F, span=.4) +
  labs(x="Age", y="Height (in)") + 
  theme_minimal()


# residual plot - revisited

ggplot(fev_DF, aes(x=age.yrs, y=r) ) +
  geom_jitter(width=.25,alpha=.4) +
  geom_smooth(method="loess", se=F, color="black") +
  geom_hline(yintercept=0, color="red") +
  theme_minimal() +
  labs(x="Age", y="Residual") + 
  theme_minimal()


#======================================================================

# What about the other variables in the fev_DF?

ggplot(fev_DF,aes(x=age.yrs, y=ht.in, color=Gender)) + 
  geom_jitter(width=.25, alpha=.3) +
  geom_smooth() +
  labs(x="Age", y="Height (in)") + 
  theme_minimal()

ggplot(fev_DF,aes(x=age.yrs, y=ht.in)) + 
  geom_jitter(width=.25, alpha=.3) +
  facet_grid(. ~ Gender) +
  labs(x="Age", y="Height (in)") + 
  theme_minimal()

#======================================================================

# what if you want to add a model fit to an existing plot?
# Using Challenger disaster data
# http://www.asktog.com/books/challengerExerpt.html
# https://archive.ics.uci.edu/ml/datasets/Challenger+USA+Space+Shuttle+O-Ring
# Temp forecast at launch of Challenger = 31 F

# data set is text with spaces separating values - may need to edit to make sure filename is ok
oring <- read_table(file="https://archive.ics.uci.edu/ml/machine-learning-databases/space-shuttle/o-ring-erosion-only.data",
                    col_names=c("Nring","Ndistress","TempF","LeakCheckPSI","LaunchOrder"))

str(oring)
mode(oring)                        # so what type of object did read_table produce?

oring_DF <- as.data.frame(oring)   # convert to a data frame
oring_DF

# $!$#@#$$ server won't work for everyone so if using the server
# alternative - download data and upload to server and read from there

oring2 <- read_table(file="~/classes/sta404/o-ring-erosion-only.data",
                    col_names=c("Nring","Ndistress","TempF","LeakCheckPSI","LaunchOrder"))

str(oring2)

View(oring_DF)

# adding variable indicating distress to at least one O-ring
oring_DF <- oring %>% 
  mutate(ind_distress = ifelse(Ndistress>0, 1, 0))    # adding variable indicating distress 

# oring_DF <- oring_DF %>% 
#   mutate(ind_distress = as.numeric(Ndistress>0))  

# plot of original data
# start here on Tuesday - 24 sept 19 ...

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
  geom_point()

# suppose a simple logistic regression model was fit
# log(odds of O-ring distress) = b0 + b1 TempF

oring_logreg <- glm(ind_distress ~ TempF, 
                    data=oring_DF, family=binomial)

summary(oring_logreg)

oring_DF <- oring_DF %>% 
  mutate(phat = predict(oring_logreg, type="response"))

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
  geom_point() +
  geom_line(aes(y=phat))

# predictions are a little jagged - clean this up

pred_DF <- data.frame(TempF = seq(from=min(oring_DF$TempF),
                                  to=max(oring_DF$TempF),
                                  length=1000))

pred_DF <- pred_DF %>% 
  mutate(phat = predict(oring_logreg, newdata=pred_DF, type="response"))

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
  geom_point() +
  geom_line(data=pred_DF, aes(x=TempF,y=phat))

# cleaning up figure
# http://ggplot2.tidyverse.org/reference/annotate.html

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
  geom_point() +
  geom_line(data=pred_DF, aes(x=TempF,y=phat), color="darkgrey")+
  scale_x_continuous(name="Temperature (F)", limits=c(50,84)) +
  scale_y_continuous(name="O-ring distress observed", breaks=c(0,1),
                     labels=c("No","Yes")) +
  annotate("text", x=71,y=.25, 
           label="No problems for\nlaunch Temp > 70 F",
           hjust=0, color="blue") +
  annotate("rect", xmin = 71, xmax = 83, ymin = 0, ymax = 1,
           alpha = .2, fill="blue") +
  annotate("text", x=50, y=.50,
           label="Challenger launch\ntemperature forecast\nwas 31 F!",
           hjust=0, color="red") +
  annotate("text",x=60,y=.9,label="Logistic Regression Prediction",
           hjust=0, color="darkgrey") +
  labs(caption="Source: UCI Machine Learning Repository") +
  theme_minimal()

#================================================================

# Lab Exercise

#work with the lake-do-depth data

# Base R - read.csv()
# readr::read_csv

# read data from Lake Tahoe / Tahoe Keys - dissolved Oxygen as function of depth
#    at 2 sites:  Tahoe Keys marina and Eagle Lake
lake_DF <- read_csv("http://www.users.miamioh.edu/baileraj/classes/sta404/lake-do-depth.csv")

ggplot(lake_DF, aes(x=depth, y=dis_oxygen, color=lakeid)) +
  geom_point() + 
  scale_y_log10() +
  geom_smooth(method="lm", se=FALSE) +
  labs(x = "Depth (in feet)", y = "Dissolved Oxygen")

# clean up the labels for lake ID and labels on axes
