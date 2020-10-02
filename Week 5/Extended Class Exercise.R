# E X T E N D E D C L A S S E X E R C I S E
# START OF CLASS EXERCISE
# 1. Fit a model with common intercept but different slopes
# 2. Superimpose fit on the plot
# 3. Clean up the axes
# 4. Move the lake id onto the plot

library(tidyverse)
lake_DF <- read_csv("http://www.users.miamioh.edu/baileraj/classes/sta404/lake-do-depth.csv")

# 1. Fit a model with common intercept but different slopes

##################################################################################
# ref: https://www.eia.gov/todayinenergy/detail.php?id=41133
# https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_w.htm
# https://www.eia.gov/opendata/qb.php?sdid=PET.EMM_EPM0_PTE_SOH_DPG.W



years <- 2020:2003

# extract weekly avg for dates before 9 sep but after 1 sep
prices <- c(2.11, 2.61, 2.76, 2.65, 2.26, 2.21, 3.48, 3.67,
            3.89, 3.73, 2.75, 2.46, 3.60, 3.03, 2.47, 3.06,
            1.81, 1.72)

gas_DF <- data.frame(year=rev(years),
                     price=rev(prices))  #reverses order of variables
gas_DF

ggplot(gas_DF, aes(x = year, y = price)) + 
  geom_col()

gas_DF <- gas_DF %>% 
  mutate(cyear = as.factor(year),
         bar_col = ifelse(cyear=="2017", "blue", "lightgrey"))

ggplot(gas_DF, aes(x = year, y = price, fill = year)) + 
  geom_col()

ggplot(gas_DF, aes(x = year, y = price, fill = cyear)) + 
  geom_col()

ggplot(gas_DF, aes(x = year, y = price, fill = cyear)) + 
  geom_col() +
  scale_fill_manual(values = gas_DF$bar_col)

xbar <- mean(gas_DF$price)

gas_DF$pricediff2 <- gas_DF$price - xbar

gas_DF <- gas_DF %>% 
  mutate(pricediff = price - mean(price),
         pricediffcat = cut(pricediff, breaks = (seq(from = -1.25, to = 1.25, by = .25))),
         diff_ind = ifelse(pricediff>0, "greater", "less"))

ggplot(gas_DF, aes(x = year, y = pricediff, fill = diff_ind)) + 
  geom_col()

ggplot(gas_DF, aes(x = year, y = pricediff, fill = pricediffcat)) +
  geom_col() +
  scale_fill_brewer(type = "div", palette = "PuOr", guide = FALSE) +
  labs(x = "Year", y = "",
       title = "Ohio's Labor Day week average gas prices",
       subtitle = "Difference from series average ($2.79)",
       caption = "Data source: U.S. Energy Information Admin.") +
  theme_minimal()
  









##################################################################################

names(lake_DF)
ggplot(lake_DF, aes(x = depth, y = dis_oxygen, color = lakeid)) +
  geom_point() +
  scale_y_log10()

#log(DO) beta) + beta1*depth + beta2*I(eagle) + error
#I(eagle) = 1 eagle lake
#         = 0 if tahoe
lake_DF <- lake_DF %>%
  mutate(log2DO = log2(dis_oxygen),
         IndEagle = as.numeric(lakeid == "E"),
         IndEagle2 = ifelse(lakeid == "E", 1, 0))

view(lake_DF)
diffslope <- lm(log2DO ~ depth + depth:IndEagle, data = lake_DF)
summary(diffslope)

# 2. Superimpose fit on the plot

ggplot(lake_DF, aes(x = depth, y = log2DO, color = lakeid)) +
  geom_point()

predict(diffslope)

predlog2DO_DF <- data.frame(
  depth = c(0:5, 0:26),
  IndEagle = rep(c(0,1), c(6, 27)),
  lakeid = rep(c("T", "E"), c(6, 27))
)

predlog2DO_DF <- predlog2DO_DF %>%
  mutate(yhat = predict(diffslope, newdata = predlog2DO_DF))

ggplot(lake_DF, aes(x = depth, y = log2DO, color = lakeid)) +
  geom_point() +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid))

# 3. Clean up the axes
# text appearance
# color palettes

#change from point to E or T
ggplot() +
  geom_text(data = lake_DF, aes(x = depth, y = log2DO, label = lakeid, color = lakeid)) +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid))

#remove legend
ggplot() +
  geom_text(data = lake_DF, aes(x = depth, y = log2DO, label = lakeid, color = lakeid)) +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid)) +
  guides(color = FALSE)

#clean up axes
ggplot() +
  geom_text(data = lake_DF, aes(x = depth, y = log2DO, label = lakeid, color = lakeid)) +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid)) +
  guides(color = FALSE) +
  labs(x = "Depth (meters)", y = "log2(Dissolved Oxygen (ug/l))")

#increase size
ggplot() +
  geom_text(data = lake_DF, aes(x = depth, y = log2DO, label = lakeid, color = lakeid),
            size = 5) +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid), size = 1.25) +
  guides(color = FALSE) +
  labs(x = "Depth (meters)", y = "log2(Dissolved Oxygen (ug/l))")

#change colors
ggplot() +
  geom_text(data = lake_DF, aes(x = depth, y = log2DO, label = lakeid, color = lakeid),
            size = 5) +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid), size = 1.25) +
  guides(color = FALSE) +
  labs(x = "Depth (meters)", y = "log2(Dissolved Oxygen (ug/l))") +
  scale_color_manual(values = c("orange", "blue")) +
  theme_minimal()

# 4. Move the lake id onto the plot

ggplot() +
  geom_text(data = lake_DF, aes(x = depth, y = log2DO, label = lakeid, color = lakeid),
            size = 5) +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid), size = 1.25) +
  guides(color = FALSE) +
  labs(x = "Depth (meters)", y = "log2(Dissolved Oxygen (ug/l))") +
  scale_color_manual(values = c("orange", "blue")) +
  theme_minimal(base_size = 14)

#annotate
ggplot() +
  geom_text(data = lake_DF, aes(x = depth, y = log2DO, label = lakeid, color = lakeid),
            size = 5) +
  geom_line(data=predlog2DO_DF, aes(x = depth, y = yhat, color = lakeid), size = 1.25) +
  guides(color = FALSE) +
  labs(x = "Depth (meters)", y = "log2(Dissolved Oxygen (ug/l))") +
  scale_color_manual(values = c("orange", "blue")) +
  theme_minimal(base_size = 14) +
  annotate("text", x = 7.5, y = .75, label = "Tahoe Keys", color = "blue", size = 5) +
  annotate("text", x = 11, y = 2.25, label = "Eagle Lake", color = "orange", size = 5)



