# patchwork-extended-example.R

library(maps)
library(ggplot2)
library(patchwork)

states_map <- map_data("state")

ohio_map <- subset(states_map, states_map$region == "ohio")

GTLEFT <- ggplot(data=ohio_map,
       aes(x=long, y=lat)) +
  geom_point(fill="white", colour="black") +
  theme_void()
GTLEFT

# create a couple of other graphs for exploring patchwork
state.x77.DF <- as.data.frame(state.x77)
state.x77.DF$StateName <- rownames(state.x77)
names(state.x77.DF)

G1R <- ggplot(state.x77.DF,
       aes(x=Population)) +
  geom_histogram() +
  scale_x_log10()

G2R <- ggplot(state.x77.DF,
       aes(x=Income)) +
  geom_histogram() +
  scale_x_log10()

G3R <- ggplot(state.x77.DF,
       aes(x=`HS Grad`)) +
  geom_histogram() 

G4R <- ggplot(state.x77.DF,
       aes(x=Illiteracy)) +
  geom_histogram() 

GB1 <- ggplot(state.x77.DF,
       aes(x=`Life Exp`, y = reorder(StateName,`Life Exp`))) +
  geom_point() +
  labs(y = "")
GB1

GB2 <- ggplot(state.x77.DF,
       aes(x=Murder, y = reorder(StateName,`Life Exp`))) +
  geom_point() +
  labs(y = "")
GB3 <- ggplot(state.x77.DF,
       aes(x=Illiteracy, y = reorder(StateName,`Life Exp`))) +
  geom_point() +
  labs(y = "")
GB4 <- ggplot(state.x77.DF,
       aes(x=Frost, y = reorder(StateName,`Life Exp`))) +
  geom_point() +
  labs(y = "")


GPATCH <- ( GTLEFT | (G1R/G2R/G3R/G4R) ) / (GB1 | GB2 | GB3 | GB4)
GPATCH

#
# can control layouts
# https://patchwork.data-imaginist.com/articles/guides/layout.html

GTOP <- ( GTLEFT | (G1R/G2R/G3R/G4R) ) + plot_layout(widths=c(2,1))
GTOP

GTOP / (GB1 | GB2 | GB3 | GB4) + plot_layout(heights=c(2,1))





