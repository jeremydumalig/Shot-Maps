library(tidyverse)
library(ggplot2)
library(ggrepel)
library(png)
library(gt)
rm(list=ls())

# Import sample data, calculate FG%
kobe_sample <- read_csv("kobe_regions.csv") %>%
  group_by(Region) %>%
  summarize(FGM = sum(shot_made_flag),
         FGA = n()) %>%
  mutate(`FG%` = 100 * round(FGM / FGA, 3),
         Splits = paste(paste(as.character(FGM), 
                              as.character(FGA), 
                              sep="/"), 
                        paste(as.character(100*round(FGM/FGA, 3)),
                              "%",
                              sep=""), 
                        sep="\n")) %>%
  ungroup()

# Import court locations
court <- read_csv(file="court.csv") %>%
  select(X, Y, Region)

# Import region label locations
region_labels <- read_csv(file="region_labels.csv") %>%
  select(Region, X, Y)

# Align court locations and shot data
shots <- merge(x=court,
               y=select(kobe_sample, Region, `FG%`, Splits),
               by="Region",
               all.x=TRUE)

# Align region label locations and shot data
region_labels <- merge(x=region_labels,
                       y=kobe_sample,
                       by="Region",
                       all.x=TRUE)

# Set title and subtitle
title <- "Kobe Bryant (1996-2016)"
subtitle <- "Los Angeles Lakers"

# Create shot map
plot <- ggplot(data=shots, aes(x=X, y=Y, color=`FG%`)) + 
  geom_point(shape=15,
             size=2,
             alpha=0.625,
             show.legend=FALSE) +
  scale_color_gradient(low="white", 
                        high="red", 
                        limits=range(shots$`FG%`),
                        na.value="#c4c4c4") +
  annotation_raster(readPNG("court.png"), 
                    ymin = 0,
                    ymax= 47,
                    xmin = 0,
                    xmax = 50) +
  geom_label(
    data=region_labels, 
    aes(x=X, y=Y, label=Splits),
    size=4,
    color='black',
    fill = alpha(c("white"), 0.5)) +
  xlim(0, 50) +
  ylim(0, 47) +
  labs(x="", 
       y="",
       title=md(title),
       subtitle=subtitle) +
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      color = "black"),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(face="bold",
                              size=20),
    plot.subtitle = element_text(size=16),
    legend.title = element_text(size=10)
  )

# Create accompanying table
table <- kobe_sample %>%
  mutate(Splits = paste(as.character(FGM), 
                        as.character(FGA), 
                        sep="/"),
         Region = case_when(
           Region == 'P' ~ "Paint",
           Region == 'LC' ~ "Left Corner",
           Region == 'RC' ~ "Right Corner",
           Region == 'LW' ~ "Left Wing",
           Region == 'RW' ~ "Right Wing",
           Region == 'T' ~ "Top",
           Region == 'TK' ~ "Top of the Key",
           Region == 'RE' ~ "Right Elbow",
           Region == 'LE' ~ "Left Elbow",
           Region == 'RB' ~ "Right Baseline",
           Region == 'LB' ~ "Left Baseline"
         )) %>%
  select(Region, Splits, `FG%`) %>%
  arrange(desc(`FG%`)) %>%
  gt() %>%
  tab_header( 
    title = md(paste("**", title, "**", 
                     sep="")),
    subtitle = subtitle)

# Generate shot map and table
print(plot)
print(table)
