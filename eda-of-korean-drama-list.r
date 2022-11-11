---
title: "EDA of Korean Drama List"
date: "`r format(Sys.time(), '%B, %Y')`"
author: "@permadisatya"

output:
  html_document:
    df_print: paged
    toc: true
    toc_depth: 3
    fig_caption: true
    fig_width: 8
    fig_height: 5
    theme: paper
    highlight: zenburn
    code_folding: hide
---

```{r setup, include=FALSE}
# setup
knitr::opts_chunk$set(cache = FALSE)
options(
  scipen = 9999,
  dplyr.summarise.inform = FALSE
)

# library
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(ggplotify)
library(gridExtra)
library(scales)
library(RColorBrewer)
```

------------------------------------------------------------------------

# Kdrama and actors

```{r}
# load data
tbl_actors <- read.csv("../input/korean-drama-list-about-740-unique-dramas/actors.csv")

# create graph
p1 <- tbl_actors %>%
  group_by(actor) %>%
  summarise(total_kdrama = n()) %>%
  arrange(desc(total_kdrama)) %>% 
  ggplot(aes(x = total_kdrama)) +
  geom_histogram(binwidth = 2, fill = "cornflowerblue") +
  labs(y = "Frequency", x = "Total kdrama by each actor")
p2 <- tbl_actors %>%
  group_by(kdrama_name) %>%
  summarise(total_actor = n()) %>%
  arrange(desc(total_actor)) %>% 
  ggplot(aes(x = total_actor)) +
  geom_histogram(binwidth = 1, fill = "cornflowerblue") +
  labs(y = "Frequency", x = "Total actor by each kdrama")
p3 <- data.frame(
  var = c("total_kdrama", "total_actors"),
  count = c(length(unique(tbl_actors$kdrama_name)), length(unique(tbl_actors$actor)))
  ) %>% 
  ggplot(aes(x = var, y = count)) +
  geom_col(fill = "cornflowerblue") +
  labs(y = "Count")

# generate chart into layout
grid.arrange(
  arrangeGrob(p3),
  arrangeGrob(p1, p2, ncol = 1),
  ncol = 2
)
```

-   Actors are distributed to starred in 1 kdrama, only few actors who starred many kdrama.

-   Kdrama list consist to mention 6 actors, few kdrama mention less than 6.

```{r warning=FALSE}
# load data
tbl_descriptors <- read.csv("../input/korean-drama-list-about-740-unique-dramas/main_descriptors.csv")

# feature engineering
tbl_descriptors <- tbl_descriptors %>% 
  # standardize column name
  rename_all(tolower) %>% 
  rename_all(~gsub("\\s+|\\.", "_", .)) %>% 
  mutate(start_airing = case_when(
    grepl(",", start_airing, ignore.case = T) ~ mdy(start_airing),
    grepl("", start_airing, ignore.case = T) ~ dmy(start_airing)
  )) %>% 
  mutate(end_airing = case_when(
    grepl(",", end_airing, ignore.case = T) ~ mdy(end_airing),
    grepl("", end_airing, ignore.case = T) ~ dmy(end_airing)
  )) %>% 
  mutate(
    content_rating = as.factor(content_rating),
    score = as.double(score),
    scored_by = as.integer(scored_by),
    duration = as.integer(duration),
    watchers = as.integer(stringr::str_replace(watchers, ",", ""))
  )

# view summary
summary(tbl_descriptors)
```

```{r}
tbl_descriptors %>% 
  mutate(year = year(start_airing)) %>% 
  group_by(year) %>% 
  summarise(total = n()) %>% 
  na.omit() %>% 
  ggplot(aes(x = year, y = total)) +
  geom_col(fill = "cornflowerblue")
```

-   We can see that kdrama produce had decreased in 2008-2010, but increased significantly in 2011 til 2021. 
This is the moment where the Korean Culture breakthrough and known in the world. [(src)](https://en.wikipedia.org/wiki/Korean_wave)