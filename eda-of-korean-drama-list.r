---
title: "EDA of Korean Drama List"
date: "`r format(Sys.time(), '%B, %Y')`"
author: "@permadisatya"

output:
  html_document:
    toc: true
    toc_depth: 3
    fig_width: 8
    fig_height: 5
    theme: cosmo
    highlight: tango
    code_folding: hide
---

```{r setup, include=FALSE}
# setup
knitr::opts_chunk$set(cache = FALSE)
options(
  scipen = 9999,
  dplyr.summarise.inform = FALSE
)
```

# Load library

Here are the library that helps we discover the data:

```{r warning=FALSE, message=FALSE}
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(ggplotify)
library(gridExtra)
library(scales)
library(PerformanceAnalytics)
```

# Load data

```{r warning=FALSE, message=FALSE}
df_descriptors <- read.csv(
  file = "../input/korean-drama-list-about-740-unique-dramas/main_descriptors.csv",
  na.strings = ""
)
df_actors <- read.csv(
  file = "../input/korean-drama-list-about-740-unique-dramas/actors.csv",
  na.strings = ""
)
```

# Exploratory

As we can see at kaggle data overview, we know there are NA value occured. We did data cleaning and creating some column feature to improve exploratory.

## DF Descriptors

```{r warning=FALSE}
# feature engineering
df_descriptors <- df_descriptors %>% 
  # standardize column name
  rename_all(tolower) %>% 
  rename_all(~gsub("\\s+|\\.", "_", .)) %>% 
  # there are two format of date that need to standardize
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
  ) %>% 
  # filter NA only for needed, end_airing data can be NA because is still on going
  filter(
    !is.na(start_airing),
    !is.na(score)
  ) %>% 
  # create data year_airing
  mutate(
    year_airing = year(start_airing)
  )
# view sample data
sample_n(df_descriptors, 10)
```

Data structure:

```{r warning=FALSE}
str(df_descriptors)
```

We still take the observation with the missing value except in `start_airing` and `score`. 
Here is a summary of missing value:

```{r warning=FALSE}
apply(df_descriptors, 2, function (x) sum(length(which(is.na(x)))))
```

**How many kdrama produced every year?**

```{r, fig.height=4, out.width="100%"}
df_descriptors %>% 
  mutate(year_airing = year(start_airing)) %>% 
  group_by(year_airing) %>% 
  summarise(total_kdrama = n()) %>% 
  na.omit() %>% 
  ggplot(aes(x = year_airing, y = total_kdrama)) +
  geom_col(fill = "cornflowerblue")
```
-   We can see that kdrama produce had decreased in 2008-2010, but increased significantly in 2011 til 2021. 
This is the moment where the Korean Culture breakthrough and known in the world. [(src)](https://en.wikipedia.org/wiki/Korean_wave)

**How the correlation?**

The data mostly as numeric data type, we can see the correlation between them:

```{r warning=FALSE, fig.height=6, fig.width=8}
chart.Correlation(
  df_descriptors[, purrr::map_lgl(df_descriptors, is.numeric)],
  histogram = T,
  method = c("pearson", "kendall", "spearman")
)
```
We find:

-   All parameters seems like having a highly significant correlation to each other.

-   Year of airing have negative correlation with total of episodes, it means the kdrama total
episode have decreased over the year. The assumption, the kdrama produced pattern has moved from 
producing more episodes to more new kdrama/storyline that drive the watchers have wide option
preferences.

-   `ranked` and `popularity` have an reverse ordinal data that have negative correlation 
between other variables.

-   `watchers` and `scored_by` have as good as perfect correlation that means more watchers can make
the chance to give the movie a score.

## DF Actors

```{r}
p1 <- df_actors %>%
  group_by(actor) %>%
  summarise(total = n()) %>% 
  ggplot(aes(x = total)) +
  geom_histogram(binwidth = 2, fill = "cornflowerblue") +
  labs(y = "Frequency", x = "Total kdrama by each actor")
p2 <- df_actors %>%
  group_by(kdrama_name) %>%
  summarise(total = n()) %>% 
  ggplot(aes(x = total)) +
  geom_histogram(binwidth = 1, fill = "cornflowerblue") +
  labs(y = "Frequency", x = "Total actor by each kdrama")
p3 <- data.frame(
  var = c("total_kdrama", "total_actors"),
  count = c(length(unique(df_actors$kdrama_name)), length(unique(df_actors$actor)))
  ) %>% 
  ggplot(aes(x = var, y = count)) +
  geom_col(fill = "cornflowerblue") +
  labs(y = "Total") +
  theme(axis.title.x = element_blank())
# generate chart into layout
grid.arrange(
  arrangeGrob(p3),
  arrangeGrob(p1, p2, ncol = 1),
  ncol = 2
)
```

-   Actors are distributed to starred in 1 kdrama, only few actors who starred many kdrama.

-   Kdrama list consist to mention 6 actors, few kdrama mention less than 6.
