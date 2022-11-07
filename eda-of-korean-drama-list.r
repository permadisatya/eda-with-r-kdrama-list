---
title: "KDrama EDA"
date: "`r format(Sys.time(), '%B, %Y')`"
author: "@permadisatya"
output:
  bookdown::html_document2:
    theme: paper
    highlight: zenburn
    df_print: paged
    toc: true
    toc_depth: 2
    toc_float:
      collapsed: false
    number_sections: true
    code_folding: hide
---

```{r setup, include=FALSE}
# setup
knitr::opts_chunk$set(cache=FALSE)
options(
  scipen = 9999,
  dplyr.summarise.inform = FALSE
)
# library
library(bookdown)

library(dplyr)
library(reshape2)
library(lubridate)

library(ggplot2)
library(gridExtra)
library(scales)
library(RColorBrewer)
```

---

```{r}
tbl_actors <- read.csv("../input/korean-drama-list-about-740-unique-dramas/actors.csv")

p1 <- tbl_actors %>% 
  group_by(actor) %>% 
  summarise(total_kdrama = n()) %>% 
  arrange(desc(total_kdrama)) %>% 
  ggplot(aes(total_kdrama)) +
  geom_histogram(binwidth = 5)
p2 <- tbl_actors %>% 
  group_by(kdrama_name) %>% 
  summarise(total_actor = n()) %>% 
  arrange(desc(total_actor)) %>% 
  ggplot(aes(total_actor)) +
  geom_histogram(binwidth = 1)
p3 <- data.frame(
  var = c("total_kdrama", "total_actors"),
  count = c(length(unique(tbl_actors$kdrama_name)), length(unique(tbl_actors$actor)))
  ) %>% 
  ggplot(aes(x = var, y = count)) +
  geom_col()
grid.arrange(
  arrangeGrob(p3),
  arrangeGrob(p1, p2, ncol = 1),
  ncol = 2
)
```

We find:

- Only some actors that have played in more than 10 kdrama.
- Each kdrama listed of all actors by mostly 6 name of actors. Only few kdrama listed actors name with less than 6 name.