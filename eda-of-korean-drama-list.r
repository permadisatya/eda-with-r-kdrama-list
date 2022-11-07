---
title: "KDrama EDA"
author: "@permadisatya"
---

```{r setup, include=FALSE}
# setup
knitr::opts_chunk$set(cache=FALSE)
options(
  scipen = 9999,
  dplyr.summarise.inform = FALSE
)
```

```{r}
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

```{r}
df_actors <- read.csv("../input/korean-drama-list-about-740-unique-dramas/actors.csv")
p1 <- df_actors %>% 
  group_by(actor) %>% 
  summarise(total_kdrama = n()) %>% 
  arrange(desc(total_kdrama)) %>%  
  ggplot(aes(total_kdrama)) +
  geom_histogram(binwidth = 3) +
  theme(text = element_text(size = 21), element_line(size = 1.5))
p2 <- df_actors %>% 
  group_by(kdrama_name) %>% 
  summarise(total_actor = n()) %>% 
  arrange(desc(total_actor)) %>% 
  ggplot(aes(total_actor)) +
  geom_histogram(binwidth = 1) +
  theme(text = element_text(size = 21), element_line(size = 1.5))

grid.arrange(p1, p2, ncol = 2)
```

We find:
- Only some actors that have played in more than 10 kdrama.
- Each kdrama listed of all actors by mostly 6 name of actors. Only few kdrama listed actors name with less than 6 name.