##### Libraries #####
library(tidyverse)
library(here)

##### Data #####
dayfour <- read_lines(here("data/day-four-data.txt")) %>% 
  paste(sep = "", collapse = " ") %>% 
  str_split("  ") %>%
  unlist()

dayfour

##### Part One Analysis #####
tibble(raw_string = dayfour) %>%
  separate(col = raw_string, into = as.character(c(1:8)), sep = " ", fill = "right") %>%
  view()

