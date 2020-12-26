##### Libraries #####
library(tidyverse)
library(here)

##### Data #####
dayone <- read_csv(here("data/day-one-data.csv"), col_names = FALSE) %>%
  select(entry = X1)

##### Analysis #####
# Find the two entries that sum to 2020 and then multiply them together
merge(dayone, dayone, by = c()) %>%
  mutate(sum = entry.x + entry.y) %>%
  filter(sum == 2020) %>%
  mutate(answer = entry.x * entry.y)

# Now we need to find THREE values that sum to 2020
merge(dayone, dayone, by = c()) %>%
  #might have to find a more efficient solution!
  merge(dayone, by = c()) %>%
  mutate(sum = entry.x + entry.y + entry) %>%
  filter(sum == 2020) %>%
  mutate(answer = entry.x * entry.y * entry)
