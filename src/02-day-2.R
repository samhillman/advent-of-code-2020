##### Libraries #####
library(tidyverse)
library(here)

##### Data #####
daytwo <- read_csv(here("data/day-two-data.csv"), col_names = FALSE) %>%
  separate(col = X1, into = c("policy", "password"), sep = ":")

##### Part One Analysis #####
# Each password must conform to the policy where the policy gives the min and
# max instances of each letter needed in the password

# think we'll start by separating the policy into its constituent parts:

daytwo <- daytwo %>%
  separate(policy, into = c("amount", "letter"), sep = " ", remove = FALSE) %>%
  separate(amount, into = c("min_times", "max_times"), sep = "-") %>%
  # this ordering of the columns is just because I think it flows better!
  select(policy, letter, min_times, max_times, password)

# we also need to clean up the password column a little and change the format of
# the min and max columns
daytwo <- daytwo %>%
  mutate(min_times = as.numeric(min_times),
         max_times = as.numeric(max_times))

# that looks good to me! Let's see how we can find passwords that match the policy

# we can filter out results that don't meet the requirements - would be nice to 
# find a one stage solution but we can probably do it in two steps
daytwo %>%
  mutate(letter_count = as.numeric(str_count(string = password, pattern = letter))) %>%
           filter(letter_count >= min_times & letter_count <= max_times) %>%
  NROW()

##### Part Two Analysis #####
# We got the policy wrong! The two numbers are the string position, the letter
# is still the letter that must be there, and only ONE of these positions can
# contain that letter

# we need to rename some columns first!
daytwo <- daytwo %>%
  select(policy, letter, first_position = min_times, second_position = max_times,
         password) %>%
  mutate(password = str_trim(password))

daytwo %>%
  # does calculations by row, seems to need this
  rowwise() %>%
  #for the mutate calls - finds if the character at the index provided by the 
  # first position variable equals the letter in question, then the same for the
  # second position. Then we add the scores and find the ones with only a score
  # of one - valid passwords
  mutate(first_policy_score = dplyr::case_when(
    substr(password, first_position, first_position) == letter ~ 1,
    TRUE ~ 0)) %>% 
  mutate(second_policy_score = dplyr::case_when( 
    substr(password, second_position, second_position) == letter ~ 1,
    TRUE ~ 0)) %>%
  mutate(policy_score = first_policy_score + second_policy_score) %>%
  filter(policy_score == 1) %>%
  NROW()
  
