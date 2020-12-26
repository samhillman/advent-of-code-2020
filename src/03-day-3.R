##### Libraries #####
library(tidyverse)
library(here)

##### Data #####
daythree <- readLines(here("data/day-three-data.txt"))
daythree


##### Part One #####

# Plan is to make data into a dataframe with each symbol having own row and column
# then can generate a list of row + column numbers needed for each 'move'
# then extract the symbols at each of these places and count the number of 'trees'

# THE SAME PATTERN repeats itself to the right indefinitely.
# The data is 323 long and 31 wide - if we have a movement of one down then it'll 
# take 322 moves to get below the slope.
# 322 moves = (322*3) = 966 moves to the right
# 966 on the right / 31 per df = 32 repeats

daythree_rep <- tibble(slope = strrep(daythree, 32))

daythree_sep <- daythree_rep %>%
  separate(col = slope, into = as.character(0:992), sep = "") %>%
  select(-"0")

# 323 down means we need to make 323 moves
# 323 moves down means we need to take 323*3 to the right =969 moves
# so lets reduce the data down to 323 down and 969 across as otherwise the for loop
# doesn't like the different dimensions
daythree_reduced <- daythree_sep[,1:969]

# just check the dimensions  
969/323

# So our route starts at (1,1) and every step is right 3 and down one
# e.g. (1,1)(4,2)(7,3)
# lets see if we can extract all those values!

locations <- tibble(move = seq(from = 1, to = 323, by = 1),
                    across = seq(from = 1, to = 969, by = 3),
                    down = seq(from = 1, to = 323, by = 1),
                    encounter = NA) 

for (i in 1:323){
  locations$encounter[i] <- daythree_reduced[locations$down[i], locations$across[i]] 
  }

# now we have these in a dataframe, let's count them!
#table(locations$encounter this wasnt working for some reason so 
locations %>% 
  filter(encounter == "#") %>%
  NROW()

##### Part Two #####
# now we need to generalise this out to any set of x and y movement
# e.g. right 1 down 1, right 5 down 1

# function to make the right-sized skiing field with the correct dimensions
generate_field <- function(across, down){

  down_moves_needed <- ceiling(323 / down)
  repeats_needed <- ceiling((down_moves_needed * across) / 31)
  len_across_needed <- repeats_needed * nchar(daythree[1])
  across_moves_needed <- down_moves_needed * across
  
  # now let's build the ski slope
  daythree_field <- tibble(slope = strrep(daythree, times = as.integer(repeats_needed))) %>% 
    separate(col = slope, into = as.character(0:len_across_needed), sep = "") %>%
    select(-"0")
  
  # then reduce it down so the dimensions fit (basically shave some x axis off)
  daythree_field_reduced <- daythree_field[,1:across_moves_needed] 
  
  print(daythree_field_reduced)
  
}

#now we have the field, let's generate the locations we need to check
locations_needed <- function(across, down){
  
  down_moves_needed <- ceiling(323 / down)
  across_moves_needed <- down_moves_needed * across
  locations <- tibble(move = seq(from = 1, to = down_moves_needed, by = 1),
         across = seq(from = 1, to = across_moves_needed, by = across),
         down = seq(from = 1, to = 323, by = down),
         encounter = NA)
  
  print(locations)

}

# count the trees
count_trees <- function(location, field){
  for (i in 1:nrow(location)){
    location$encounter[i] <- field[location$down[i], location$across[i]]
  }
  
  print(location %>% 
    filter(encounter == "#") %>%
    NROW())
  
}

# Combining All Functions? 
combined_count <- function(across, down){
  field <- generate_field(across, down)
  location <- locations_needed(across, down)
  number <- count_trees(location, field)
  print(number)
}


### Now let's solve! ###
one <- combined_count(1, 1) # R1 D1
two <- combined_count(3, 1) # R3 D1
three <- combined_count(5, 1) # R5 D1
four <- combined_count(7, 1) # R7 D1
five <- combined_count(1, 2) # R1 D3



one * two * three * four * five
