## Author: @Shel_Kariuki
## Day: #3
## Toboggan Trajectory

## Load the packages needed
library(tidyverse)
library(datapasta)

## Read in the data
df <- read.table("Day3/input_day3.txt")


## Condense this into one long vector
df2 <- df %>% 
  mutate(x = paste0(V1, collapse = "")) %>% 
  distinct(x)

start_point = 1
jump = 34

output <- ""

for(i in 1: 295){
  output[i] <- substr(trimws(df2$x), start_point + (i *jump), start_point + (i *jump))
  
}

table(output, useNA = "always")




