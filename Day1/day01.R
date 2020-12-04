## Author: @Shel_Kariuki
## Day: #1

## Load the packages needed
library(tidyverse)

## ------------------------Part 1---------------------------------------------------------------------------------------
## Find the two entries that sum to 2020; what do you get if you multiply them together?
## ------------------------------------------------------------------------------------------------

## Read in the data
df <- read.table("Day1/input.txt")

input_1 <- df$V1
input_2 <- df$V1

tab1 <- expand_grid(input_1, input_2)

result1 <- tab1 %>% 
  mutate(jumla = input_1 + input_2) %>% 
  filter(jumla == 2020) %>% 
  select(input_1, input_2) %>% 
  sample_n(1) %>% 
  mutate(product = input_1 * input_2) %>% 
  pull(product)
  
## ------------------------Part 2---------------------------------------------------------------------------------------
## What is the product of the three entries that sum to 2020?
## ------------------------------------------------------------------------------------------------

input_3 <- df$V1
tab2 <- expand_grid(input_1, input_2, input_3)

result2 <- tab2 %>% 
  mutate(jumla = input_1 + input_2 + input_3) %>% 
  filter(jumla == 2020) %>% 
  select(input_1, input_2, input_3) %>% 
  sample_n(1) %>% 
  mutate(product = input_1 * input_2 * input_3) %>% 
  pull(product)