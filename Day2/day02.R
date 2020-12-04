## Author: @Shel_Kariuki
## Day: #2

## Load the packages needed
library(tidyverse)

## Read in the data
df <- read.table("Day2/input_day2.txt")

## Rename the columns into `condition`, `letter`, `password`
colnames(df) <- c("condition", "letter", "password")

## Split the condition into `left_condition` and `right_condition`.
df <- df %>% 
  separate(condition, 
           into = c("left_condition", "right_condition"),
           sep = "-", remove = FALSE)

## Convert the `left_condition` and `right_condition` into numerics
df <- df %>% 
  mutate(across(c("left_condition", "right_condition"), ~as.numeric(.)))

## Remove the colon from the `letter`
df <- df %>% 
  mutate(letter = trimws(gsub(":", "", letter)))

## ------------------------Part 1------------------------------------------------------------------
## How many passwords are valid according to the initial policies?
## ------------------------------------------------------------------------------------------------

## Generate a variable `letter_counter` that contains the number of times the 
## letter appears in the password
df1 <- df %>% 
  mutate(letter_counter = str_count(password, letter))

## Generate a variable `validity`, that checks whether the password condition has
## been met or not.
df1 <- df1 %>% 
  mutate(validity = ifelse(letter_counter >= left_condition &
                          letter_counter <= right_condition, "valid", "not valid"  ))

## How many passwords are valid?
valid_passwords <- df1 %>% 
  filter(validity == "valid") %>% 
  select(validity) %>% 
  count() %>% 
  pull(n)

## ------------------------Part 2---------------------------------------------------------------------------------------
## How many passwords are valid according to the new interpretation of the policies?
## ------------------------------------------------------------------------------------------------

## Generate variables that indicate the letters that are in the positions given
df2 <- df %>% 
  mutate(left_position_letter = substr(password, left_condition, left_condition),
         right_position_letter = substr(password, right_condition, right_condition))

df2 <- df2 %>% 
  ## only one of the letters should match the positions, not both
  mutate(diclaimer = ifelse(letter == left_position_letter &
                              letter == right_position_letter, "not allowed", "okay")) %>% 
       
  mutate(validity = ifelse((letter == left_position_letter|
                             letter == right_position_letter) &
                             diclaimer != "not allowed",
                           "valid", "not valid"))

## Generate the number of passwords that are valid
valid_passwords2 <- df2 %>% 
  filter(validity == "valid") %>% 
  select(validity) %>% 
  count() %>% 
  pull(n)
