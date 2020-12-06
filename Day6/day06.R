## Author: @Shel_Kariuki
## Day: #6
## Challenge: Custom Customs

## Load the packages needed
library(tidyverse)
library(zoo)

## Read in the data
df <- data.frame(questions = read_lines("Day6/input_day6.txt"))

## ----------------------------------------------------------------------------
## ------------------Part 1------------------------------------------------------
## ----------------------------------------------------------------------------

## Generate a variable that shows the groupid and another one that shows the person_id
df2 <- df %>% 
  mutate(blank = ifelse(questions == "", "group", NA)) %>% 
  mutate(counter = seq_along(blank),
         groupid = ifelse(blank == "group", paste0(blank, counter),NA),
         groupid = na.locf0(groupid, fromLast = TRUE)) %>% 
  group_by(groupid) %>% 
  filter(blank != "group" |is.na(blank)) %>% 
  select(-blank, -counter) %>% 
  mutate(personid = seq_along(groupid)) %>% 
  select(groupid, personid, questions)

## Generate an object that shows the maximum possible `yes` answers
max_characterlength = max(nchar(trimws(str_squish(df2$questions))))

## Separate the questions into different variables and 
## reshape the data so that all the qs are in one variable
df2 <- df2 %>% 
  group_by(groupid) %>% 
  mutate(questions = trimws(str_squish(questions))) %>% 
  separate(questions, into = paste0("q_", 1:max_characterlength), sep = "")%>% 
  pivot_longer(cols = contains("q_"), values_drop_na = TRUE) %>% 
  filter(value!="") %>% 
  mutate(value = trimws(str_squish(value)))

## Count the number of questions to which anyone answered "yes" in each group and
## generate the sum of these counts
answer_1 <- df2 %>% 
  group_by(groupid) %>% 
  summarise(n_unique_qs = length(unique(value))) %>% 
  ungroup() %>% 
  summarise(summation = sum(n_unique_qs)) %>% 
  pull(summation)


## ----------------------------------------------------------------------------
## ------------------Part 2------------------------------------------------------
## ----------------------------------------------------------------------------

tab2 <- df2 %>% 
  ungroup() %>% 
  group_by(groupid) %>% 
  mutate(n_persons = length(unique(personid))) %>% 
  ungroup() %>% 
  group_by(groupid, value) %>% 
  mutate(n_qs = length(value)) %>% 
  ungroup() %>% 
  mutate(all_yes = ifelse(n_qs == n_persons, 1, 0)) %>% 
  filter(all_yes == 1) %>% 
  distinct(groupid, value, all_yes) %>% 
  group_by(groupid) %>% 
  summarise(n_unique_qs = sum(all_yes, na.rm = T)) %>% 
  ungroup()
  
answer_2 <- tab2 %>% 
  summarise(summation = sum(n_unique_qs, na.rm = T)) %>% 
  pull(summation)
