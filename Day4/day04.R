## Author: @Shel_Kariuki
## Day: #4
## Challenge: Passport Processing

## Load the packages needed
library(tidyverse)
library(zoo)

## Read in the data
df <- data.frame(V1 = read_lines("Desktop/Day4/input_day4.txt"))

## ----------------------------------------------------------------------------
## ------------------Part 1------------------------------------------------------
## ----------------------------------------------------------------------------

## Generate a variable that shows the passport id
df2 <- df %>% 
  mutate(blank = ifelse(V1 == "", "blank", NA)) %>% 
  mutate(counter = seq_along(blank),
         passportid = ifelse(blank == "blank", paste0(blank, counter),NA),
         passportid = na.locf0(passportid, fromLast = TRUE)) %>% 
  filter(blank != "blank" |is.na(blank)) %>% 
  select(-blank, -counter) 

## Collapse the records into one record
df2 <- df2 %>% 
  group_by(passportid) %>% 
  mutate(single_record = paste(V1, collapse = " ")) %>% 
  distinct(passportid, single_record)

## Separate the record into different variables
df2 <- df2 %>% 
  separate(single_record, into = paste0("C_", 1:8), sep = " ")

## Reshape the data
df2 <- df2 %>% 
  pivot_longer(names_to = "field_no", 
               values_to = "field", 
               cols = starts_with("C_"),
               values_drop_na = TRUE)

## Separate the field into field_name and field_value
df2 <- df2 %>% 
  separate(field, into = c("field_name", "field_value"), sep = ":") %>% 
  mutate(field_name = trimws(field_name)) %>% 
  select(-field_no)

## Generate a variable that counts the number of fields per passport id
df2 <- df2 %>% 
  group_by(passportid) %>% 
  mutate(field_count = length(unique(field_name)))

## Check validity
df2 <- df2 %>% 
  group_by(passportid) %>% 
  mutate(validity = ifelse(field_count == 8, "valid",
                           ifelse(field_count == 7 & any(field_name == "cid") == FALSE, 
                                  "valid", "invalid"))) 

## Part 1

## Calculate the number of valid passports
valid_passports1 <- df2 %>% 
  distinct(passportid, validity) %>% 
  group_by(validity) %>% 
  count() %>% 
  filter(validity == "valid") %>% 
  pull(n)

## -----------------------------------------------------------------------------
## ------------------Part 2------------------------------------------------------
## -----------------------------------------------------------------------------

# Conditions: 
## byr (Birth Year) - four digits; at least 1920 and at most 2002.
## iyr (Issue Year) - four digits; at least 2010 and at most 2020.
## eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
## hgt (Height) - a number followed by either cm or in:
##   If cm, the number must be at least 150 and at most 193.
## If in, the number must be at least 59 and at most 76.
## hcl (Hair Color) - a ## followed by exactly six characters 0-9 or a-f.
## ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
## pid (Passport ID) - a nine-digit number, including leading zeroes.
## cid (Country ID) - ignored, missing or not.

df3 <- df2 %>% 
  mutate(field_value = trimws(field_value)) %>% 
  mutate(validity2 = 
           # byr (Birth Year) - four digits; at least 1920 and at most 2002.
           ifelse(field_name == "byr" & nchar(field_value) == 4 & 
                  between(as.numeric(field_value), 1920, 2002), "okay",
           # iyr (Issue Year) - four digits; at least 2010 and at most 2020. 
          ifelse(field_name == "iyr" & nchar(field_value) == 4 & 
                 between(as.numeric(field_value), 2010, 2020), "okay", 
                  
          # eyr (Expiration Year) - four digits; at least 2020 and at most 2030. 
          ifelse(field_name == "eyr" & nchar(field_value) == 4 & 
                 between(as.numeric(field_value), 2020, 2030), "okay",
          # hgt (Height) - a number followed by either cm or in:
          # If "cm", the number must be at least 150 and at most 193.       
          ifelse(field_name == "hgt" & 
                 field_value %in% grep("cm", field_value, ignore.case = TRUE, value = TRUE) &
                 between(as.numeric(trimws(gsub("cm","", field_value))), 150, 193),"okay",
          
          # If "in", the number must be at least 59 and at most 76.
          ifelse(field_name == "hgt" & 
                           field_value %in% grep("in", field_value,ignore.case = TRUE, value = TRUE) &
                           between(as.numeric(trimws(gsub("in","", field_value))), 59, 76),"okay",
                 
          # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
          ifelse(field_name == "hcl" & 
                   field_value %in% grep("#", field_value,ignore.case = TRUE, value = TRUE) &
                   nchar(field_value) == 7, "okay",
                 
          # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
          ifelse(field_name == "ecl" & 
                   field_value %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
                   nchar(field_value) == 3, "okay",
                   
          # pid (Passport ID) - a nine-digit number, including leading zeroes.
          ifelse(field_name == "pid" & nchar(gsub("[^0-9]+", "", field_value)) == 9, "okay",
          
          # cid (Country ID) - ignored, missing or not. 
          ifelse(field_name == "cid", "okay",  "invalid")))))))))) %>% 
  
  mutate(final_validity = ifelse(all(validity == "valid") & all(validity2 == "okay"),
                                 "valid", "invalid"))
     

## Calculate the number of valid passports
valid_passports2 <- df3 %>% 
  distinct(passportid, final_validity) %>% 
  group_by(final_validity) %>% 
  count() %>% 
  filter(final_validity == "valid") %>% 
  pull(n)  
