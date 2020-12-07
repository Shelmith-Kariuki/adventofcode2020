## Author: @Shel_Kariuki
## Day: #7
## Challenge: Handy Haversacks

## Load the packages needed
library(tidyverse)
library(zoo)

## Read in the data
df <- data.frame(rules = read_lines("Day7/input_day7.txt"))

## ----------------------------------------------------------------------------
## ------------------Part 1----------------------------------------------------
## How many bag colors can eventually contain at least one shiny gold bag?
## ----------------------------------------------------------------------------


## Ensure that we do not have blank rows and Split the `rules` into `outer_bag` and `contents` 
df2 <- df %>% 
        filter(rules != "") %>% 
        separate(rules, into = c("outer_bag", "contents"), sep = "contain") 


## Count the number of times the word "bags" is mentioned per row. This will help us know the
## maximum number of bags in each row
df2$no_bags = apply(df2[,-1,drop=F], 1, function(x) 
                sum(str_count(x, "bag")))

max_no_bags = max(df2$no_bags, na.rm = TRUE)

## Separate the contents into different variables and reshape to long format
df2 <-  df2 %>%  
        select(-no_bags) %>% 
        separate(contents, into = paste0("bag_", c(1:max_no_bags+3)), sep = ",") %>% 
        pivot_longer(cols = contains("bag_"),
                    names_to = "bags_serialno",
                    values_to = "contents",
                    values_drop_na = TRUE)

## Drop "bag_" from bags_serialno and "bags" from contents and `outer_bag`
df2 <- df2 %>% 
  mutate(across(c("bags_serialno", "contents", "outer_bag"), 
                ~gsub("bag_|bags|bag|\\.", "", .)))

## Generate no_of_bags from `contents`.
df2 <- df2 %>% 
  mutate(no_of_bags = str_extract(contents, "[0-9]"),
         no_of_bags = ifelse(is.na(no_of_bags), 0, no_of_bags)) %>% 
  mutate(contents = gsub("[0-9]", "", contents))

## Ensure that the numeric variables are actually numeric and the character variables do 
## not have leading, trailing or white spaces.
df2 <- df2 %>% 
  mutate(across(c("bags_serialno", "no_of_bags"), ~as.numeric(trimws(str_squish(.)))),
         across(c("outer_bag", "contents"), ~trimws(str_squish(.))))


## Generate the bag colors that contain at least one shiny gold bag
outer_bags = df2 %>% 
  filter(contents == "shiny gold") %>%
  distinct(outer_bag) %>% 
  pull()

## Loop over the bag colors to get a vector of all possible bag colors that can contain
## a shiny gold bag
i <- 1
while(i <= 594){
  
  all_possible_bags <- df2 %>% 
                        filter(contents == outer_bags[i]) %>%
                        distinct(outer_bag)%>% 
                        pull()
  
  outer_bags <- c(outer_bags, all_possible_bags)
  
  i = i + 1
}


## Count the number of unique bag colors that can eventually contain at least one shiny gold bag
answer_1 = unique(outer_bags)


## ----------------------------------------------------------------------------
## ------------------Part 2----------------------------------------------------
## How many individual bags are required inside your single shiny gold bag?
## ----------------------------------------------------------------------------

## Generate a vector of possible bags that can be inside the shiny gold bag

# shiny_bag_contents <- df2 %>% 
#   filter(outer_bag == "shiny gold") %>%
#   distinct(contents) %>% 
#   pull()
# 
# i <- 1
# while(i <= 594){
#   
#   all_possible_bags <- df2 %>% 
#     filter(contents == shiny_bag_contents[i]) %>%
#     distinct(outer_bag)%>% 
#     pull()
#   
#   shiny_bag_contents <- c(shiny_bag_contents, all_possible_bags)
#   
#   i = i + 1
# }
# 
# shiny_bag_contents2 <- unique(c(shiny_bag_contents, answer_1)) #397
# 
# bag_color <- ""
# y <- 0
# for(i in 1: length(shiny_bag_contents2)){
#   bag_color[i] <- shiny_bag_contents2[i]
#   
#   bag_color_content = df2 %>% 
#     filter(outer_bag == shiny_bag_contents2[i]) 
#   
#   no_bag_color_content =    try(bag_color_content %>% 
#     select(no_of_bags) %>% 
#     pull())
#   
#   unique_outerbags <- unique(bag_color_content$contents)
#   
#   x <- 0
#   
#   if(length(unique_outerbags) >0){
#   for( j in 1: length(unique_outerbags)){
# 
#   no_content_bag <-   try(df2 %>% 
#     filter(outer_bag == unique_outerbags[j]) %>% 
#     select(no_of_bags) %>% 
#     pull())
#     
#   x[j] <- no_bag_color_content[j] * sum(no_content_bag)
#   
#   }
#   }
#   
#   
#   y[i] =  sum(x[j])
# }
# 
# 
# 
# df3 <- data.frame(bag_color, no_bags = y)
# 
# answer_3 <- sum(df3$no_bags)
