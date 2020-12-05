## Author: @Shel_Kariuki
## Day: #5
## Challenge: Passport Processing

## Load the packages needed
library(tidyverse)
library(zoo)

## Read in the data
df <- data.frame(V1 = read_lines("Day5/input_day5.txt"))

## ----------------------------------------------------------------------------
## ------------------Part 1------------------------------------------------------
## ----------------------------------------------------------------------------

## Ensure that the boarding seat specifications do not have leading or trailig spaces
df <- df %>% 
  mutate(V1 = trimws(V1))

## Row function

row_function <- function(boarding_pass){

letter_count <- nchar(gsub("R|L", "", boarding_pass))

start = 0
end = 127
i = 1
while(i <= 7){
  
  letter <- substr(boarding_pass, i, i)
  
  div = (end - start)/2
  first_half_start = start
  first_half_end = start + div - 0.5
  second_half_start = end - div + 0.5
  second_half_end = end
  
  if(letter == "F"){
    min_range = first_half_start
    max_range = first_half_end
  }else{
    if(letter == "B"){
      min_range = second_half_start
      max_range = second_half_end  
    }
    
}

  start = min_range
  end = max_range 
  i = i + 1
}

final_row_num = as.integer(start)
return(final_row_num)

}

## Column function

col_function <- function(boarding_pass){
  
  letter_count <- nchar(gsub("F|B", "", boarding_pass))
  
  start = 0
  end = 7
  i = 8
  while(i >= nchar(boarding_pass) -letter_count +1 & 
        i <= nchar(boarding_pass)){
    
    letter <- substr(boarding_pass, i, i)
    
    div = (end - start)/2
    first_half_start = start
    first_half_end = start + div - 0.5
    second_half_start = end - div + 0.5
    second_half_end = end
    
    if(letter == "L"){
      min_range = first_half_start
      max_range = first_half_end
    }else{
      if(letter == "R"){
        min_range = second_half_start
        max_range = second_half_end  
      }
      
    }
    
    start = min_range
    end = max_range 
    i = i + 1
  }
  
  final_col_num = as.integer(start)
  return(final_col_num)
  
}

## Generate variables that contain the row number and column number
df$row_num <- map_int(df$V1, row_function)
df$col_num <- map_int(df$V1, col_function)

# Generate a variable that shows the seatID

df <- df %>% 
  mutate(seatid = row_num * 8 + col_num)

## Generate the answer (maximum row id) 
answer = max(df$seatid)

## ----------------------------------------------------------------------------
## ------------------Part 1------------------------------------------------------
## ----------------------------------------------------------------------------

union_seq = seq(min(df$seatid), max(df$seatid), 1)
answer2 = union_seq[which(!union_seq %in% df$seatid)]
