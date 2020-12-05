<center>

<strong>

<h1>

[Advent of Code 2020](https://adventofcode.com/2020/about)

</h1>

</strong>

</center>
================
<center>

<strong>

<h2>

[Shelmith Nyagathiri Kariuki](https://shelkariuki.netlify.app/)

</h2>

</strong>

</center>
<center>

<strong>

<h2>

1st December, 2020

</h2>

</strong>

</center>

  - [Day 1: Report Repair](#day-1-report-repair)
      - [Part 1:](#part-1)
      - [Part 2:](#part-2)
  - [Day 2: Password Philosophy](#day-2-password-philosophy)
      - [Part 1:](#part-1-1)
      - [Part 2:](#part-2-1)
  - [Day 3: Toboggan Trajectory](#day-3-toboggan-trajectory)
      - [Part 1:](#part-1-2)
      - [Part 2:](#part-2-2)
  - [Day 4: Passport Processing](#day-4-passport-processing)
      - [Part 1:](#part-1-3)
      - [Part 2:](#part-2-3)
  - [Day 5: Binary Boarding](#day-5-binary-boarding)
      - [Part 1:](#part-1-4)
      - [Part 2:](#part-2-4)

``` r
## Load the packages required
library(tidyverse)
library(zoo)
```

<style>
body {text-align: justify}
</style>

## Day 1: Report Repair

### Part 1:

After saving Christmas five years in a row, you’ve decided to take a
vacation at a nice resort on a tropical island. Surely, Christmas will
go on without you.

The tropical island has its own currency and is entirely cash-only. The
gold coins used there have a little picture of a starfish; the locals
just call them stars. None of the currency exchanges seem to have heard
of them, but somehow, you’ll need to find fifty of these coins by the
time you arrive so you can pay the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on
each day in the Advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck\!

Before you leave, the Elves in accounting just need you to fix your
expense report (your puzzle input); apparently, something isn’t quite
adding up.

Specifically, they need you to find the two entries that sum to 2020 and
then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721 979 366 299 675 1456 In this list, the two entries that sum to 2020
are 1721 and 299. Multiplying them together produces 1721 \* 299 =
514579, so the correct answer is 514579.

Of course, your expense report is much larger. **Find the two entries
that sum to 2020; what do you get if you multiply them together?**

``` r
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
```

### Part 2:

The Elves in accounting are thankful for your help; one of them even
offers you a starfish coin they had left over from a past vacation. They
offer you a second one if you can find three numbers in your expense
report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are
979, 366, and 675. Multiplying them together produces the answer,
241861950.

**In your expense report, what is the product of the three entries that
sum to 2020?**

``` r
input_3 <- df$V1
tab2 <- expand_grid(input_1, input_2, input_3)

result2 <- tab2 %>% 
  mutate(jumla = input_1 + input_2 + input_3) %>% 
  filter(jumla == 2020) %>% 
  select(input_1, input_2, input_3) %>% 
  sample_n(1) %>% 
  mutate(product = input_1 * input_2 * input_3) %>% 
  pull(product)
```

## Day 2: Password Philosophy

### Part 1:

Your flight departs in a few days from the coastal airport; the easiest
way down to the coast from here is via
[toboggan](https://en.wikipedia.org/wiki/Toboggan).

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad
day. “Something’s wrong with our computers; we can’t log in\!” You ask
if you can take a look.

Their password database seems to be a little corrupted: some of the
passwords wouldn’t have been allowed by the Official Toboggan Corporate
Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle
input) of passwords (according to the corrupted database) and the
corporate policy when that password was set.

For example, suppose you have the following list:

1-3 a: abcde 1-3 b: cdefg 2-9 c: ccccccccc Each line gives the password
policy and then the password. The password policy indicates the lowest
and highest number of times a given letter must appear for the password
to be valid. For example, 1-3 a means that the password must contain a
at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg,
is not; it contains no instances of b, but needs at least 1. The first
and third passwords are valid: they contain one a or nine c, both within
the limits of their respective policies.

**How many passwords are valid according to their policies?**

``` r
## Read in the datasets
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
```

``` r
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
```

### Part 2:

While it appears you validated the passwords correctly, they don’t seem
to be what the Official Toboggan Corporate Authentication System is
expecting.

The shopkeeper suddenly realizes that he just accidentally explained the
password policy rules from his old job at the sled rental place down the
street\! The Official Toboggan Corporate Policy actually works a little
differently.

Each policy actually describes two positions in the password, where 1
means the first character, 2 means the second character, and so on. (Be
careful; Toboggan Corporate Policies have no concept of “index zero”\!)
Exactly one of these positions must contain the given letter. Other
occurrences of the letter are irrelevant for the purposes of policy
enforcement.

Given the same example list from above:

1-3 a: abcde is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

**How many passwords are valid according to the new interpretation of
the policies?**

``` r
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
```

## Day 3: Toboggan Trajectory

### Part 1:

``` r
## Pending
```

### Part 2:

``` r
## Pending
```

## Day 4: Passport Processing

### Part 1:

You arrive at the airport only to realize that you grabbed your North
Pole Credentials instead of your passport. While these documents are
extremely similar, North Pole Credentials aren’t issued by a country and
therefore aren’t actually valid documentation for travel in most of the
world.

It seems like you’re not the only one having problems, though; a very
long line has formed for the automatic passport scanners, and the delay
could upset your travel itinerary.

Due to some questionable network security, you realize you might be able
to solve both of these problems at the same time.

The automatic passport scanners are slow because they’re having trouble
detecting which passports have all required fields. The expected fields
are as follows:

byr (Birth Year)

iyr (Issue Year)

eyr (Expiration Year)

hgt (Height)

hcl (Hair Color)

ecl (Eye Color)

pid (Passport ID)

cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each
passport is represented as a sequence of key:value pairs separated by
spaces or newlines. Passports are separated by blank lines.

Here is an example batch file containing four passports:

ecl:gry pid:860033327 eyr:2020 hcl:\#fffffd byr:1937 iyr:2017 <cid:147>
hgt:183cm

iyr:2013 ecl:amb <cid:350> eyr:2023 pid:028048884 hcl:\#cfa07d byr:1929

hcl:\#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm

hcl:\#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in The first
passport is valid - all eight fields are present. The second passport is
invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it
looks like data from North Pole Credentials, not a passport at all\!
Surely, nobody would mind if you made the system temporarily ignore
missing cid fields. Treat this “passport” as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is
fine, but missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid
passports.

**Count the number of valid passports - those that have all required
fields. Treat cid as optional. In your batch file, how many passports
are valid?**

``` r
## Read in the data
df <- data.frame(V1 = read_lines("Day4/input_day4.txt"))

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

## Calculate the number of valid passports
valid_passports1 <- df2 %>% 
  distinct(passportid, validity) %>% 
  group_by(validity) %>% 
  count() %>% 
  filter(validity == "valid") %>% 
  pull(n)
```

### Part 2:

The line is moving more quickly now, but you overhear airport security
talking about how passports with invalid data are getting through.
Better add some data validation, quick\!

You can continue to ignore the cid field, but each other field has
strict rules about what values are valid for automatic validation:

byr (Birth Year) - four digits; at least 1920 and at most 2002.

iyr (Issue Year) - four digits; at least 2010 and at most 2020.

eyr (Expiration Year) - four digits; at least 2020 and at most 2030.

hgt (Height) - a number followed by either cm or in:

If cm, the number must be at least 150 and at most 193.

If in, the number must be at least 59 and at most 76.

hcl (Hair Color) - a \# followed by exactly six characters 0-9 or a-f.

ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.

pid (Passport ID) - a nine-digit number, including leading zeroes.

cid (Country ID) - ignored, missing or not.

Your job is to count the passports where all required fields are both
present and valid according to the above rules. Here are some example
values:

byr valid: 2002

byr invalid: 2003

hgt valid: 60in

hgt valid: 190cm

hgt invalid: 190in

hgt invalid: 190

hcl valid: \#123abc

hcl invalid: \#123abz

hcl invalid: 123abc

ecl valid: brn

ecl invalid: wat

pid valid: 000000001

pid invalid: 0123456789

Here are some invalid passports:

eyr:1972 <cid:100>

hcl:\#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019

hcl:\#602927 eyr:1967 hgt:170cm

ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012

ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 <cid:277>

hgt:59cm ecl:zzz

eyr:2038 hcl:74454a iyr:2023

pid:3556412378 byr:2007

Here are some valid passports:

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:\#623a2f

eyr:2029 ecl:blu <cid:129> byr:1989 iyr:2014 pid:896056539 hcl:\#a97842
hgt:165cm

hcl:\#888785 hgt:164cm byr:2001 iyr:2015 <cid:88> pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:\#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719

**Count the number of valid passports - those that have all required
fields and valid values. Continue to treat cid as optional. In your
batch file, how many passports are valid?**

``` r
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
```

## Day 5: Binary Boarding

### Part 1:

You board your plane only to discover a new problem: you dropped your
boarding pass\! You aren’t sure which seat is yours, and all of the
flight attendants are busy with the flood of people that suddenly made
it through passport control.

You write a quick program to use your phone’s camera to scan all of the
nearby boarding passes (your puzzle input); perhaps you can find your
seat through process of elimination.

Instead of zones or groups, this airline uses binary space partitioning
to seat people. A seat might be specified like FBFBBFFRLR, where F means
“front”, B means “back”, L means “left”, and R means “right”.

The first 7 characters will either be F or B; these specify exactly one
of the 128 rows on the plane (numbered 0 through 127). Each letter tells
you which half of a region the given seat is in. Start with the whole
list of rows; the first letter indicates whether the seat is in the
front (0 through 63) or the back (64 through 127). The next letter
indicates which half of that region the seat is in, and so on until
you’re left with exactly one row.

For example, consider just the first seven characters of FBFBBFFRLR:

Start by considering the whole range, rows 0 through 127.

  - F means to take the lower half, keeping rows 0 through 63.

  - B means to take the upper half, keeping rows 32 through 63.

  - F means to take the lower half, keeping rows 32 through 47.

  - B means to take the upper half, keeping rows 40 through 47.

  - B keeps rows 44 through 47.

  - F keeps rows 44 through 45.

The final F keeps the lower of the two, row 44.

The last three characters will be either L or R; these specify exactly
one of the 8 columns of seats on the plane (numbered 0 through 7). The
same process as above proceeds again, this time with only three steps. L
means to keep the lower half, while R means to keep the upper half.

For example, consider just the last 3 characters of FBFBBFFRLR:

Start by considering the whole range, columns 0 through 7.

  - R means to take the upper half, keeping columns 4 through 7.

  - L means to take the lower half, keeping columns 4 through 5.

  - The final R keeps the upper of the two, column 5.

So, decoding FBFBBFFRLR reveals that it is the seat at row 44, column 5.

Every seat also has a unique seat ID: multiply the row by 8, then add
the column. In this example, the seat has ID 44 \* 8 + 5 = 357.

Here are some other boarding passes:

  - BFFFBBFRRR: row 70, column 7, seat ID 567.

  - FFFBBBFRRR: row 14, column 7, seat ID 119.

  - BBFFBBFRLL: row 102, column 4, seat ID 820.

**As a sanity check, look through your list of boarding passes. What is
the highest seat ID on a boarding pass?**

``` r
## Read in the data
df <- data.frame(V1 = read_lines("Day5/input_day5.txt"))

## Ensure that the boarding seat specifications do not have leading or trailig spaces
df <- df %>% 
  mutate(V1 = trimws(V1))

## Row number function
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

## Column number function
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
```

### Part 2:

Ding\! The “fasten seat belt” signs have turned on. Time to find your
seat.

It’s a completely full flight, so your seat should be the only missing
boarding pass in your list. However, there’s a catch: some of the seats
at the very front and back of the plane don’t exist on this aircraft, so
they’ll be missing from your list as well.

Your seat wasn’t at the very front or back, though; the seats with IDs
+1 and -1 from yours will be in your list.

**What is the ID of your seat?**

``` r
union_seq = seq(min(df$seatid), max(df$seatid), 1)

answer2 = union_seq[which(!union_seq %in% df$seatid)]
```
