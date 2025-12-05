library(tidyverse)
library(readxl)

# * Meet Record
# ** State and Meet Record
# w Wind-aided meet record
# ^ Ties Meet Record

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\New York")

# Read and combine into one dataframe
rawdata <- lapply(list("NYS GIRLS OUTDOOR STATE CHAMPIONS.xlsx", "NYS BOYS OUTDOOR STATE CHAMPIONS.xlsx"),
       read_excel) |> 
  bind_rows(.id = "Gender")

# Clean dataframe
cleandata <- rawdata |> 
  rename(Text = `NYS Girls Outdoor Track & Field Champions`,
         Name = ...2,
         School = ...3,
         Mark = ...4) |> 
  mutate(Text = coalesce(Text, `NYS Boys Outdoor Track & Field Champions`),
         Gender = ifelse(Gender == 1, "Girls", "Boys")) |> 
  mutate(Year = str_extract(Text, "^([12][90][0-9][0-9]( +.*)?| *(RETIRED|STATE MEET) RECORDS.+|NYS Outdoor Boys Meet Records)$"),
         Year = ifelse(is.na(Year), str_extract(Name, "RETIRED YARD RECORDS"), str_extract(Year, "^([12][90][0-9][0-9]|(RETIRED|STATE MEET) RECORDS.+|NYS Outdoor Boys Meet Records)")),
         Event = str_extract(Text, "^([0-9x]+0 ?(mH?|yd)( Relay| RW| Hurdles| ?Walk)?|Mile( Walk)?|2 Mile( Walk)?|High Jump|Long Jump|Pole Vault|Discus|Javelin|Shot Put|Pentatha?lon|Triple Jump)"),
         Class = str_extract(Text, "(Class [ABCD]|Intersectional|D-[12]|Federation|Wheelchair)"))|>
  select(Gender,Year, Event, Class, Text, Name, School, Mark) |> 
  mutate(Event = gsub(" ?yd", "y", Event),
         Mark = gsub("II", "\"", Mark)) |> 
  fill(Year, Event, Class) |> 
  filter(str_detect(Year,"^[12][90][0-9][0-9]" )) |> 
  filter(Event %in% c("100y", "100m", "880y", "800m", "Mile", "1500m", "1600m",
                      "High Jump", "Pole Vault")) |> 
  filter(!(Event %in% c("800m", "1600m") & Class == "Wheelchair")) %>% 
  mutate(Year = as.numeric(Year),
         State="New York") |> 
  drop_na(Mark) |> 
  rename(Mark.raw = Mark) |> 
  filter(Mark.raw != "Performance") |> 
  separate_wider_regex(Mark.raw, c("(?:[0-9\\.]+ [\\*\\(P\\),/]+)?",
                                  Mark="[0-9\\.:\"\'\\- ]+",
                                  Mark.Note = "[\\*\\^wA#HT ]*")) |> 
  select(State, Year, Gender, Event, Class, Text, Mark, Mark.Note)

# Write out file
write.csv(cleandata, "new_york_clean.csv", row.names = F)

# Checking all made it through
cleandata |> 
  filter(Year >= 1978) |> 
  count(Year, Gender, Event) |> 
  print(n=500)
