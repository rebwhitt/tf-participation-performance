library(tidyverse)
library(readxl)
library(pdftools)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Participation\ Data\\")

ps <- read_excel("participation_statistics.xlsx")

ps <- ps |> 
  mutate(Year = as.numeric(str_sub(Year, start=-4)))

boys79raw <- pdf_text("1979 Boys.pdf")
cat(boys79raw[1])

all2023raw <- pdf_text("2022-23_participation_survey.pdf")
cat(all2023raw[1])

boys74raw <- read_excel("1973-74 Boys.xlsx")
girls74raw <- read_excel("1973-74 Girls.xlsx")

boys76raw <- read_excel("1975-76 Boys.xlsx", col_types="text")
girls76raw <- read_excel("1975-76 Girls.xlsx")

boys78raw <- read_excel("1978 Boys.xlsx")
girls78raw <- read_excel("1978 Girls.xlsx")

all8084raw <- read_excel("80-84\ all.xlsx")
  
clean_data <- function(rawdata,year,gender, cols){
  cleandata <- rawdata |> 
    rename(State = cols[1],
           Schools = cols[2],
           Participants = cols[3]) |> 
    mutate(Year = year,
           Gender = gender) |> 
    select (Year, Gender, State, Schools, Participants) |> 
    filter(!State %in% c("STATE-",
                         "COUNTRY",
                         "STATE-COUNTRY",
                         "STATE•COUNTRY",
                         "sTATE.G0LINTAY")) |> 
    drop_na(State)
  
  return(cleandata)
}


boys78 <- clean_data(boys78raw, 1978,"Boys", c("Track and Field", "...9", "...10"))

  

boys74 <- boys74raw |> 
  rename(State = ...1,
         Schools = ...8,
         Participants = ...9) |> 
  mutate(State = coalesce(State, ...2),
         No.Schools = coalesce(as.character(...7), Schools),
         Year = 1974,
         Gender = "Boys") |> 
  select (Year, Gender, State, Schools, Participants) |> 
  filter(!State %in% c("STATE-", "COUNTRY")) |> 
  drop_na(State)

girls74a <- girls74raw |> 
  select(...10,`TRACK & FIELD`,...12) |> 
  rename(State = ...10,
         Schools = `TRACK & FIELD`,
         Participants = ...12) |>
  filter(!State %in% c("STATE-COUNTRY", "No.")) |> 
  drop_na(State) |> 
  filter(!grepl("^[0-9]+$", State))

girls74b <- girls74raw[73:133,] |> 
  rename(State = ...1,
         Schools = SKIING,
         Participants = ...3) |> 
  select (State, Schools, Participants) |> 
  drop_na(State)

girls74c <- girls74raw[73:133,] |> 
  rename(State = TENNIS,
         Schools = ...10,
         Participants = `TRACK & FIELD`) |> 
  select (State, Schools, Participants) |> 
  drop_na(State)

girls74 <- bind_rows(list(girls74a, girls74b, girls74c)) |> 
  mutate(Year = 1974,
         Gender = "Girls") |> 
  select (Year, Gender, State, Schools, Participants)

boys76 <- boys76raw |> 
  rename(State = ...1,
         Schools = `Track (Outdoor)`,
         Participants = ...8) |> 
  mutate(Year = 1976,
         Gender = "Boys") |> 
  select (Year, Gender, State, Schools, Participants) |> 
  filter(!State %in% c("COUNTRY-", "STATE", "'Figures based on")) |> 
  drop_na(State)

girls76 <- girls76raw |> 
  rename(State = ...1,
         Schools = `Track (Outdoor)`,
         Participants = ...5) |> 
  mutate(Year = 1976,
         Gender = "Girls") |> 
  select (Year, Gender, State, Schools, Participants) |> 
  filter(!State %in% c("COUNTRY-", "STATE", "VW")) |> 
  drop_na(State)

girls78 <- clean_data(girls78raw[70:144,], 1978, "Girls", c("snnis","...4","...5"))



cleandata <- bind_rows(list(ps,boys74, girls74, boys76, girls76, boys78, girls78)) |> 
  mutate(State = str_to_title(State),
         State = gsub(",", "\\.", State),
         State = gsub("'", "", State),
         State = gsub("\"", "", State),
         State = gsub("N\\. Hampshire", "New Hampshire", State),
         State = gsub("N\\. Jersey", "New Jersey", State),
         State = gsub("N\\. York", "New York", State),
         State = gsub("N\\. Mexico", "New Mexico", State),
         State = gsub("W\\.", "West", State),
         State = gsub("N\\.", "North", State),
         State = gsub("No\\.", "North", State),
         State = gsub("S\\.", "South", State),
         State = gsub("So\\.", "South", State),
         State = gsub("Hamps\\.", "Hampshire", State),
         State = gsub("Penn\\.", "Pennsylvania", State),
         State = gsub("15\\.", "Island", State),
         State = gsub("Is\\.", "Island", State),
         State = gsub("Marylano", "Maryland", State),
         State = gsub("Idwa", "Iowa", State),
         State = ifelse((State =="Washington" &
                  Gender == "Girls" &
                  Year == 1974 &
                  Schools == 67), "Wyoming", State)) # fix presumed typo in original data

write.csv(cleandata, "participation_clean.csv", row.names=F)
