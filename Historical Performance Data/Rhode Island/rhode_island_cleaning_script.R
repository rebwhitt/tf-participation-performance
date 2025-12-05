library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Rhode\ Island\\")
rawdata <- lapply(list("Girls Outdoor Track Division, Class, State Meets - States.csv",
                       "Boys Outdoor Track Division, Class, State Meets - State.csv"),read.csv) |> 
  bind_rows(.id="Gender")

cleandata <- rawdata |> 
  rename(Event = EVENT,
         Year = YEAR,
         Mark = PERFORMANCE) |> 
  select(-X, -MEET, -X.4) |> 
  mutate(Event = case_when(
    Event == "100 Meters" ~ "100m",
    Event == "100 Yards" ~ "100y",
    Event == "100 Yd" ~ "100y",
    Event == "1500 Meters" ~ "1500m",
    Event == "880 Yards" ~ "880y",
    Event == "880 Yd" ~ "880y",
    Event == "800 Meters" ~ "800m",
    Event == "Mile Run" ~ "Mile",
    TRUE ~ Event
  )) |> 
  filter(Event %in% c("100y", "100m", "880y", "800m",
                      "Mile", "1500m", "High Jump", "Pole Vault")) |> 
  filter(PLACING %in% c("1", "T-1")) |> 
  mutate(Gender = ifelse(Gender==1, "Girls", "Boys"),
         State = "Rhode Island",
         Year = as.numeric(Year)) |> 
  select(State, Year, Event, Gender, Mark) |> 
  distinct()

write.csv(cleandata, "rhode_island_clean.csv", row.names = F)

cleandata |> 
  count(Year) |> 
  filter(Year > 1977, Year < 2020)

cleandata |> 
  count(X, MEET, X.4)
cleandata |> 
  count(PLACING)
