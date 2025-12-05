library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Wisconsin\\")
rawdata <- lapply(list("Wisconsin Girls.csv",
                       "Wisconsin Boys.csv"),read.csv) |> 
  bind_rows(.id="Gender")

cleandata <- rawdata |> 
  rename(Mark = Time.Mark) |> 
  mutate(Gender = ifelse(Gender == 1, "Girls", "Boys"),
         Year = ifelse(Year == "202412", "2024-2", Year),
         Year = ifelse(str_detect(Year, "[0-9][0-9][0-9][0-9][A-C]"), gsub("C", "-C", Year), Year )) |> 
  filter(!Mark %in% c("", "Time/Mark"), !is.na(Mark)) |> 
  separate_wider_delim(cols=Year, names=c("Year", "Class"), delim="-",
                       too_few = "align_start") |> 
  mutate(
    
    Year = as.numeric(Year),
    Event = ifelse(Event == "100 Dash",
                   ifelse( Year < 1980, "100y", "100m"),
                   Event),
    Event = str_replace(Event, "Mile Run", "Mile"),
    Event = str_replace(Event, " Dash", "m"),
    Event = str_replace(Event, " Meters", "m"),
    Event = str_replace(Event, " Meter", "m"),
    Event = str_replace(Event, " Run", "m"),
    Event = str_replace(Event, " run", "m"),
    Event = ifelse(Event == "880m", "880y", Event),
    Event = ifelse(Event == "", NA, Event),
    Mark = ifelse(Event == "Pole Vault" & Mark == "11.00",
                  "11-0",
                  Mark),
    Mark = ifelse((Mark == "10-6" & Event!="Pole Vault"), "6-10", Mark)) |> 
  filter(!(Event == "1600m" & str_detect(Mark, "1:[0-9][0-9]\\.[0-9][0-9]"))) %>% 
  filter(Event %in% c("100y", "100m", "880y", "800m",
                      "Mile", "1600m", "High Jump", "Pole Vault")) |> 
  mutate(State = "Wisconsin") |> 
  select(State, Year, Gender, Event, Class, Mark)

write.csv(cleandata, "wisconsin_clean.csv", row.names = F)
  
cleandata |> 
  count(Event) |> 
  print(n=100)

cleandata |> 
  count(Year) |> 
  print(n=200)
