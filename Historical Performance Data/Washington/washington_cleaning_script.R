library(tidyverse)
library(readxl)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Washington\\")
path<-"Washington\ All.xlsx"
rawdata <- lapply(excel_sheets(path), function(x) read_excel(path, sheet=x)) |> 
  bind_rows(.id="id")

events <- tibble("id"=1:10,"EventGender"=excel_sheets(path))

cleandata <- rawdata |> 
  mutate(Year = coalesce(Year, Date),
         id = as.numeric(id)) |> 
  select(-Date) |> 
  left_join(events, by="id") |> 
  separate_wider_regex(EventGender,
                       c(Gender = "?:^(Girls|Boys)",
                         Event="(?:[0-9]+m|HighJump|PoleVault)$")) |> 
  mutate(Event=gsub("V", " V", Event),
         Event=gsub("J", " J", Event),
         Event = ifelse(Event == "100m" & Year < 1980, "100y", Event),
         Event = ifelse(Event == "800m" & Year < 1981, "880y", Event),
         Event = ifelse(Event == "1600m" & Year < 1981, "Mile", Event),
         State = "Washington") |> 
  filter(str_detect(Year, "^[12][0-9][0-9][0-9]$")) |> 
  rename(Mark = Time) |> 
  mutate(
    Mark.Note = str_extract(Mark, "[wh]$"),
    Mark = str_replace(Mark, "h", ""),
    Mark = str_replace(Mark, "w", ""),
    Year = as.numeric(Year)
  ) |> 
  select(State, Year, Gender, Event, Mark, Mark.Note)

write.csv(cleandata, "washington_clean.csv", row.names = F)

cleandata |> 
  count(Year) |> 
  filter(Year >= 1978, Year <= 2019) |> 
  print(n=100)
