library(tidyverse)
library(readxl)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Alaska\\")
rawdata <- read_excel("1975-2003-Track-Field-Results-Awards.xlsx")

cleandata <- rawdata |> 
  rename(Year = `Track & Field State Results 1975 - 2003`,
         Event = ...2,
         Mark.Boys = ...5,
         Mark.Girls = ...8) |> 
  select(Year, Event, Mark.Boys, Mark.Girls) |> 
  mutate(Event = ifelse(is.na(Year), Event, ifelse(Year == "Discus", "Discus", Event)),
         Year = as.numeric(Year),
         State = "Alaska",
         Event = gsub(" Meters", "m", Event)) |> 
  fill(Year) |> 
  filter(Event %in% c("100m", "400m", "800m", "High Jump")) |> 
  select(Year, State, Event, Mark.Boys, Mark.Girls)

write.csv(cleandata, "alaska_clean.csv", row.names = F)
