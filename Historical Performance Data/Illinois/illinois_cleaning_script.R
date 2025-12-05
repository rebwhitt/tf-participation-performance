library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Illinois\\")
rawdata <- lapply(list("Illinois Girls.csv",
                       "Illinois Boys.csv"),read.csv, header=F) |> 
  bind_rows(.id="Gender")

convert_dates <- Vectorize(function(raw){
  spl <- str_split(raw, "-")
  inches <- spl[[1]][1]
  feet <- spl[[1]][2]
  inches <- str_replace_all(inches, " ", "")
  feet <- str_replace_all(feet, " ", "")
  if(str_detect(inches, "[A-Za-z]+")){
    inches <- case_when(
      inches == "Apr" ~ 4,
      inches == "May" ~ 5,
      inches == "Jul" ~ 7,
      inches == "Aug" ~ 8,
      inches == "Sep" ~ 9,
      inches == "Oct" ~ 10,
      inches == "Nov" ~ 11,
      inches == "Dec" ~ 12,
      TRUE ~ NA
    )
    return(paste(inches, "-", feet, sep=""))
  } else {
    feet <- case_when(
      feet == "Apr" ~ 4,
      feet == "May" ~ 5,
      feet == "Jul" ~ 7,
      feet == "Aug" ~ 8,
      feet == "Sep" ~ 9,
      feet == "Oct" ~ 10,
      feet == "Nov" ~ 11,
      feet == "Dec" ~ 12,
      TRUE ~ NA
    )
    return(paste(feet, "-", inches, sep=""))
  }
})

cleandata <- rawdata |> 
  rename(Event = V1,
         Year = V2,
         Class = V3,
         Name = V4,
         School = V5,
         Mark = V6) |>
  drop_na(Year) |> 
  mutate(Year = paste(str_extract(Year, "^[12][789012]"),
                       str_extract(Year, "[0-9][0-9]$"), sep=""),
         Year = ifelse(Year == "1900", "2000", Year),
         Year = ifelse(Year == "1800", "1900", Year),
         Event = str_replace(Event, "-yard dash", "y"),
         Event = str_replace(Event, "-yard run", "y"),
         Event = str_replace(Event, "-meter dash", "m"),
         Event = str_replace(Event, "-meter run", "m"),
         Event = str_replace(Event, "1-mile run", "Mile"),
         Event = str_replace(Event, "high jump", "High Jump"),
         Event = str_replace(Event, "pole vault", "Pole Vault"),
         Event = ifelse(Event == "", NA, Event)) |> 
  fill(Event) |> 
  mutate(Mark = ifelse(Event %in% c("High Jump", "Pole Vault") &
                          str_detect(Mark, "[A-Z][a-z][a-z]"),
                        convert_dates(Mark),
                        Mark),
         Gender = ifelse(Gender==1, "Girls", "Boys"),
         State = "Illinois") |> 
  filter(Year != "NANA") |> 
  select(State, Year, Gender, Event, Class, Mark)

write.csv(cleandata, "illinois_clean.csv", row.names = F)

cleandata |> 
  filter(Event %in% c("High Jump", "Pole Vault")) |> 
  count(Mark)
