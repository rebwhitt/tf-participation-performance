library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Kansas\\")

rawdata <- lapply(list("tabula-girlstrackindivwinners.csv",
                       "tabula-boystrackindivwinners.csv"),read.csv, header=F) |> 
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

convert_dates("Oct-00")
convert_dates("6-Oct")

cleandata <- rawdata |> 
  mutate(All = paste(V1, V2, V3, V4)) |> 
  select(Gender, All) |> 
  mutate(t =str_detect(All, "^[12][90][0-9][0-9] [A-Za-z ,\\-\\’\\.\\(\\)]+ [0-9\\-\\.:h^(May|Nov|Aug|Oct|Apr|Sep|Dec)]+ *$")) |> 
  filter(t) |> 
  separate_wider_regex(All, c("Year" = "^[12][90][0-9][0-9]",
                              " ",
                              "Name" = "[A-Za-z ,\\-\\’\\(\\)\\.]+",
                              " ",
                              "Mark" = " *[0-9\\-\\.:h^(May|Aug|Nov|Oct|Apr|Sep|Dec)]+ *$")
                       ) |> 
  select(-t) |> 
  mutate(
    Mark = ifelse(str_detect(Mark, "(Apr|May|Aug|Sep|Oct|Nov|Dec)"),
                   convert_dates(Mark),
                  Mark),
    Mark.Note = str_extract(Mark, "[\\^h]$"),
    Mark.Note = str_replace(Mark.Note, "\\^", "w"),
    Mark = str_replace(Mark, "h", ""),
    Mark = str_replace(Mark, "\\^", ""),
    Event = case_when(
      str_detect(Mark, "^ ?[019][01239]?[\\.:]?[0-9]* *$") ~ "100m",
      str_detect(Mark, "[12]:[0-9][0-9][\\.:][0-9]* *") ~ "800m",
      str_detect(Mark, "[345]:[0-9][0-9]\\.?[0-9]* *") ~ "1600m",
      str_detect(Mark, "^ ?([4567][\\-][0-9]|[12][0-9\\.]*m$)") ~ "High Jump",
      str_detect(Mark, "^ ?([189][0-9]?[’\\'-][0-9]|[345]\\.+[0-9]*m?$)") ~ "Pole Vault",
      TRUE ~ NA
    ),
    Event = case_when(
      Name == "McCoy Assumption" ~ "100y",
      Name == "Hill Bryan Station" & Year == 1978 ~ "100y",
      Name == "Ward Newport" & Year == 1979 ~ "100y",
      Name == "Boyd Harrodsburg" & Year == 1978 ~ "100y",
      Name == "Jones Pleasure Ridge Park " & Year ==1978 ~ "Mile",
      Name == "Martin  Central" ~ "880y",
      Name == "Beumel  Apollo" & Event == "1600m" & Year == 1978 ~ "Mile",
      Name == "Beumel  Apollo" & Event == "800m" & Year == 1978 ~ "880y",
      Name == "Foster  Owen County" ~ "Mile",
      Name == "Hood Fort Campbell " & Year == 1978 ~ "880y",
      Year <= 1978 & Event == "100m" ~ "100y",
      Year <= 1978 & Event == "800m" ~ "880y",
      Year <= 1978 & Event == "1600m" ~ "Mile",
      Year == 1979 & Name == "Alexander Trigg County" ~ "100y",
      Year == 1979 & Name == "Choo Choo Lee Paducah Tilghman" ~ "100y",
      TRUE ~ Event
    ),
    Gender = ifelse(Gender==1, "Girls", "Boys"),
    State = "Kansas"
  ) #|> 
  #select(State,Year,Event,Gender,Mark,Mark.Note)

write.csv(cleandata, "kansas_clean.csv", row.names = F)
  
cleandata |> 
  count(Year, Gender, Event) |> 
  #filter(n < 3, Year > 1977) |> 
  print(n=500) 

#str_extract("11-06 ", " *[0-9\\-\\.:h]+ *$")
#str_extract("2012 Katherine Receveur  Assumption 2:15.50 ", "^[12][90][0-9][0-9] [A-Za-z ,\\-\\’\\.]+ [0-9\\-\\.:h]+ *$")
#str_detect("2012 Katherine Receveur  Assumption 2:15.50 ", "^[12][90][0-9][0-9] [A-Za-z ,\\-\\’]+ [0-9\\-\\.:h]+ *$") 
