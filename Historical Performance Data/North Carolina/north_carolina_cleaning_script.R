library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\North\ Carolina\\")
rawdata <- lapply(list("tabula-WTnF-Event-Champions-by-Event-020223.csv",
                       "tabula-MTnF-Event-Champions-by-Event-020223.csv"),read.csv, header=F) |> 
  bind_rows(.id="Gender")

cleandata <- rawdata |> 
  mutate(All = ifelse(str_detect(V1, "^[0-9][0-9][0-9][0-9]$"),
                      paste(V2,V3,V4),
                      paste(V1,V2,V3,V4)),
         V1 = ifelse(str_detect(V1, "^[0-9][0-9][0-9][0-9]$"),
                     V1,
                     NA)) |> 
  rename(Year = V1) |> 
  select(Gender, Year, All)

for(i in nrow(cleandata):1) {
  if(is.na(cleandata[i,2])){
    cleandata[i-1,3] <- (paste(cleandata[i-1,3], cleandata[i,3]))
  }  
}  

cleandata <- cleandata |> 
  drop_na(Year) |> 
  separate_wider_regex(All, c(V1="[A-Za-z\\-\\.\\&’ ]+ [0-9\\.:’”*]+",
                              " ",
                              V2="[A-Za-z\\-\\.\\&’ ]+ [0-9\\.:’”*]+",
                              " ",
                              V3="[A-Za-z\\-\\.\\&’ ]+ [0-9\\.:’”*]+",
                              " ",
                              V4="[A-Za-z\\-\\.\\&’ ]+ [0-9\\.:’”*]+"),
                       too_few="align_start") |> 
  pivot_longer(cols=c(V1,V2,V3,V4), values_to="All") |> 
  select(Gender, Year, All) |> 
  drop_na(All) |> 
  separate_wider_regex(All, c(Name="[A-Za-z\\-\\. \\&’]+",
                              " ",
                              Mark="[0-9\\.:’”*]+")) |> 
  mutate(Gender = ifelse(Gender ==1, "Girls", "Boys"),
         Event = case_when(
           str_detect(Mark, "^ ?[019][01239]?[\\.:][0-9]*$") ~ "100m",
           str_detect(Mark, "[12]:[0-9][0-9][\\.:][0-9]*") ~ "800m",
           str_detect(Mark, "[345]:[0-9][0-9]\\.?[0-9]*") ~ "1600m",
           str_detect(Mark, "^ ?([456][”’\\'][0-9\\.’]+”$)") ~ "High Jump",
           (Gender=="Girls" & str_detect(Mark, "^ ?([7][”’\\'][0-9\\.’]+”$)")) ~ "Pole Vault",
           (Gender=="Boys" & str_detect(Mark, "^ ?([7][”’\\'][0-9\\.’]+”$)")) ~ "High Jump",
           str_detect(Mark, "^ ?(1?[0-9][:’\\'-][0-9\\.’]+”$)") ~ "Pole Vault",
           TRUE ~ NA
         ),
         Event = ifelse(Event == "100m" & Year < 1980, "100y", Event),
         Event = ifelse(Event == "800m" & Year < 1980, "880y", Event),
         Event = ifelse(Event == "1600m" & Year < 1980, "Mile", Event),
         State = "North Carolina",
         Year = as.numeric(Year))

write.csv(cleandata, "north_carolina_clean.csv", row.names = F)

cleandata |> 
  count(Year, Gender, Event) |> 
  filter(Year >= 1978, Year <= 2019) |> 
  print(n=400)
  