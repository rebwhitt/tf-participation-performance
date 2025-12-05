library(tidyverse)
library(janitor)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Massachusetts\\")
rawdata <- read.csv("Massachusetts_times.csv")

extract_event <- Vectorize(function(evt){
  return(
    strsplit(strsplit(evt, ") ")[[1]][2], "[0-9\\.:-]+$")
  )
})

cleandata <- rawdata |> 
  row_to_names(1) |> 
  mutate(Year = str_extract(Year, "^[0-9]+"),
         Gender = case_when(
           str_detect(Event, "^\\(B\\)") ~ "Boys",
           str_detect(Event, "^\\(G\\)") ~ "Girls",
           TRUE ~ NA
           ),
         Mark = str_extract(Event, "[0-9\\.,:\\-\"\']+( \\(.*y\\))?$"),
         Event = extract_event(Event),
         State = "Massachusetts",
         Mark = gsub("-", "'", Mark),
         Mark = gsub(" \\(.*y\\))?$", "", Mark),
         Event = gsub("M", "m", Event),
         Event = gsub("1 mILE", "Mile", Event),
         Event = gsub("m[0-9].*", "m", Event),
         Event = ifelse(str_detect(Event, "High Jump"),"High Jump", Event),
         Event = ifelse(str_detect(Event, "Pole Vault"),"Pole Vault", Event)
         ) |> 
  filter(Champion != "----") %>% 
  select(Year, State, Gender, Event, Mark) |> 
  filter(Event %in% c("100y / 100m",
                      "880y / 800m",
                      "Mile",
                      "High Jump",
                      "Pole Vault"
                      )) %>% 
    mutate(Event = 
      ifelse(str_detect(Event, "..0y / ..0m"),
             ifelse(Year >= 1982,
                    str_extract(Event, "..0m"),
                    str_extract(Event, "..0y")),
             Event))

cleandata %>% 
  count(Year, Gender)

cleandata %>% 
  filter(Year<1982, Event == "100y / 100m", Gender == "Boys") %>% 
  summarize(mean = mean(as.numeric(Mark)))
cleandata %>% 
  filter(Year>=1982, Event == "100y / 100m", Gender == "Boys") %>% 
  summarize(mean = mean(as.numeric(Mark)))
cleandata %>% 
  filter(Event == "100y / 100m", Gender == "Boys") %>% 
  summarize(mean = mean(as.numeric(Mark)))

cleandata %>% 
  filter(Event == "100y / 100m", Gender == "Girls") %>% 
  ggplot(aes(x=Year, y=as.numeric(Mark)))+
  geom_point()

write.csv(cleandata, "massachusetts_clean.csv", row.names = F)

