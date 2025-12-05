library(tidyverse)
library(rvest)
library(readxl)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Michigan")

rawdata <- read_excel("Michigan_data.xlsx", sheet = "Sheet2")

cleandata <- rawdata %>% 
  fill(Gender) %>% 
  mutate(
    Event = trimws(Event),
    Year = str_extract(Event, "[ \\t\\n]*[12][90][0-9][0-9] *"),
    Year = gsub("[ \t\n]*", "", Year),
    Class = str_extract(str_to_upper(Event),"(CLASS [ABCD\\-]+|[LU]P DIVISION [1234])")) %>% 
  fill(Year, Class) %>% 
  mutate(Event = case_when(
    `Individual Champion` == "Ewing-Salazar-Grabowski-Wollett" ~ "800 relay",
    Year < 1982 & Event == "100" ~ "100y",
    Year >= 1982 & Event == "100" ~ "100m",
    Event == "800" ~ "800m",
    Event == "880" ~ "880y",
    Event == "1600" ~ "1600m",
    str_to_title(Event) == "Mile" ~ "Mile",
    Event == "High jump" ~ "High Jump",
    str_detect(Event, "Pole vault") ~ "Pole Vault",
    TRUE ~ Event
    ),
    Mark = case_when(
      `Individual Champion` == "Tom Broekema" & Event == "1600m" & Year == 1981 ~"4:21.1",
      `Individual Champion` == "Jerry Curtis" & Event == "Mile" & Year == 1978 ~"4:24", # corrected via original document https://my.mhsaa.com/Portals/0/Documents/BTR/PDF-Archives/1978-BTR-LP-D.pdf
      TRUE ~ Mark
      ),
    Mark = trimws(Mark),
  ) %>%
  #filter(str_detect(Event, "2007"), Gender=="Boys")
  filter(Event %in% c("100y", "100m", "880y", "800m",
                      "Mile", "1600m", "High Jump",
                      "Pole Vault")) %>% 
  mutate(State = "Michigan",
         Mark = trimws(Mark),
         Event = case_when(
           `Individual Champion` == "Kevin Robinson" ~ "100y",
           `Individual Champion` == "Rickey Swilley" ~ "100m",
           `Individual Champion` == "Rob Bramer" ~ "100m",
           `Individual Champion` == "Fred King" ~ "100m",
           `Individual Champion` == "Sandy Harris" ~ "100m",
           `Individual Champion` == "Donna Smith" & Year == 1981 ~ "100m",
           `Individual Champion` == "Andrea Kincannon" & Year == 1981 ~ "100m",
           `Individual Champion` == "Pallastean Harris" ~ "100m",
           `Individual Champion` == "Sue Steeby" ~ "100m",
           `Individual Champion` == "Andreas Laut" ~ "100m",
           `Individual Champion` == "Larry Jordan" ~ "100m",
           `Individual Champion` == "Jerry Campbell" ~ "100m",
           `Individual Champion` == "Penny Zeneberg" ~ "100m",
           TRUE ~ Event
         )) %>% 
  select(State, Year, Gender, Event, Class, Mark)
   
write.csv(cleandata, "michigan_clean.csv", row.names = F)

cleandata %>% 
  count(Year, Gender, Event) %>% 
  print(n=500)

cleandata %>% 
  count(Event) %>% 
  print(n=500)

str_extract(trimws(" 2007"), "^ *[12][90][0-9][0-9] *$")
str_extract("	
 2007", "^[ \\t\\n]*[12][90][0-9][0-9] *$") %>% 
  gsub("[ \\t\\n]*", "", .) %>% 
  as.numeric(.)

h<- read_html("https://my.mhsaa.com/Sports/Girls-Track-Field/Individual-Champions/1980s", mode="wb")
  html_table(h) 
headers<-  html_elements(h,"h3") %>% 
    html_text()

headers[4:length(headers)]
t<-html_text(h) %>% 
  tibble(text=.) %>% 
  separate_longer_delim(text,delim="\r\n")
