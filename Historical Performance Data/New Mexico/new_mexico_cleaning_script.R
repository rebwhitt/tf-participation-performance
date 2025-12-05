library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\New\ Mexico\\interim")

rawdata <- lapply(list.files(pattern="tabula-.*\\.csv"), read.csv, header=F) %>% 
  bind_rows()

event_str <- "((4?x?([1-8]?[0-9]0+(y| ?Y| Yard)|[1-9][0-9]*[05](m| ?M| Meter))|([12] )?Mile)( Run| Dash| Hurdles| ?H| (Medley )?Relay)?)|((High|Long|Triple) Jump|Discus|Hammer|Javelin|Pole Vault|Shot Put|Softball Throw)"

data<- rawdata %>% 
  mutate(Text = paste(V1,V2,V3,V4)) %>% 
  select(Text)%>% 
  mutate(
    Year = str_extract(Text, "^[12][90][0-9][0-9]"),
    Gender = str_extract(Text, "(Girls|Boys)"),
    Event = str_extract(Text, event_str)
  ) %>% 
  fill(Year, Gender, Event) %>% 
  mutate(Text = gsub("\\’ \\-","\\’\\-", Text),
    Mark = str_extract(Text, "[0-9][0-9:\\.\\-\\'\"\\’\\”\\-]+[0-9\\’\\”]m? *$|(NM|NT|NH|DNF|DQ|FS) *$"),
         Class = str_extract(Text, "^[1-9]?[ABCD]+ ")) %>% 
  filter(!is.na(Mark))%>% 
  mutate(
    State="New Mexico",
    Event.raw = Event,
    Event = gsub(" Y ( Dash| Run)?","y ",Event),
    Event = trimws(gsub(" M( Dash| Run| |$)", "m ", Event)),
    Event = gsub(" Run","", Event),
    Event = gsub("1 Mile","Mile", Event)
  )
setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\New\ Mexico\\")
write.csv(data, file="new_mexico_clean.csv", row.names=F)

data %>% 
  select(Event, Event.raw) %>% 
  distinct() %>% 
  arrange(Event)
