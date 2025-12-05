library(tidyverse)
library(readxl)
library(janitor)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Maryland\\")

girlsraw <- read_excel("Girls\ selected.xlsx") |> 
  rename(...1 = `MPSSAA Girls Track and Field All-Time State Champions`)
boysraw <- read_excel("Boys\ selected.xlsx")
girls.add <- read.csv("tabula-girls_updated.csv") %>% 
  rbind(colnames(.),.) %>% 
  rename(v1=1)
boys.add <- read.csv("tabula-boys_updated.csv")%>% 
  rbind(colnames(.),.)%>% 
  rename(v1=1)

raw.add <- bind_rows(list(girls.add, boys.add), .id="Gender")

clean.add <- raw.add %>% 
  mutate(text = paste(v1, X, sep=" ")) %>% 
  select(Gender,text) %>% 
  mutate(Event = str_extract(text, "Event.*"),
         Event = str_extract(gsub("\\."," ",Event), "([816]+00 Meters|880 Yards|Mile|Pole Vault)"),
         Gender = ifelse(Gender==1, "Girls", "Boys")) %>% 
  separate_wider_regex(text, c(Year = "[12][90][0-9][0-9]",
                               " *",
                               Class = "(?:[1234]A|AA|[ABC]+|Comb)",
                               " +",
                               Name = "[^0-9]+",
                               " *",
                               Mark.Raw = "[0-9\\-\\.:\'\" y]+"), too_few="align_start") %>% 
  
  mutate(
    Mark = str_extract(Mark.Raw, "[0-9]*[:\\.-]*[0-9]*[:\\.-]*[0-9]+( [1-9]/[1-9])*"),
    Mark.Note = str_extract(Mark.Raw, "[why]*$")
  ) %>% 
  fill(Event) %>% 
  mutate(Event = gsub(" Meters", "m", Event)) %>% 
  drop_na(Mark)

clean.add %>% 
  count(Year, Event, Gender) %>% 
  print(n=500)

clean_chunk <- function(rawdata, event, gender, cols){
  cleandata <- rawdata |> 
    rename(Year = cols[1],
           Class = cols[2],
           Mark.Raw = cols[3]) |> 
    mutate(Year = ifelse(is.na(Year), str_extract(Class, "^[0-9]+"), Year)) |> 
    select(Year, Class, Mark.Raw) |> 
    remove_empty("rows") |> 
    mutate(Event = event,
           Gender = gender,
           Mark.Raw = gsub("P00", "POO", Mark.Raw),
           Mark.Raw = gsub("1MB", "IMB", Mark.Raw),
           Mark.Raw = gsub("CR0", "CRO", Mark.Raw),
           Mark.Raw = gsub("C7", "CTT", Mark.Raw),
           Mark.Raw = gsub(";", ":", Mark.Raw),
           Mark = str_extract(Mark.Raw, "[0-9]*[:;\\.-]*[0-9]*[:;\\.-]*[0-9]+( [1-9]/[1-9])*"),
           Mark.Note = str_extract(Mark.Raw, "[why]*$")) |> 
    select(Year, Gender, Event, Class, Mark.Raw, Mark, Mark.Note)
  return(cleandata)
}

cleandata <- bind_rows(
  clean_chunk(boysraw[3:101,1:3], "100y", "Boys",
              c("MPSSAA Boys Track and Field All-Time State Champions", "...2", "...3")),
  clean_chunk(boysraw[3:33,4:6], "100y", "Boys", c("...4","...5","...6")) ,
  
  clean_chunk(boysraw[36:99,4:6], "100m", "Boys", c("...4","...5","...6")),
  
  clean_chunk(boysraw[3:100,], "100m", "Boys", c("...8","...9","...10")),
  
  clean_chunk(mutate(boysraw[108:135,], ...3 = coalesce(...3,...4)),
              "100m", "Boys",
              c("MPSSAA Boys Track and Field All-Time State Champions", "...2", "...3")),
  clean_chunk(boysraw[297:299,1:3], "440y", "Boys",
              c("MPSSAA Boys Track and Field All-Time State Champions", "...2", "...3")),
  clean_chunk(boysraw[201:300,], "440y", "Boys", c("...4","...5","...6")),
  clean_chunk(boysraw[201:223,], "440y", "Boys", c("...7","...8","...9")),
  clean_chunk(boysraw[228:296,], "400m", "Boys", c("...7","...8","...9")),
  clean_chunk(boysraw[306:393,], "400m", "Boys",
              c("MPSSAA Boys Track and Field All-Time State Champions", "...2", "...3")),
  clean_chunk(boysraw[345:392,], "880y", "Boys", c("...4","...5","...6")),
  clean_chunk(boysraw[306:374,], "880y", "Boys", c("...7","...8","...9")),
  clean_chunk(boysraw[380:393,], "800m", "Boys", c("...7","...8","...9")),
  clean_chunk(boysraw[400:497,], "800m", "Boys",
              c("MPSSAA Boys Track and Field All-Time State Champions", "...2", "...3")),
  clean_chunk(boysraw[400:482,], "800m", "Boys", c("...4","...5","...7")),
  clean_chunk(boysraw[505:601,], "High Jump", "Boys", c("...5","...6","...7")),
  clean_chunk(boysraw[505:603,], "High Jump", "Boys", c("...8","...9","...10")),
  clean_chunk(boysraw[610:704,], "High Jump", "Boys",
              c("MPSSAA Boys Track and Field All-Time State Champions", "...2", "...3")),
  clean_chunk(boysraw[610:613,], "High Jump", "Boys", c("...4","...5","...6")),
  clean_chunk(girlsraw[4:98,], "100m", "Girls", c("...1","...2","...3")),
  clean_chunk(girlsraw[4:100,], "100m", "Girls", c("...4","...5","...6")),
  clean_chunk(girlsraw[4:30,], "100m", "Girls", c("...7","...8","...9")),
  clean_chunk(girlsraw[170:192,], "400m", "Girls", c("...6","...7","...8")),
  clean_chunk(girlsraw[109:193,], "400m", "Girls", c("...9","...10","...11")),
  clean_chunk(girlsraw[201:296,], "400m", "Girls", c("...1","...2","...3")),
  clean_chunk(girlsraw[211:294,], "800m", "Girls", c("...4","...5","...6")),
  clean_chunk(girlsraw[201:296,], "800m", "Girls", c("...8","...9","...10")),
  clean_chunk(girlsraw[304:335,], "800m", "Girls", c("...1","...2","...3")),
  clean_chunk(girlsraw[407:485,], "High Jump", "Girls", c("...7","...8","...9")),
  clean_chunk(girlsraw[495:583,], "High Jump", "Girls", c("...1","...2","...3")),
  clean_chunk(girlsraw[495:530,], "High Jump", "Girls", c("...4","...5","...6")) ,
  clean.add
)

cleandata <- cleandata |> 
  mutate(Mark = gsub("-", "'", Mark),
         Mark = gsub(" 1/2", ".5", Mark),
         Mark = gsub(" 1/4", ".25", Mark),
         Mark = gsub(" 3/4", ".75", Mark),
         Mark = gsub(" 7/8", ".875", Mark),
         Mark = trimws(Mark),
         Mark.Note = ifelse(Mark.Note=="", NA, Mark.Note),
         Event = case_when(
           (Event == "100m" & Mark.Note == "y") ~ "100y",
           (Event == "400m" & Mark.Note == "y") ~ "440y",
           (Event == "800m" & Mark.Note == "y") ~ "880y",
           Class == "Comb - 60 yds." ~ "60y",
           TRUE ~ Event
         ),
         Mark = case_when(
           Year==2004&Gender=="Girls"&Event=="1600m"&Class=="4A" ~ "4:55.75",
           Year==2019&Gender=="Boys"&Event=="800m"&Class=="3A" ~ "1:56.37",
           TRUE ~ Mark
         ),
         State = "Maryland") |> 
  filter(Event %in% c("100y", "100m", "800m", "880y", "Mile", "1600m",
                      "High Jump", "Pole Vault")) %>% 
  select(State, Year, Gender, Event, Class, Mark) %>% 
  distinct()

cleandata %>% 
  filter(Year >=1978) %>% 
  count(Year, Gender, Event) %>% 
  print(n=500)

write.csv(cleandata, "maryland_clean.csv", row.names = F)


