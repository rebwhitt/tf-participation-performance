library(tidyverse)
library(rvest)
library(readxl)
library(tabulapdf)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Georgia\\")

# https://www.ghsa.net/final-results-2011-boys-state-track-meet
# https://www.ghsa.net/sites/default/files/documents/track/2012-GHSA-Boys-State-Final-Results.pdf
# https://www.ghsa.net/sites/default/files/documents/track/2013_GHSA_Boys_State_Meet_Class_1A-Public_Complete_Results.pdf
# https://www.ghsa.net/sites/default/files/documents/track/2013_GHSA_Boys_State_Meet_Class_3A_Complete_Results.pdf
# https://www.ghsa.net/sites/default/files/documents/track/2014_Track_Boys_State_Meet_-_2A_Results.pdf
# https://www.ghsa.net/sites/default/files/documents/track/2015_ghsa_boys_state_meet_class_6a_performance_lists.pdf
# https://www.ptgrouponline.com/Results/2016/HS/GHSA_Boys_Results.htm
# http://www.ptgrouponline.com/Live/2017/HS/GHSA/TF/Albany/
# http://www.ptgrouponline.com/Live/2017/HS/GHSA/TF/Rome/
# http://www.ptgrouponline.com/Live/2017/HS/GHSA/TF/Carrollton/
# ^ 2018
# https://ga.milesplit.com/meets/346234/results/653087/raw#.XwSKhihKhhE
# https://ga.milesplit.com/meets/346233/results/653473/raw#.XwSJ-ihKhhE
# https://ga.milesplit.com/meets/346235/results/653115/raw#.XwSK8ihKhhE


rawdata <- lapply(list("Georgia\ Girls.csv",
                       "Georgia\ Boys.csv"),read.csv) |> 
  bind_rows(.id="Gender")

cleandata <- rawdata |> 
  mutate(Event = ifelse(Event == "", NA, Event)) |> 
  fill(Year, Event) |> 
  mutate(Event = case_when(
    Event == "100 Yd. Dash" ~ "100y",
    Event == "100yd Dash" ~ "100y",
    Event == "100m\n\t\t\tDash" ~ "100m",
    Event == "100m Dash" ~ "100m",
    Event == "880 Yd. Dash" ~ "880y",
    Event == "880 Yd. Run" ~ "880y",
    Event == "880yd Run" ~ "880y",
    Event == "800m\n\t\t\tRun" ~ "800m",
    Event == "800m Run" ~ "800m",
    Event == "1 Mile Run" ~ "Mile",
    Event == "Mile Run" ~ "Mile",
    Event == "1600m\n\t\t\tRun" ~ "1600m",
    Event == "1600m Run" ~ "1600m",
    Event == "Running High Jump" ~ "High Jump",
    Event == "Pole\n\t\t\tVault" ~ "Pole Vault",
    TRUE ~ Event
  ),
  Gender = ifelse(Gender == 1, "Girls", "Boys"),
  Event = ifelse(Year %in% c(1985,1986) & Gender == "Girls" & Event == "High Jump",
                 NA,
                 Event),
  Event = ifelse(Year %in% c(1985,1986) & Gender == "Girls" & Event == "Long Jump",
                 "High Jump",
                 Event),
  Mark = case_when(
    Year == 2002 & Name == "Rivers, Clark" ~ "4:32.30",
    TRUE ~ Mark
  ),
  State = "Georgia"
  ) |> 
  filter(Event %in% c("100y", "100m", "880y", "800m", 
                      "Mile", "1600m", "High Jump", "Pole Vault")) |> 
  select(State, Year, Event, Gender, Class, Mark)



# 2011
html <- read_html("https://www.ghsa.net/final-results-2011-boys-state-track-meet")

events <- html %>% 
  html_elements("b") %>% 
  html_text() %>% 
  data.frame(Event = ., df = c(1:16, 18:33, 35:50, 52:67, 69:84))

tables <- html %>% 
  html_table() %>% 
  lapply(., function(d){
    data.frame(d) %>% 
    mutate_all(as.character)
  }) %>% 
  bind_rows(.id = "df") %>% 
  mutate(df = as.numeric(df)) %>% 
  inner_join(events, by="df") %>% 
  select(df, Event, everything())

ga2011 <- tables %>% 
  mutate(Mark = coalesce(Time, coalesce(Height, Distance)),
         Gender = "Boys",
         State = "Georgia",
         Year = 2011,
         Event = gsub(" METER (DASH|RUN)", "m", Event),
         Event = str_to_title(Event)) %>% 
  filter(Event %in% c("100m", "800m", "1600m", "High Jump", "Pole Vault"),
         Place == "1") %>% 
  select(State, Year, Gender, Event, Mark)

# 2012
#url<-names[[1]]
url <- "https://www.ghsa.net/sites/default/files/documents/track/2012-GHSA-Boys-State-Final-Results.pdf"
url <- "https://www.ghsa.net/sites/default/files/documents/track/2015_ghsa_boys_state_meet_class_6a_performance_lists.pdf"

clean_hytek <- function(url){
  pdf <- extract_text(url)
  event_str <- "([0-9]?(\\.X\\.)?[0-9]+0[\\. ]?(meter|yard|mile|m)[\\. ](run|dash|relay|hurdles)|discus|shot.put|(high|long|triple).jump|pole.vault)"
  mark_str <- "[0-9]+(' )?[0-9\\.\'\":\\-]+ *[%@!#]*( +[\\-\\+]?[0-9\\.NWI]+)? *([0-9]+)? *$"
  ga2012 <- pdf %>% lapply(., function(d){
    data.frame(text = d) %>% 
      separate_longer_delim(text, delim="\n")
  }) %>% 
    bind_rows(.id = "df") %>% 
    mutate(df = as.numeric(df)) %>% 
   # unite(col="text") %>% 
   # separate_wider_delim(text, delim="_", names=c("df", "text"), too_many="merge") %>% 
    mutate(text = gsub("_NA", "", text),
           Event = str_extract(str_to_lower(text), event_str),
           Event = str_to_title(gsub("\\."," ", Event)),
           Event = gsub(" Meter (Dash|Run)", "m", Event),
           Class = str_extract(text, "(Class [A]+|Wheelchair Div(ision)? [123])"),
           Mark.raw = str_extract(text, mark_str),
           Mark.raw = trimws(Mark.raw),
           Mark = str_extract(Mark.raw, "^[0-9]+(' )?[0-9\\.\'\":\\-]+"),
           Race.Type = str_extract(text, "(Preliminaries|Finals|Heat [1-9])"),
           Rank.Raw = trimws(str_extract(text, "^ *[0-9]* ")),
           Rank = str_extract(Rank.Raw, "1?[0-9]$"),
           Gender = "Boys",
           State = "Georgia",
           Year = 2012
    ) %>% 
    fill(Event, Class, Race.Type) %>% 
    filter(Event %in% c("100m", "800m", "1600m", "High Jump", "Pole Vault"),
           Rank == "1",
           Race.Type == "Finals") %>% 
    select(State, Year, Gender, Event, Class, Mark)
  return(ga2012)
}

clean_hytek_v <- Vectorize(clean_hytek)
ga2012 <- clean_hytek("https://www.ghsa.net/sites/default/files/documents/track/2012-GHSA-Boys-State-Final-Results.pdf") 

# 2013

names <- list()
for(i in c("1A-Public",
           "1A-Private",
           "2A",
           "3A",
           "4A",
           "5A",
           "6A")){
  names[length(names)+1]<-paste("https://www.ghsa.net/sites/default/files/documents/track/2013_GHSA_Boys_State_Meet_Class_",i,"_Complete_Results.pdf", sep="")
}
n<-names[1]
files <- list()
for(i in 1:length(names)){
  files[length(files)+1]<-list(clean_hytek(names[[i]][1]))
} 
ga2013 <- files %>% bind_rows(.id="df") %>% 
  inner_join(data.frame(c=c("1A-Public",
                            "1A-Private",
                            "2A",
                            "3A",
                            "4A",
                            "5A",
                            "6A"), df=as.character(c(1:length(names)))), by="df") %>% 
  mutate(Class = c,
         Year = 2013,
         Mark = case_when( # Fixing manually bc error for these five cases
           Event == "100m" & Gender == "Boys" & Class == "2A" ~ "11.00",
           Event == "100m" & Gender == "Boys" & Class == "3A" ~ "11.02",
           Event == "100m" & Gender == "Boys" & Class == "4A" ~ "11.05",
           Event == "100m" & Gender == "Boys" & Class == "5A" ~ "10.95",
           Event == "100m" & Gender == "Boys" & Class == "6A" ~ "11.03",
           TRUE ~ Mark
         )) %>% 
  select(State, Year, Gender, Event, Class, Mark)

##########
# 2014
# https://www.ghsa.net/sites/default/files/documents/track/2014_Track_Boys_State_Meet_-_2A_Results.pdf
names <- list()
for(i in c("1A_Public",
           "1A_Private",
           "2A",
           "3A",
           "4A",
           "5A",
           "6A")){
  names[length(names)+1]<-paste("https://www.ghsa.net/sites/default/files/documents/track/2014_Track_Boys_State_Meet_-_",i,"_Results.pdf", sep="")
}
n<-names[1]
files <- list()
for(i in 1:length(names)){
  files[length(files)+1]<-list(clean_hytek(names[[i]][1]))
} 
ga2014 <- files %>% bind_rows(.id="df") %>% 
  inner_join(data.frame(c=c("1A_Public",
                            "1A_Private",
                            "2A",
                            "3A",
                            "4A",
                            "5A",
                            "6A"), df=as.character(c(1:length(names)))), by="df") %>% 
  mutate(Class = c,
         Year = 2014) %>% 
  select(State, Year, Gender, Event, Class, Mark)

##########
# 2015
# https://www.ghsa.net/sites/default/files/documents/track/2015_ghsa_boys_state_meet_class_6a_performance_lists.pdf
url <- "https://www.ghsa.net/sites/default/files/documents/track/2015_ghsa_boys_state_meet_class_5a_performance_lists.pdf"

clean_hytek2015 <- function(url){
  pdf <- extract_text(url)
  event_str <- "([0-9]?(\\.X\\.)?[0-9]+0[\\. ]?(meter|yard|mile|m)[\\. ](run|dash|relay|hurdles)|discus|shot.put|(high|long|triple).jump|pole.vault)"
  mark_str <- "[A-Za-z] [0-9]+(' )?[0-9\\.\'\":\\-]+ *[%@!#]*( +[\\-\\+]?[0-9\\.NWI]+)? *([0-9]+)?[A-Za-z]+"
  ga2012 <- pdf %>% lapply(., function(d){
    data.frame(text = d) %>% 
      separate_longer_delim(text, delim="\r\n")
  }) %>% 
    bind_rows(.id = "df") %>% 
    mutate(df = as.numeric(df)) %>% 
    # unite(col="text") %>% 
    # separate_wider_delim(text, delim="_", names=c("df", "text"), too_many="merge") %>% 
    mutate(text = gsub("_NA", "", text),
           Event = str_extract(str_to_lower(text), event_str),
           Event = str_to_title(gsub("\\."," ", Event)),
           Event = gsub(" Meter (Dash|Run)", "m", Event),
           Class = str_extract(text, "(Class [A]+|Wheelchair Div(ision)? [123])"),
           Mark.raw = str_extract(text, mark_str),
           Mark.raw = trimws(Mark.raw),
           Mark = str_extract(Mark.raw, "[0-9]+(' )?[0-9\\.\'\":\\-]+ *[%@!#]*( +[\\-\\+]?[0-9\\.NWI]+)? *([0-9]+)?"),
           Race.Type = ifelse(str_detect(text, "^ *Event"),
                              ifelse(str_detect(text, "Prelims"),
                                     "Preliminaries",
                                     "Finals"),
                              NA),
           Rank.Raw = trimws(str_extract(text, " *[0-9]+ *$")),
           Rank = substr(Rank.Raw, 2, length(Rank.Raw)),
           Gender = "Boys",
           State = "Georgia",
           Year = 2012
    ) %>% 
    fill(Event, Class, Race.Type) %>% 
  filter(Event %in% c("100m", "800m", "1600m", "High Jump", "Pole Vault"),
         Rank == "1") %>% 
    select(State, Year, Gender, Event, Class, Mark)
  return(ga2012)
}

names <- list()
for(i in c("1a-public",
           "1a-private",
           "2a",
           "3a",
           "4a",
           "5a",
           "6a")){
  names[length(names)+1]<-paste("https://www.ghsa.net/sites/default/files/documents/track/2015_ghsa_boys_state_meet_class_",i,"_performance_lists.pdf", sep="")
}
n<-names[1]
files <- list()
for(i in 1:length(names)){
  files[length(files)+1]<-list(clean_hytek2015(names[[i]][1]))
} 
ga2015 <- files %>% bind_rows(.id="df") %>% 
  inner_join(data.frame(c=c("1A_Public",
                            "1A_Private",
                            "2A",
                            "3A",
                            "4A",
                            "5A",
                            "6A"), df=as.character(c(1:length(names)))), by="df") %>% 
  mutate(Class = c,
         Year = 2015) %>% 
  select(State, Year, Gender, Event, Class, Mark)

##########
# 2016
# https://www.ghsa.net/sites/default/files/documents/track/2014_Track_Boys_State_Meet_-_2A_Results.pdf
url<-names
clean_hytek_html <- function(url){
  html <- read_html(url) %>% html_text
  event_str <- "([0-9]?(\\.X\\.)?[0-9]+0[\\. ]?(meter|yard|mile|m)[\\. ](run|dash|relay|hurdles)|discus|shot.put|(high|long|triple).jump|pole.vault)"
  mark_str <- "[0-9]+(' )?[0-9\\.\'\":\\-]+ *[%@!#&]*( +[\\-\\+]?[0-9\\.NWI]+)? *([0-9]+)? *$"
  ga2012 <- html %>% lapply(., function(d){
    data.frame(text = d) %>% 
      separate_longer_delim(text, delim="\n")
  }) %>% 
    bind_rows(.id = "df") %>% 
    mutate(df = as.numeric(df)) %>% 
    # unite(col="text") %>% 
    # separate_wider_delim(text, delim="_", names=c("df", "text"), too_many="merge") %>% 
    mutate(text = gsub("_NA", "", text),
           Event = str_extract(str_to_lower(text), event_str),
           Event = str_to_title(gsub("\\."," ", Event)),
           Event = gsub(" Meter (Dash|Run)", "m", Event),
           Class = str_extract(text, "(Class [A]+|Wheelchair Div(ision)? [123])"),
           Mark.raw = str_extract(text, mark_str),
           Mark.raw = trimws(Mark.raw),
           Mark = str_extract(Mark.raw, "^[0-9]+(' )?[0-9\\.\'\":\\-]+"),
           Race.Type = str_extract(text, "(Preliminaries|Finals|Heat [1-9])"),
           Rank.Raw = trimws(str_extract(text, "^ *[0-9]* ")),
           Rank = str_extract(Rank.Raw, "1?[0-9]$"),
           Gender = "Boys",
           State = "Georgia",
           Year = 2016
    ) %>% 
    fill(Event, Class, Race.Type) %>% 
    filter(Event %in% c("100m", "800m", "1600m", "High Jump", "Pole Vault"),
           Rank == "1",
           Race.Type == "Finals") %>% 
    select(State, Year, Gender, Event, Class, Mark)
  return(ga2012)
}

names <- "https://www.ptgrouponline.com/Results/2016/HS/GHSA_Boys_Results.htm"

ga2016 <- clean_hytek_html(names)

####
# 2017,18

urls.all <- read_excel("..\\Wyoming\\Book1.xlsm", sheet="Georgia")
urls <- urls.all %>% 
  fill(Year) %>% 
  filter(str_detect(Name,"Boys ([186]+00 Meter (Dash|Run)|Pole Vault|High Jump)( Class)? [1234567]A(-(Public|Private))?( Finals)? *$")) %>% 
  distinct()

texts<-list()
for(i in 1:nrow(urls)){
  texts[length(texts)+1]<-read_html(urls[i,3][[1]])%>% 
    html_text()
}
rawhtml<-lapply(texts, function(d){
  tibble(v1=d) %>% 
    separate_longer_delim(v1, delim = "\n") %>% 
    data.frame(text = .)
}) %>% 
  bind_rows(.id="df") %>% 
  mutate(df = as.character(ceiling(as.numeric(df)/40)+2016)) %>% 
  select(df, everything())


event_str <- "([0-9]?(\\.X\\.)?[0-9]+0[\\. ]?(meter|yard|mile|m)[\\. ](run|dash|relay|hurdles)|discus|shot.put|(high|long|triple).jump|pole.vault)"
mark_str <- "[0-9]+(' )?[0-9\\.\'\":\\-]+ *[%@!#&]*( +[\\-\\+]?[0-9\\.NWI]+)? *([0-9]+)? *$"
ga201718<- rawhtml %>% 
    rename(text = v1) %>% 
    mutate(Year = as.numeric(df)) %>% 
    #mutate(Year = str_extract(text, "201[78]")) %>% 
    # unite(col="text") %>% 
    # separate_wider_delim(text, delim="_", names=c("df", "text"), too_many="merge") %>% 
    mutate(text = gsub("_NA", "", text),
           Event = str_extract(str_to_lower(text), event_str),
           Event = str_to_title(gsub("\\."," ", Event)),
           Event = gsub(" Meter (Dash|Run)", "m", Event),
           Class = str_extract(text, "([1234567]A(-(Public|Private))?|Wheelchair Div(ision)? [123])"),
           Mark.raw = str_extract(text, mark_str),
           Mark.raw = trimws(Mark.raw),
           Mark = str_extract(Mark.raw, "^[0-9]+(' )?[0-9\\.\'\":\\-]+"),
           Race.Type = str_extract(text, "(Preliminaries|Finals|Heat [1-9])"),
           Rank.Raw = trimws(str_extract(text, "^ *[0-9]* ")),
           Rank = str_extract(Rank.Raw, "1?[0-9]$"),
           Gender = "Boys",
           State = "Georgia"
    ) %>% 
    fill(Event, Class, Race.Type, Year) %>% 
    filter(Event %in% c("100m", "800m", "1600m", "High Jump", "Pole Vault"),
           Rank == "1",
           Race.Type == "Finals") %>% 
    select(State, Year, Gender, Event, Class, text,Rank, Mark)
ga201718 %>% count(Year, Event)

######
# 2019
names <- c("https://ga.milesplit.com/meets/346234/results/653087/raw#.XwSKhihKhhE",
           "https://ga.milesplit.com/meets/346233/results/653473/raw#.XwSJ-ihKhhE",
           "https://ga.milesplit.com/meets/346235/results/653115/raw#.XwSK8ihKhhE")
clean_hytek_html(names[[1]])
url<-names[[3]]
clean_hytek_html_2019 <- function(url){
  html <- read_html(url) %>% html_text()
  event_str <- "([0-9]?(\\.X\\.)?[0-9]+0[\\. ]?(meter|yard|mile|m)[\\. ](run|dash|relay|hurdles)|discus|shot.put|(high|long|triple).jump|pole.vault)"
  mark_str <- "[0-9]+(' )?[0-9\\.\'\":\\-]+ *[%@!#&\\*]*( +[\\-\\+]?[0-9\\.NWI]+)? *([0-9]+)? *$"
  ga <- html %>% lapply(., function(d){
    data.frame(text = d) %>% 
      separate_longer_delim(text, delim="\n")
  }) %>% 
    bind_rows(.id = "df") %>% 
    mutate(df = as.numeric(df)) %>% 
    # unite(col="text") %>% 
    # separate_wider_delim(text, delim="_", names=c("df", "text"), too_many="merge") %>% 
    mutate(text = gsub("_NA", "", text),
           Event = str_extract(str_to_lower(text), event_str),
           Event = str_to_title(gsub("\\."," ", Event)),
           Event = gsub(" Meter (Dash|Run)", "m", Event),
           Class = str_extract(text, "([1234567]A(-(Public|Private))?|Wheelchair WH Class [123])"),
           Gender = str_extract(text, "(Girls|Boys)"),
           Mark.raw = str_extract(text, mark_str),
           Mark.raw = trimws(Mark.raw),
           Mark = str_extract(Mark.raw, "^[0-9]+(' )?[0-9\\.\'\":\\-]+"),
           Race.Type = str_extract(text, "(Preliminaries|Finals|Heat [1-9])"),
           Rank.Raw = trimws(str_extract(text, "^ *[0-9]* ")),
           Rank = str_extract(Rank.Raw, "1?[0-9]$"),
           State = "Georgia",
           Year = 2019
    ) %>% 
    fill(Event, Class, Race.Type,Gender) %>% 
    filter(Event %in% c("100m", "800m", "1600m", "High Jump", "Pole Vault"),
           Rank == "1",
           Race.Type == "Finals",
           Gender =="Boys") %>% 
    select(State, Year, Gender, Event, Class, Mark)
  return(ga)
}

ga2019<- lapply(names, clean_hytek_html_2019) %>% 
  bind_rows()

######
# Combine all, Save

combined_data = bind_rows(list(cleandata,
                               ga2011,
                               ga2012,
                               ga2013,
                               ga2014,
                               ga2015,
                               ga2016,
                               ga201718,
                               ga2019))

write.csv(combined_data, "georgia_clean.csv", row.names = F)

combined_data %>%  
  count(Gender, Year, Event) %>%
  filter(Year >= 1978) #%>% 
  print(n=500)
