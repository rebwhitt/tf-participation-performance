library(tidyverse)
library(wkb)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\")
#"PerformanceData.csv",

convert_time <- Vectorize(function(time){
  time <- gsub(" ","",time)
  t<-str_split(time, ":")
  min <- as.numeric(t[[1]][1])
  sec <- t[[1]][2]

  sec=gsub("1[/⁄?]5", ".2", sec)
  sec=gsub("2[/⁄?]5", ".4", sec)
  sec=gsub("3[/⁄?]5", ".6", sec)
  sec=gsub("4[/⁄?]5", ".8", sec)

  sec = as.numeric(sec)

  if(!is.na(min)){
    return((min*60)+sec)
  } else {
    return(sec)
  }
})
convert_time(":104?5")
convert_time(" 5:43.37")
convert_dist <- Vectorize(function(dist){
  dist <- gsub(" ","",dist)
  if(str_detect(dist, "[mM]$")){
    dist = gsub("[mM]","", dist)
    as.numeric(dist)
  } else {
    dist = gsub("\\(tie\\)", "", dist)
    dist = gsub(" ", "", dist)
    d<- str_split(dist, "[\'’\"”\\-\\.:]+")[[1]]
    d[2]<- ifelse(d[2]=="",
                  "0",
                  d[2])
    d[2] = gsub("1[/⁄?]8", ".125", d[2])
   # d[2] =gsub("1[/⁄?]5", ".2", d[2])
    d[2] = gsub("1[/⁄?]4", ".25", d[2])
    d[2] = gsub("1[/⁄?]2", ".5", d[2])
    d[2] =gsub("3[/⁄?]4", ".75", d[2])
  #  d[2] =gsub("2[/⁄?]5", ".4", d[2])
   # d[2] =gsub("3[/⁄?]5", ".6", d[2])
    d[2] =gsub("3[/⁄?]8", ".375", d[2])
    d[2] =gsub("5[/⁄?]8", ".625", d[2])
  #  d[2] =gsub("4[/⁄?]5", ".8", d[2])
    d[2] =gsub("7[/⁄?]8", ".875", d[2])
    d[2] = gsub("¼", ".25", d[2])
    d[2] = gsub("½", ".5", d[2])
    d[2] = gsub("¾", ".75", d[2])
    inches <- (as.numeric(d[1])*12) + as.numeric(d[2])
    inches * 0.0254
    #999
  }
})
convert_dist("5-0")
# Fixes when Excel auto-converts distances (e.g. 6-7) to dates (7-Jun)
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
      inches == "Jun" ~ 6,
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
      feet == "Jun" ~ 6,
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

dist = gsub(" \\(tie\\)", "", "5-3 (tie)")
convert_dist("\t 9-6")
#m <- performance[8390,8]
#convert_dist(gsub(g, "", m))
#g<- str_extract(m, "[^0-9\\-]+")
#str_extract(performance[8390,8], "[^0-9\\-]+")
performance <- lapply(c(
                        "California\\california_clean.csv",
                        "Florida\\florida_clean.csv",
                        "Georgia\\georgia_clean.csv",
                        "Illinois\\illinois_clean.csv",
                        "Kansas\\kansas_clean.csv",
                        "Maryland\\maryland_clean.csv",
                        "Massachusetts\\massachusetts_clean.csv",
                        "Michigan\\michigan_clean.csv",
                        "Nebraska\\nebraska_clean.csv",
                        "New Mexico\\new_mexico_clean.csv",
                        "New York\\new_york_clean.csv",
                        "North\ Carolina\\north_carolina_clean.csv",
                        "Ohio\\ohio_clean.csv",
                        "Oklahoma\\oklahoma_clean.csv",
                        "Oregon\\oregon_clean.csv",
                        "Rhode\ Island\\rhode_island_clean.csv",
                        "Tennessee\\tennessee_clean.csv",
                        "Washington\\washington_clean.csv",
                        "Wisconsin\\wisconsin_clean.csv",
                        "Wyoming\\wyoming_clean.csv"),
                      read.csv, colClasses=c("Mark.Note"="character")) |> 
  bind_rows()|> 
  filter(is.na(Stage) | Stage != "Prelim") %>% 
  select(State, Year, Event, Gender, Class, Mark, Mark.Note) |> 
  filter(Year <= 2019) |> 
  filter((is.na(Class) | Class != "W"), Mark != "") |> 
  drop_na(Event) %>% 
  mutate(Mark.raw = Mark,
         Mark.raw = gsub("\\.\\.", "\\.", Mark.raw),
         Mark.raw = gsub("[\n ]", "", Mark.raw),
        # Mark.raw = gsub(" ", "", Mark.raw),
         Mark.raw = gsub("--", "-", Mark.raw),
         Mark.raw = gsub(",", "\\.", Mark.raw),
         Mark.raw = ifelse(str_detect(Mark.raw, "[JFMASOND][aepuco][nbrylgptvc]"),
                           convert_dates(Mark.raw),
                           Mark.raw),
         Mark.raw = ifelse(Event %in% c("High Jump", "Pole Vault") & str_count(Mark.raw, "\"")>1,
                           str_replace(Mark.raw, "\"","\'"),
                           Mark.raw),
         Mark.raw = ifelse(Event %in% c("High Jump", "Pole Vault"),
                           str_replace(Mark.raw, "\'\'","\'"),
                           Mark.raw),
         Mark.raw = ifelse(Event %in% c("800m","1600m") & str_count(Mark.raw, "\\.")>1,
                           str_replace(Mark.raw, "\\.",":"),
                           Mark.raw),
         Mark.raw = gsub("\xc2\xa0", "", Mark.raw),
         Mark.Note = str_extract(Mark.raw, "[\\*WwR]+$"),
         Mark.raw = gsub("[\\*WR]", "", Mark.raw),
         Mark = case_when(
           Event == "100m" ~ as.numeric(gsub(":","",Mark.raw)),
           Event == "100y" ~ as.numeric(gsub(":","",Mark.raw)) * 1.094, #check this
           Event == "800m" ~ convert_time(gsub(";",":", Mark.raw)),
           Event == "880y" ~ convert_time(gsub(";",":", Mark.raw)) * 0.9942,
           Event == "1600m" ~ convert_time(gsub(";",":", Mark.raw)),
           Event == "1500m" ~ convert_time(gsub(";",":", Mark.raw)) * 1.0737,
           Event == "Mile" ~ convert_time(gsub(";",":", Mark.raw)) * 0.9942,
           Event == "High Jump" ~ convert_dist(Mark.raw),
           Event == "Pole Vault" ~ convert_dist(Mark.raw),
           TRUE ~ NA
         ),
         Event.raw = Event,
         Event = case_when(
           Event.raw == "100y" ~ "100m",
           Event.raw == "880y" ~ "800m",
           Event.raw == "1500m" ~ "1600m",
           Event.raw == "Mile" ~ "1600m",
           TRUE ~ Event.raw
         ))  %>% 
  filter(Event %in% c("100m","800m","1600m","High Jump","Pole Vault")) %>% 
  distinct()

 #|> 
  #pivot_wider( names_from=c(Gender), values_from=Mark)

write.csv(performance, "performance_clean.csv", row.names = F)
#write.csv(performance_one, "performance_one_clean.csv", row.names = F)

#performance |> 
#  count(State)
#performance |> 
#  filter(State=="Massachusetts",
#         Year < 1982,
#         Event == "100y / 100m") |> 
#  summarize(mean = mean(Mark))

#gsub("\\.\\.", "\\.", "2:10..47")
#convert_dist(" 8-6")

#charToRaw("\xc2\xa0")
#rawToChar(c(hex2raw("a0"), hex2raw("c2")))

