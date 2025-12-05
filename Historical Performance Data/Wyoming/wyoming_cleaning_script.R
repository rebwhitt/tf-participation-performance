library(tidyverse)
library(rvest)
library(tabulapdf)
library(janitor)
library(pdftools)
library(readxl)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Wyoming")

names <- list()
for(i in 1978:2001){
  names[length(names)+1]<- paste("https://www.whsaa.org/archives/track/TR-",i,".pdf",sep="")
}

names2 <- list()
for(i in 2005:2019){
  names2[length(names2)+1]<- paste("https://www.whsaa.org/archives/track/TR-",i,".pdf",sep="")
}

pages <- c(89,1,1,90,90,101,100,97,98,112,104,105,101,102,101)
# 05: 89-163
# 06: 1-78 (all)
# 07: all
# 08: 90-273 (end)
# 09: 90-169 end
# 10: 101-180 end
# 11: 100-180 end
# 12: 97-179 end
# 13: 98-179 end
# 14: 112-194 end
# 15: 104-186 end
# 16: 105-187 end
# 17: 101- 183 end
# 18: 102-190 end
# 19: 101-188


#files <- lapply(names, function(url){
#  print(url)
#  extract_tables(url, method="stream") 
#})
load("rawdata78.rda")

dfs <- list()
for(i in 1:1){
  print(names2[[i]][1])
  #dfs[length(dfs)+1]<-list(extract_tables(names2[[i]][1], pages=pages[[i]][1]:get_n_pages(names2[[i]][1])))
}
dfs <- list()
for(i in 1:15){
  download.file(names2[[i]][1], basename(names2[[i]][1]), mode="wb")
  txt <- pdf_text(basename(names2[[i]][1]))
  dfs[length(dfs)+1]<- paste(txt[pages[[i]][1]:length(txt)], collapse="\n") %>% 
    tibble(all=.) %>% 
    separate_longer_delim(all, delim = "\n") %>% 
    data.frame(text = .)
}
rawdata.05<-lapply(dfs, function(d){
  data.frame(d) %>% 
    rename(v1=d)
}) %>% 
  bind_rows(.id="df") %>% 
  mutate(df = as.character(as.numeric(df)+27))

#d[[1]]
#txt[89]
#files <- append(files, dfs)
#files <- files[1:24]


## Load from html sites (2002-2004)

urls.all <- read_excel("Book1.xlsm")
urls <- urls.all %>% 
  filter(str_detect(Name,"(Girls|Boys) ([186]+00 Meter (Dash|Run)|Pole Vault|High Jump) [1234]A( Finals)? *$"))

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
  mutate(df = as.character(ceiling(as.numeric(df)/40)+24)) %>% 
  select(df, everything())


read_html(urls[i,3][[1]])
length(urls)


#rawdata.78 <- lapply(files, function(file){
#  lapply(file, function(d){
#    data.frame(d) %>%
#      mutate_all(as.character) %>% 
#      rbind(colnames(.), .) %>% 
#      rename(any_of(c(v1 = 1, v2= 2, v3 = 3, v4 = 4, v5 = 5,
#                      v6 = 6, v7 = 7, v8 = 8, v9 = 9, v10 = 10,
#                      v11 = 11, v12 = 12, v13 = 13, v14 = 14, v15 = 15)))
#    }) %>% 
#    bind_rows(.id="tbl")
#  })%>% 
#  bind_rows(.id="df") %>% 
#  filter(as.numeric(df) <= 24)

  rawdata <- bind_rows(list(rawdata.78, rawhtml, rawdata.05))

rawdata %>% count(df)  
save(rawdata.78, file="rawdata78.rda")

event_str <- "([0-9]?(\\.X\\.)?[0-9]*0?[\\. ]?(meter|yard|mile|m)[\\. ](run|dash|relay|hurdles)|discus|shot.put|(high|long|triple).jump|pole.vault)"

cleandata <- rawdata %>% 
  unite(col="text") %>% 
  mutate(text = str_replace_all(text, "_NA", ""),
         Event = str_extract(str_to_lower(text), event_str),
        title = str_extract(text, "[12][09][0-9][0-9](\\.Class)?\\.[1-6]?[ABCD]+\\.(Girls|Boys)")) %>% 
  mutate(title = case_when(
    text == "2_40_...1_X100.Meter.Dash_...3" ~ "1979.Class.B.Boys",
    text == "7_101_...1_X100.Meter.Dash_...3" ~ "1984.Class.3A.Girls",
    text == "11_121_PLACEME_SCHOOL_MARK" ~ "1988.Class.2A.Girls",
    text == "13_41_...1_X100.Meter.Dash_...3_...4" ~ "1990.Class.2A.Boys",
    text == "13_101_...1_X100.Meter.Dash_...3_...4" ~ "1990.Class.3A.Girls",
    text == "17_120_PLACEME_GRADE_SCHOOL_MARK" ~ "1994.Class.2A.Girls",
    text == "18_60_...1_X100.Meter.Dash_...3_...4" ~ "1995.Class.1A.Boys",
    text == "20_119_...1_X100.Meter.Dash_...3_...4" ~ "1997.Class.2A.Girls",
    text == "22_127_PLACEME_GRADE_SCHOOL_MARK" ~ "1999.Class.2A.Girls",
    TRUE ~ title
  )) %>% 
  mutate(
    text = gsub("X", "", text),
    Event = case_when(
    text == "11_121_PLACEME_SCHOOL_MARK" ~ "100.meter.dash",
    text == "17_120_PLACEME_GRADE_SCHOOL_MARK" ~ "100.meter.dash",
    text == "22_127_PLACEME_GRADE_SCHOOL_MARK" ~ "100.meter.dash",
    text == "1_20_Place_Name_School_Mark" ~ "200.meter.dash",
    text == "1_30_Place_Name_School_Mark" ~ "high.jump",
    text == "1_130_PLACEME_SCHOOL_MARK" ~ "880.yard.run",
    text == "3_30_1_Brian Trebelcock_Buffalo_20\' 3.5\"" ~ "long.jump",
    text == "5_130_1ST_Gosar_Pinedale_12:22:00" ~ "3200.meter.run",
    text == "6_88_PLACEME_SCHOOL_MARK" ~ "800.meter.run",
    text == "7_121_1ST_Hium, S._Tongue River_27.12" ~ "200.meter.dash",
    text == "8_79_1ST_Wilson, K._Green River_26.18" ~ "200.meter.dash",
    text == "10_116_1ST_Hill, Kristi_Worland_16\'04.75\"" ~ "long.jump",
    text == "12_144_1ST_Rauth, Diana_Hulett_12:31:00" ~ "3200.meter.run",
    text == "13_66_1ST_Hiler, Nick_12_Ten Sleep_04:41:00" ~ "1600.meter.run",
    text == "14_66_1ST_Davison, David_12_Lingle-Ft. Laramie_4:47.01" ~ "1600.meter.run",
    text == "14_120_1ST_Dillon, Marsha_11_Upton_27.29" ~ "200.meter.run",
    text == "16_12_1ST_Sheridan_7:59.99" ~ "4.x.800.meter.relay",
    text == "17_144_1ST_Collins, Jessica_10_Kaycee 13:10.66" ~ "3200.meter.run",
    text == "18_46_1ST_Oliver, Brian_11_Upton_10:11:00" ~ "3200.meter.run",
    text == "23_29_1st_Arte Huhn_11_Jackson_09:56:00" ~ "3200.meter.run",
    text == "22_100_1_Ashley Despain_12_Kelly Walsh_16\'08.50\"" ~ "long.jump",
    text == "22_87_1_Erin Smith_12_Green River_26.07" ~ "200.meter.dash",
    text == "25_75_1.Burke..PaulSo.Campbell.County_4.28.00_4.35.59_10" ~ "1600.meter.run",
    #text == "25_1_3.Roberson..Kelsey12.Meeteetse_13.34q..2_...3" ~ "100.meter.dash",
    text == "19_46_PLACEME_GRADE_SCHOOL_MARK" ~ "800.meter.run",
    text == "19_117_1ST_Walker, Becky_12_Rawlins_9\'00\"" ~ "pole.vault",
    TRUE ~ Event
  ))  %>% 
  separate_wider_regex(title, c(Year = "[0-9]+",
                                "\\.(?:Class\\.)?",
                                Class = "[ABC1234]+",
                                "\\.",
                                Gender = "(?:Boys|Girls)")) %>%
  mutate(
    Year = as.numeric(Year),
    Year = case_when(
      text == "25_1_X1.Roberson..Kelsey12.Meeteetse_X13.68_X13.67Q..3"
        ~ 2005,
      TRUE ~ Year
      ),
    Class = trimws(ifelse(is.na(Class),
                   str_extract(text, "\\b[1234]?A\\b"),
                   Class)),
    Gender = ifelse(is.na(Gender),
                    str_extract(text, "(Girls|Boys)\\b"),
                    Gender),
    Race.Type = str_extract(text, "(Preliminaries|Finals|Heat [1-9])"),
    Rank = trimws(str_extract(str_extract(text, "^[0-9]+_ ?([0-9]+_)? *[0-9]+[_ SsNnRrTt][TtDdHh]?"),"[0-9]+.?.?$")),
    Rank = trimws(gsub("[_A-Za-z]+","",Rank)),
    Year = as.numeric(str_extract(text, "^[0-9]+"))+1977,
   # Year = ifelse(Year > 2001,
    #              as.numeric(str_extract(text, "^[0-9]+"))+2004,
     #             Year),
    text = gsub(">",".", text),
    Mark.raw = str_extract(text, "[0-9]+(' )?[0-9\\.\'\":\\-,]+([_ #!@]+[0-9\\. ]+)?[#!@ ]*$"),
    Mark.raw = gsub("\\.\\.\\.\\.","_", Mark.raw), 
   Mark = str_extract(Mark.raw, "^[0-9]+(' )?[0-9\\.\'\":\\-,]+"),
    Event = case_when(
      str_detect(Event, "^(1.)?mile.run") ~ "Mile",
      str_detect(Event, "100.m(eter)?.dash") ~ "100m",
      str_detect(Event, "100.yard.dash") ~ "100y",
      str_detect(Event, "1600.m(eter)?.run") ~ "1600m",
      str_detect(Event, "800.m(eter)?.(run|dash)") ~ "800m",
      str_detect(Event, "880.yard.run") ~ "880y",
      str_detect(Event, "high.jump") ~ "High Jump",
      str_detect(Event, "pole.vault") ~ "Pole Vault",
      TRUE ~ Event
    ),
  Mark = ifelse(text == "21_107_1_Katie Hostetler 12 Buffalo_4:11.70",
                "5:11.70",
                Mark)
    ) %>%
  fill(Year, Class, Gender, Event, Race.Type) %>% 
  select(Year, Gender, Event, Class, text, Rank, Mark, Mark.raw, Race.Type) %>% 
  filter(Rank=="1",
         (is.na(Race.Type) | Race.Type != "Preliminaries"),
         Event %in% c("100y", "100m", "880y", "800m",
                      "Mile", "1600m", "High Jump",
                      "Pole Vault")) %>% 
  mutate(Mark = ifelse(Mark== "6\'06000\"",
                       "6\'06.00\"",
                       Mark)) %>% 
  drop_na(Mark) %>% 
  mutate(State = "Wyoming") %>% 
  select(State, Year, Gender, Event, text, Class, Mark)



write.csv(cleandata, "wyoming_clean.csv", row.names = F)

cleandata %>% 
  count(Year, Gender, Event) %>% 
  filter(n != 4, Year > 1979) %>% 
  print(n=500) 
  
cleandata %>% 
  count(Event) %>% 
  print(n=100)
  
str_extract("39_171_Event 136  Boys Shot Put 4A",
            "\\b[1234]A\\b")  

str_to_lower("1.MILE.RUN")


#files[[4]][1]
# 1979 Boys B 1265
# 1984 Girls 3A
# 1988 2A Girls
# 1990 2A Boys
# 1990 3A Girls
# 1994 2A Girls
# 1995 1A Boys
# 1997 2A Girls
# 1999 2A Girls

str_extract("25_1_    1) Campbell County High Sch    36.50     2) Evanston ",
            "^[0-9]+_ ?([0-9]+_)? *[0-9]+ ")
str_extract("7_77_2ND_McGarvin, J. Ten Sleep_11'06\"",
            "^[0-9]+_ ?([0-9]+_)? *[0-9]+[_ SNRT][TDH]?")



# 
