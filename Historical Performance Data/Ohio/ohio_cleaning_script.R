library(tidyverse)
library(rvest)
library(pdftools)
library(tabulapdf)
library(tesseract)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Ohio")

# url format examples
url <- "https://www.ohsaa.org/Portals/0/Sports/Track-Field/pastresults/boys/TF_1997.pdf" # 1978-1997 boys
url <- "https://www.ohsaa.org/Portals/0/Sports/Track-Field/pastresults/boys/2000-b.pdf" #1998-2000boys, 1978-2000girls
url <- "https://www.ohsaa.org/sports/tf/2010/bd1reslt10.htm" # 2001 - 2008, 2010-2011
url <- "https://www.baumspage.com/ohsaa/tf/2013/b1.htm" # 2009, 2012 - 2013
url <- "https://www.baumspage.com/ohsaa/tf/2019/Results1.htm" # 2014 - 2017, 2019
url <- "https://www.ohsaa.org/Portals/0/Sports/Track-Field/2018/Div1Results.pdf" # 2018

urls <- list()
for(i in c(2001:2008, 2010:2011)){
  for(j in 1:3){
    for(k in c("b","g")){
      urls[length(urls)+1]<-paste("https://www.ohsaa.org/sports/tf/",i,"/",k,"d",j,"reslt",str_sub(i,3,4),".htm", sep="")
    }
  }
}
for(i in c(2009,2012:2013)){
  for(j in 1:3){
    for(k in c("b","g")){
      urls[length(urls)+1]<-paste("https://www.baumspage.com/ohsaa/tf/",i,"/",k,j,".htm", sep="")
    }
  }
}
for(i in c(2014:2017,2019)){
  for(j in 1:3){
    urls[length(urls)+1]<-paste("https://www.baumspage.com/ohsaa/tf/",i,"/Results",j,".htm", sep="")
  }
}

pdfs2018 <- list()
for(i in 1:3){
  pdfs2018[length(pdfs2018)+1]<-paste("https://www.ohsaa.org/Portals/0/Sports/Track-Field/2018/Div",i,"Results.pdf", sep="")
}

files <- lapply(pdfs2018, pdf_text) #%>% 
  bind_rows()
pdf_text(pdfs2018[[1]])

read_html(url) %>% 
  html_text()

texts <-list()
for(i in 1:length(urls)){
  texts[length(texts)+1]<-read_html(urls[[i]])%>% 
    html_text()
}

texts <- append(texts, files)
#texts <- files
rawhtml<-lapply(texts, function(d){
  tibble(text=d) %>% 
    separate_longer_delim(text, delim = "\n") %>% 
    data.frame(text = .)
}) %>% 
  bind_rows(.id="df") %>% 
  #mutate(df = as.character(ceiling(as.numeric(df)/40)+24)) %>% 
  select(df, everything())

event_str <- "([0-9]?(x)?[0-9]*0?[\\. ]?(meter|yard|mile|m)[\\. ](run|dash|relay|(low|high)? ?hurdles)|discus|shot.put|(high|long|triple).jump|pole.vault)"
mark_str <- "[0-9]+(' )?[0-9\\.\'\":\\-]+ *[%@!#]*( +[\\-\\+]?[0-9\\.NWI]+)? *([0-9]+)? *$"
cleandata <- rawhtml %>% 
  mutate(Event = str_to_title(str_extract(str_to_lower(text), event_str)),
         Year = trimws(str_extract(text, "^[ \t\n]*[12][90][0-9][0-9]")),
         Gender = str_extract(text, "(Girls|Boys)"),
         Class = str_extract(text, "Div ?ision [I123]+"),
         Race.Type = str_extract(text, "(Preliminaries|Finals|Heat [1-9])"),
         Rank = trimws(str_extract(text, "^ *1?[0-9] ")),
         Mark.raw = str_extract(text, mark_str),
         Mark.raw = trimws(Mark.raw),
         Mark = str_extract(Mark.raw, "^[0-9]+(' )?[0-9\\.\'\":\\-]+"),
         Event = gsub(" Meter (Dash|Run)", "m", Event)) %>% 
  fill(Year, Event, Gender, Class, Race.Type) %>% 
  filter(Rank == "1",
         Race.Type == "Finals",
         Event %in% c("100y", "100m", "880y", "800m",
                      "Mile", "1600m", "High Jump",
                      "Pole Vault")) %>% 
  mutate(State = "Ohio") %>% 
  select(State, Year, Gender, Event, Class, Mark)

## pre-2001 pdfs
pdfs <- list()
for(i in 1998:2000){
  pdfs[length(pdfs)+1]<-paste("https://www.ohsaa.org/Portals/0/Sports/Track-Field/pastresults/boys/",i,"-b.pdf", sep="")
}
for(i in 1978:2000){
  pdfs[length(pdfs)+1]<-paste("https://www.ohsaa.org/Portals/0/Sports/Track-Field/pastresults/girls/",i,"-g.pdf", sep="")
}

pdf_files <- lapply(pdfs, pdf_ocr_text)
pdf_files <- lapply(pdfs, extract_tables) 

rawdata.98 <- lapply(pdf_files, function(file){
  lapply(file, function(d){
    data.frame(d) %>%
      mutate_all(as.character) %>% 
      rename(any_of(c(v1 = 1, v2= 2, v3 = 3, v4 = 4, v5 = 5,
                      v6 = 6, v7 = 7, v8 = 8, v9 = 9, v10 = 10,
                      v11 = 11, v12 = 12, v13 = 13, v14 = 14, v15 = 15)))
    
  }) %>% 
    bind_rows(.id="tbl")
})%>% 
  bind_rows(.id="df")

write.csv(cleandata, "ohio_clean.csv", row.names = F)

cleandata %>% 
  count(Year,Event,Gender)

m<- "      1 Kendall Stevens           12 Lima Senior               10.62    2.1  1 "
m<- "      1 Kendall Stevens           12 Lima Senior               10.79   -4.4  10"
m<-"1   710 Chad Zallow         10 Warren JFK               14.35  -0.1  10 "


str_extract(m, mark_str)





