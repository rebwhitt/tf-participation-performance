library(tidyverse)
library(pdftools)
library(tabulapdf)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Nebraska")

links <- list()
for(i in 90:96){
  for(g in c("g", "b")){ #
    for(j in c("a", "b", "c", "d")){
      links[[length(links)+1]] <- paste("https://nsaahome.org/textfile/track/",j,g,"res",i,".pdf", sep="")
    }
  }
}

for(i in 7:8){
  for(g in c("g", "b")){ #
    for(j in c("a", "b", "c", "d")){
      links[[length(links)+1]] <- paste("https://nsaahome.org/textfile/track/",j,g,"res",i,"0s.pdf", sep="")
    }
  }
}

#links <- list("https://nsaahome.org/textfile/track/cgres96.pdf")

rawdata <- lapply(links, function(url){
  file <- basename(url)
  #download.file(url, file, mode="wb")
  pdf_text(file)
}) 

#rawdata <- lapply(links, function(url){
#  file <- basename(url)
#  download.file(url, file, mode="wb")
#  extract_tables(file)
#}) |> 
#  bind_rows(id="id")


#dfs <- list()
#i<- rawdata[[61]]
#df <- dfs[[1]]
cleandata <- lapply(rawdata, function(i){
  dfs[[length(dfs)+1]] <- tibble(txt = i) |> 
      separate_rows(txt, sep="\n") 
  dfs[[length(dfs)]] <- dfs[[length(dfs)]]|> 
      mutate(title=str_extract(trimws(txt), "[0-9][0-9][0-9][0-9] (Girls|Boys) Track Class [ABCD]? ?State Championships"),
             txt = trimws(txt)) |>
      fill(title) |> 
      separate_wider_regex(title,
                           c(" *",
                             Year = "[12][0-9][0-9][0-9]",
                             " ",
                             Gender = "(?:Girls|Boys)",
                             " ?(?:Track)? ?Class ",
                             Class = "[A-Z]",
                             ".*")) |> 
      mutate(Event = ifelse(str_detect(txt, "^[\t ]*([0-9]+ (Meter|Yard)|[A-Za-z]+ Jump|(Two )?Mile (Run|Relay)|Pole Vault|High Jump|Shot Put|Discus)"),
                            txt,
                            NA)) |> 
      fill(Event) |>
      separate_wider_regex(Event, c(Event_1 = "[0-9A-Za-z ]*",
                                    "(?:  +)",
                                    Event_2= "[0-9A-Za-z ]*"),
                           too_few="align_start") |> 
      filter(txt != "", str_detect(txt, "^[0-9] ") | trimws(Event_1) == "High Jump") |>
      drop_na(txt) |> 
      separate_wider_regex(txt,  
                           c("[ \t]*",
                             Rank_1 = "[1-6]?(?: tie)?",
                             "[ \t]*",
                             Name_1 = "[A-Z][A-Za-z\\-\\.\\'/ ]+",
                             "[ \t]+",
                             Mark_1 = "(?:[0-9:][0-9\\-\\.:,;]*| )?",
                             "[ \t]*(?:\\([A-Za-z,/\\. ]+|[A-Za-z,/\\. ]+\\)| )?[ \t]*",  
                             Rank_2 = "[1-6]?(?: tie)?",
                             "[ \t]*",
                             Name_2 = "[A-Z][A-Za-z\\-\\'\\./ ]*",
                             "[ \t]*",
                             Mark_2 = "[0-9\\-\\.:,;]*",
                             "[ \t]*(?:\\([A-Za-z,/\\. ]+|[A-Za-z,/\\. ]+\\))?[ \t]*"), too_few = "align_start") |> 
      mutate(id = row_number()) |> 
      pivot_longer(c(Rank_1, Name_1,Mark_1, Rank_2, Name_2, Mark_2, Event_1, Event_2),
                   names_to=c("Name", "Number"),
                   names_sep="_") |> 
      pivot_wider(names_from ="Name", values_from="value") |> 
      select(-id, -Number) |> 
      filter(Rank == "1" | Rank == "") |> 
      mutate(Event = str_replace(Event, " Meter (Run|Dash)", "m"),
             Event = str_replace(Event, " Yard (Run|Dash)", "y"),
             Event = trimws(Event),
             Event = ifelse(Event == "Mile Run", "Mile", Event),
           State="Nebraska",
           Year = as.numeric(Year)) |> 
      filter(Mark != "", !is.na(Mark), 
             !(Event=="High Jump" & Mark=="17-06"),
             !(Event=="High Jump" & Mark=="18-02.75"),
             !(Event=="Pole Vault" & Mark == "44-03"),
             !(Event=="Pole Vault" & Mark == "44-01.75")) %>% 
      filter(Event %in% c("100m", "100y", "800m", "880y", "Mile", "1600m", "High Jump", "Pole Vault"))
}) |> 
  bind_rows() |> 
  select(-Rank)

write.csv(cleandata, "nebraska_clean_pdf.csv", row.names = F)

web <- read.csv("nebraska_clean_web.csv") 
cleandata <- bind_rows(list(cleandata, web))

write.csv(cleandata, "nebraska_clean.csv", row.names = F)

#c<-dfs[[1]]

cleandata |> 
  count(Year, Event, Gender, Class) |> 
  print(n=1000)

c<-cleandata |> 
  mutate(s=str_extract(txt, "^[ \t]*[0-9]+ +[A-Za-z\\-\\.\\'/ ]+ +[0-9\\-\\.:]* *[\\(\\)A-Za-z,/ ]*[0-9]* +[A-Za-z\\-\\'\\./ ]+[0-9\\-\\.:]+ *[\\(\\)A-Za-z, ]*$")) |> 
  select(txt, s)

str <- "	4 Lyndsey Finney       Kearney Catholic        17-00.5  4 Jill Heywood        Scribner-Snyder               5-02"
str <- "1 Delana Hanes           Bellevue West                                           1 Kelly Cizek            Millard South                      5-8"
str <- "3 Millard North      8:13.78"

str_extract(str, "")
str_extract(str, "[ \t]*[1-6]?[ \t]+[A-Z][A-Za-z\\-\\.\\'/ ]+[ \t]+([0-9:][0-9\\-\\.:,]*)?[ \t]+(\\([A-Za-z,/\\. ]+|[A-Za-z,/\\. ]+\\))?[ \t]+[1-6][ \t]*[A-Z][A-Za-z\\-\\'\\./ ]*[ \t]*[0-9\\-\\.:,]*$")



str_extract(trimws(str), "^[ \t]*[0-9]+[ \t]+[A-Za-z\\-\\.\\' ]+")
str_extract(str, "^[ \t]*[0-9]+ +[A-Za-z\\-\\.\\' ]+ +[0-9\\-\\.:]* *[\\(\\)A-Za-z, ]*[0-9]* +[A-Za-z\\-\\'\\. ]+[0-9\\-\\.:]+ *[\\(\\)A-Za-z, ]*$")
                      