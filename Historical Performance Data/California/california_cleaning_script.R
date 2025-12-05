library(tidyverse)
library(rvest)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\California\\")

setwd("./Raw/")
rawdata <- lapply(list.files(pattern="text.*"), function(x){
  read.csv(x, colClasses=c("numeric","character"))
  }) %>% 
  bind_rows()

r1 <- rawdata[1,] %>% 
  separate_longer_delim(Text, "\n") 

r2 <- rawdata[43,]%>% 
  separate_longer_delim(Text, "\n") 

r3 <- rawdata[100,]%>% 
  separate_longer_delim(Text, "\n") 

r4 <- rawdata[145,]%>% 
  separate_longer_delim(Text, "\n") 

event_str <- "((4?x?([1-8][0-9]0+(y| Yard)|[1-9]?[1-9][0-9]0(m| Meter))|([12] )?Mile)( Run| Dash| Hurdles| ?H| Relay)?)|((High|Long|Triple) Jump|Discus|Hammer|Javelin|Pole Vault|Shot Put)|Team (Rankings|Score)?"


parse_rows <- function(r){
  row <- r %>% 
    mutate(
      Gender = str_extract(Text, "(Girls|Boys)"),
      Event = case_when(
        str_detect(Text, "(Boys|Girls|Men|Women)") ~ 
          str_extract(Text, event_str),
        TRUE ~ NA
      ),
      Stage = case_when (
        !is.na(Event) ~ "-",
        TRUE ~ str_extract(Text, "Prelim|Final")
      )
    ) %>% 
    fill(Gender, Event, Stage) %>% 
    filter(str_detect(Text, "^ *[0-9\\-\\)]+( +[1]?[9012])? +[A-Z]")) %>% 
    filter(!str_detect(Text, "[1234]\\)? *[A-Za-z\\-\\'\\. ]+ *(9|10|11|12)? *[1234]\\)? *[A-Za-z\\-\\'\\. ]+(9|10|11|12)? *$")) %>% 
    filter(!str_detect(Text, "^[0-9\\-\\.FOUL ]+$")) %>% 
    filter(!(Event  %in% c("Team Rankings", "Team Score"))) %>% 
    mutate(Mark = str_extract(Text, "[0-9][0-9:\\.\\-\\'\"]+[0-9]m?|(NM|NT|NH|DNF|DQ|FS)"),
           Place = str_extract(Text, "^ *(([1-4]?[0-9])|--)"),
           Wind = trimws(ifelse(str_detect(Event,"[12][20]0"),
                                str_extract(Text," [+\\-]?[0-9]\\.[0-9]( |$)"),
                                NA)))
  return(row)
  
}
rawdat <- rawdata[24:50,]
rowlist <- list()
for(i in 1:nrow(rawdata)){
  row<- data.frame(rawdata[i,] %>% separate_longer_delim(Text, "\n"))
  rowlist[[i]]<- parse_rows(row)
}
data<-rowlist %>% bind_rows() %>% 
  mutate(
    State="California",
    Event.raw = Event,
    Event = gsub(" Yard( Dash| Run)?","y",Event),
    Event = gsub(" Meter( Dash| Run)?", "m", Event),
    Event = gsub(" Run","", Event),
    Event = gsub("1 Mile","Mile", Event)
 ) %>% 
  filter(!is.na(Mark))

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\California\\")
write.csv(data, file="california_clean.csv", row.names=F)

data %>% 
  group_by(Year, Gender, Event) %>% 
  summarize(n()) %>% 
  print(n=2000)

data %>% 
  select(Event, Event.raw) %>% 
  distinct() %>% 
  arrange(Event)

urls <- list("https://ca.milesplit.com/meets/339050-cif-state-track-and-field-championships-2019/results",
             "https://ca.milesplit.com/meets/298564-cif-state-track-and-field-championships-2018/results",
             
)

url <- "https://ca.milesplit.com/meets/339050-cif-state-track-and-field-championships-2019/"

years <- read_html(url) %>% 
  html_elements("main") %>% 
  html_element("#subheader") %>% 
  html_element(".meet") %>% 
  html_elements(".dropdownMenus") %>% 
  html_element(".meetHistory") %>% 
  html_element(".dropdown") %>% 
  html_element(".pastMeets") %>% 
  html_element(".meets") %>% 
  html_elements("a") %>% 
  lapply(., function(x){
    #if(as.numeric(str_extract(html_attr(x, "href"), "[12][90][0-9][0-9]$")) > 1977){
    #  print(html_attr(x, "href"))
    #}
    #print(html_text(x))
    #print(html_attr(x, "href"))
    #ifelse(as.numeric(str_extract(html_attr(x, "href"), "[12][90][0-9][0-9]$")) > 1977 &
    #         as.numeric(str_extract(html_attr(x, "href"), "[12][90][0-9][0-9]$")) < 2020 ,
           html_attr(x, "href") #,
    #       NA)
  }) %>% 
  c(url, .) %>% 
  unlist() %>% 
  data.frame(url = .) %>% 
  filter(url!="#back")
  drop_na()
#write.csv(years, "Historical Performance Data/California/ca_links_year.csv", row.names = F)  


files <- read_html(paste(years$url[43], "/results", sep="")) %>% 
  html_element("#resultFileList") %>% 
  html_nodes("a") %>% 
  lapply(., function(x){
    print(html_text(x))
    if(str_detect(html_text(x), "(([^x](1|8|16)00m? ?(Dash|Run)?|High Jump|Pole Vault)|Complete Results|(Complete|Field Event|Track|Field|Running|Saturday) Final|Final Results)")){
      print(html_text(x))
      print(html_attr(x, "href"))
    }
  })

read_html(paste(years$url[43], "/results", sep="")) %>% 
  html_elements("table")

url <- "https://ca.milesplit.com/meets/225725-cif-state-track-and-field-championships-2016/results/456401/raw"

read_html(url) %>% 
  html_elements("body") %>% 
  html_elements("#page") %>% 
  html_elements("main") %>% 
  html_element("#content") %>% 
  html_elements("div") %>% 
  html_elements("article") %>% 
  html_element("#meetResultsBody")

