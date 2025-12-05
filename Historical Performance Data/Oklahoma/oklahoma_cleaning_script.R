library(tidyverse)
library(rvest)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Oklahoma\\")

links <- list()
for(g in c("girls", "boys")){
    for(e in c("100", "800", "1600", "hj", "pv")){
      links[[length(links)+1]] <- paste("http://www.ohstrack.com/pastchampions/past",e,g,".html", sep="")
    }
}

html <- lapply(links, read_html)

l <- html[[3]]
cleandata <- lapply(html, function(l){ 
  title <- html_elements(l,"p")[1:3] |> 
    html_text()
 c<- html_table(l)[2] |> 
    data.frame() |> 
    rename(Year = X1,
           Class = X2,
           Text = X3) |> 
    fill(Year) |> 
    filter(Text != "") |> 
    mutate(Text = trimws(Text),
           Text = str_replace_all(Text, "[\r\n\t]", " ")) |> 
    separate_wider_regex(Text, c(Name = "[^0-9]*",
                                 "[, \t\r\n]*",
                                 Mark = "[0-9:\\.\\-]+",
                                 Mark.Note = "(?:[hw]|)",
                                 "[ \t\r\n]*"), too_few="align_start") |> 
   
   mutate(Gender = str_extract(title[1], "(Girls|Boys)"),
          Event = trimws(title[3]),
          Event = str_replace_all(Event, "[ \r\n\t]+", " "),
          Event = case_when(
            Event == "100" & Year > 1982 ~ "100m",
            Event == "100" & Year <= 1982 ~ "100y",
            Event == "800" & Year > 1982 ~ "800m",
            Event == "800" & Year <= 1982 ~ "880y",
            Event == "1600" & Year > 1982 ~ "1600m",
            Event == "1600" & Year <= 1982 ~ "Mile",
            TRUE ~ Event
          ),
          Event = ifelse(Event == "1600 / Mile",
                         ifelse(Year <= 1982 , "Mile", "1600m"),
                         Event),
          Mark = case_when(
            Year == 1990 & Event == "1600m" & Mark=="519.81" ~ "5:19.81",
            TRUE ~ Mark
          ),
          State = "Oklahoma") %>% 
   filter(!(Year == 1982 & Event == "Mile" & Mark == "1"))
}) |> 
  bind_rows() |> 
  select(State, Year, Gender, Event, Class, Mark, Mark.Note)

cleandata |> 
  count(Year, Gender, Event) |> 
  print(n=1000)

write.csv(cleandata, "oklahoma_clean.csv", row.names = F)

"Gianna Moulis, Glenpool, 12.47"
str <- "Gianna Moulis, Glenpool, 12.47"
str_extract(str, "^[A-Za-z\\-\\.\\'�  \t\r\n]+[,\t\r\n]*[A-Za-z\\-\\.\\'�  \t\r\n]+[, \t\r\n]*[0-9;:\\-\\.]+$")

str_replace_all("1600 /\r\n      Mile", "[\r\n ]+", " ")
