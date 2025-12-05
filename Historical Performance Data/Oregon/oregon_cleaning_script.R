library(tidyverse)
library(rvest)
library(tabulapdf)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Oregon")

"https://www.osaa.org/docs/btf/records/20196a5aChampionsOnly.htm"

names <- '<select name="records" class="yearbyyearlist" data-root="https://www.osaa.org/docs/btf/records">
  <option value="">Select Year</option>
  <option value="20196a5aChampionsOnly.htm">2019 6A-5A</option>
  <option value="20194a3aChampionsOnly.htm">2019 4A-3A</option>
  <option value="20192a1aChampionsOnly.htm">2019 2A-1A</option>
  <option value="2018ChampionsOnly.htm">2018</option>
  <option value="2017ChampionsOnly.htm">2017</option>
  <option value="2016ChampionsOnly.htm">2016</option>
  <option value="2015ChampionsOnly.htm">2015</option>
  <option value="2014ChampionsOnly.htm">2014</option>
  <option value="20136a5a4aChampionsOnly.htm">2013 6A-5A-4A</option>
  <option value="20133a2a1aChampionsOnly.htm">2013 3A-2A-1A</option>
  <option value="20126a5a4aChampionsOnly.htm">2012 6A-5A-4A</option>
  <option value="20123a2a1aChampionsOnly.htm">2012 3A-2A-1A</option>
  </select>' |> 
  str_extract_all("20[0-9][0-9][1-6a]*ChampionsOnly.htm")
  names <- str_split(names[[1]], " ")
  names_pdf <- list("boysTrackChampionsThrough2011",
                           "girlsTrackChampionsThrough2011",
                           "btrackchampsthrough06",
                           "gtrackchampsthrough06")

html <- lapply(names, function(l){
  read_html(paste("https://www.osaa.org/docs/btf/records/", l, sep=""))
  })

l <- html[[6]]  
dfs <- list()
cleandata1 <- lapply(html, function(l){
  dfs[[length(dfs)+1]]<- data.frame(txt = html_text(l)) |> 
    separate_rows(txt, sep="\n") |> 
     mutate(header = str_extract(txt, "(Girls|Boys) [A-Za-z0-9 ]+ [0-9]A"),
            Year = str_extract(txt, "[12][0-9][0-9][0-9] OSAA"),
            Year = str_extract(Year, "[12][0-9][0-9][0-9]")) |> 
     fill(header, Year) |> 
     separate_wider_regex(header, c(" *",
                                    Gender = "(?:Girls|Boys)",
                                    " *",
                                    Event = "[A-Za-z0-9 ]+",
                                    " *",
                                    Class = "[0-9]A",
                                    " *")) |> 
     filter(str_detect(txt, "1 [^0-9]+ +[0-9]* [^0-9]") ,
              !str_detect(txt, "[0-9]\\) [^0-9]")) |>
     separate_wider_regex(txt, c(" *",
                                 Rank = "1",
                                 " *",
                                 Name = "[^0-9]+",
                                 " *",
                                 Grade = "[0-9]*",
                                 " *",
                                 School = "[^0-9]+",
                                 " *",
                                 Prelim = "(?:[0-9]+[\\.\\-:]+[0-9\\.\\-:]+)?",
                                 " +",
                                 Mark = "[0-9]+[\\.\\-:]+[0-9][0-9\\.\\-:]+",
                                 " *",
                                 Mark.Note = "(?:[\\+\\-]?[0-9]\\.[0-9])?",
                                 ".*"), too_few="debug")
})  |> 
  bind_rows()  |> 
  mutate(Event = trimws(str_replace(Event, " Meter (Run|Dash)", "m")),
         State = "Oregon",
         Year = as.numeric(Year)) |> 
  filter(Event %in% c("100m", "800m", "1500m", "High Jump", "Pole Vault"))|> 
  select(State, Year, Gender, Event, Class, Mark, Mark.Note)

#c<- dfs[[1]]



#cleandata |> 
#  filter(Year == 2016, Event == "High Jump", Gender=="Boys")

#cleandata |> 
#  count(Year, Event, Gender) |> 
#  print(n=500)

## PDFS
url <- "boysTrackChampionsThrough2011.pdf"


rawdata <- lapply(names_pdf, function(l){
  read.csv(paste("tabula-",l,".csv", sep=""), header=F) 
})|> 
  bind_rows(.id="Gender")

cleandata2 <- rawdata |> 
  mutate(Text = paste(V1,V2,V3),
         Text = gsub(" NA", "", Text)) |> 
  select(Text, Gender) |> 
  mutate(Gender = ifelse(as.numeric(Gender) %% 2 == 0, "Girls", "Boys"),
         Year = str_extract(Text, "^[12][90][0-9][0-9]"),
         Text = str_to_title(Text),
         Event = str_extract(Text, "([0-9x]+0(m|-Meter|-Yard) (Dash|(Medley )?Relay|Run|R|[A-Za-z]+ Hurdles|H)|(One|Two) Mile Run|(Long|High|Triple) Jump|Pole Vault|Shot Put|Javelin|Discus)")) |> 
  fill(Year, Event) |> 
  filter(!str_detect(Text, "^ *[1-6]a *$")) |> 
  filter(!str_detect(Text, paste(Event, "*[1-6]a *$" ))) |> 
  filter(!str_detect(Text, "^ *\\(.*\\) *$")) |> 
  filter(str_detect(Text, "[0-9A-Za-z ]*[A-Za-z, \\-\\'’\\./]+ ?\\.*[0-9\\.:\\'/ \\-⁄]+[hwa]?")) |> 
  separate_wider_regex(Text, c("[0-9A-Za-z \\(\\)]*",
                               Name = "[A-Za-z, \\-\\'’\\./\\(\\)]+",
                               " ?\\.*",
                               Mark = "[0-9\\.:\\'/ \\-⁄]+",
                               Mark.Note="[hwa]?"
                                ), too_few = "align_start") |> 
  mutate(Mark = ifelse(trimws(Mark) == "", NA, Mark)) |> 
  drop_na(Mark) |> 
  filter(Mark != ".") |> 
  mutate(Year = as.numeric(Year),
         Event = str_replace(Event, "-Meter (Dash|Run)", "m"),
         Event = str_replace(Event, "-Yard (Dash|Run)", "y"),
         Event = str_replace(Event, " (Dash|Run)", ""),
         Event = str_replace(Event, "One Mile", "Mile")) |> 
  filter(!(Event=="1500m"& Mark=="3:08.99")) %>% 
  filter(Event %in% c("100y", "100m", "880y", "800m", "Mile", "1500m",
                      "High Jump", "Pole Vault")) |> 
  mutate(State = "Oregon") |> 
  select(State, Year, Gender, Event, Mark, Mark.Note)

cleandata <-  
  bind_rows(list(cleandata1, cleandata2))

write.csv(cleandata, "oregon_clean.csv", row.names = F)
  
cleandata |> 
  filter(Year >= 1978) |> 
  count(Gender, Event, Year) |> 
  print(n=200)

#names_pdf[[4]]
#extract_tables(names_pdf[[4]])

#t <- extract_tables(names_pdf[[4]]) |> 
#  bind_rows()


paste("https://www.osaa.org/docs/btf/records/", url, sep="")

str <- "Girls 1500 Meter Run 1A"
str_extract(str, "(Girls|Boys) [A-Za-z0-9 ]+ [0-9]A")

str <- " 1 Colton Myers                 Silverton            6-01.00    6-02.00  "
str <- "	3) Noemiamalia Bernabe 11          4) Kerri McMahan 9   "
str_detect(str, "1 [^0-9]+ +[0-9]* [^0-9]")
!str_detect(str, "[0-9]\\) [^0-9]")
            