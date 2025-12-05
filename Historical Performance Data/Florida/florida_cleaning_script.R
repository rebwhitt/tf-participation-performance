library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Florida\\")

rawdata <- lapply(list("tabula-Track_Field_Girls_2024.csv",
                       "tabula-Track_Field_Boys.csv"),read.csv) |> 
  bind_rows(.id="Gender")


cleandata <- rawdata |> 
  rename(Year = X,
         Class = X.1,
         Name = X12.5) |>
  mutate(
    Name = ifelse(Name == "3A Tywana Dickerson, Ribault (Jacksonville),",
                  "3A Tywana Dickerson, Ribault (Jacksonville), 2:18.91", Name),
    Name = coalesce(Name, X10),
    Name = ifelse(Class == "" & Name == "",
                  str_extract(Year,
                    "[1-6]?[ABC]? ?[A-Za-z ,\\(\\)]*[ \\.0-9:\\.\"\'’”\\(\\)/]*$"),
                  Name) ,
    Name = ifelse(Name == "",
                  str_extract(Class,
                              " ?[A-Za-z ,\\(\\)\\.]*[0-9:\\.\"\'\\(\\)]*$"),
                  Name) ,
    Name = ifelse(Class=="" & is.na(Name), 
                  str_extract(Year, "[A-Za-z ,\\(\\)\\.]*[0-9\\.\"\':”’ \\-/m]*$"),
                  Name) ,
    Class = ifelse(str_detect(Class, "^[1-9]?[ABC]( .*)?$"),
                   str_extract(Class, "^[1-9]?[ABC]"),
                   "") ,
    Class = ifelse(str_detect(Year, "[1-9]?[ABC]"), str_extract(Year, "[1-9]?[ABC]"), Class),
    Year = str_extract(Year, "[12][90][0-9][0-9]"),
    #Name = ifelse(Name == "Nicole Tunsil, Lakewood (St. Petersburg)," & Year =="1987",
     #             "Nicole Tunsil, Lakewood (St. Petersburg), 5’6”", Name)
         ) |> 
  filter(Name != "01:54.7") |> 
  select (Year, Class, Name, Gender)


# Put rows that wrapped into the next all on the same line
## e.g.:
## 4A   Houston,
##      9.09
# -->
## 4A   Houston, 9.09
for(i in nrow(cleandata):1) {
  if(is.na(cleandata[i,2]) || cleandata[i,2]==""){
    cleandata[i-1,3] <- (paste(cleandata[i-1,3], cleandata[i,3]))
  }  else if(is.na(cleandata[i,3])){
    cleandata[i-1,3] <- (paste(cleandata[i-1,3], cleandata[i,2]))
  }
}

cleandata <- cleandata |> 
  mutate(
         #Year = as.numeric(Year),
         Year = ifelse(Name == "Frank Ogles, Niceville, 1:55.8", 1971, Year),
         Year = ifelse(Name == "William Banks, Lake Gibson (Lakeland), 01:56.0", 1986, Year),
         Year = ifelse(Name == "A Paul McNulty, Trinity Prep (Winter Park), 04:22.4", 1980, Year),
         Year = ifelse(Name == "Lewis McDonald, Fort Meade, 6’0”", 1972, Year),
         Year = ifelse(Name == "A Jeanie Messinese, Beaches Chapel (Neptune Beach), 2:20.6", 1981, Year),
         Year = ifelse(Name == "Erika Schlomer, Cocoa Beach, 2:23.96", 1994, Year),
         Year = ifelse(Name == "Gion Enzor, Havana Northside, 5’4”", 1987, Year),
         Year = ifelse(Name == "Tiara McMinn,  Jackson (Jacksonville), 5'8''", 2016, Year),
         Year = ifelse(Name == "Eddie Roberts, Dillard (Fort Lauderdale), 6’10”", 1989, Year),
         Year = ifelse(Name == "Patrick Johnson, Santa Fe (Alachua), 13’0”", 2001, Year),
         Year = ifelse(Name == "Houston McTear, Baker, 10.0", 1976, Year),
         Year = ifelse(Name == "Lee Hatch, Branford, 11’6”", 1973, Year)
         )|> 
  fill(Year) |> 
  filter(Year != "1975") |> 
  filter(!is.na(Name), Class!="") |> 
  mutate(Name = str_replace(Name, ";", ""),
         Name = str_replace(Name, " Meter Dash", ""),
         Name = str_replace(Name, " Meter Run", ""),
         Name = str_replace(Name, " Pole Vault", ""),
         Mark = str_extract(Name, "[0-9\\.\"\':”’  \\-/m]+$"),
         Mark = str_replace(Mark, "\\. ", ""),
         Mark = ifelse(Name == "Bobby Williams, Buchholz (Gainesville), 10:48", "10.48", Mark),
         Mark = ifelse(Name == "Krystal Sparling, St. Thomas Aquinas (Fort Lauderdale), 11:46", "11.46", Mark),
         Mark = ifelse(Name == "Cris Collinsworth, Astronaut (Titusville),", "10.0", Mark),
         Mark = ifelse(Name == "Stacy Johnson, Trinity Prep (Winter Park),", "12.5", Mark),
         Mark = ifelse(Name == "Mike Kursteiner, Nature Coast (Brooksville), 10-Jun", "6\'10\"", Mark),
         Mark = ifelse(Name == "William Lang, Chamberlain (Tampa),", "1:54.65", Mark),
         Mark = ifelse(Name == "Ryan Albertson, Pine Forest (Pensacola),", "01:54.7", Mark),
         Mark = ifelse(Name == "Nicole Tunsil, Lakewood (St. Petersburg),", "5’6”", Mark),
         Mark = ifelse(Year == 2018 &
                         str_detect(Name, "Stone Baker, River Ridge \\(New Port Richey\\)") &
                         str_detect(Mark, "5.09"), "5.09m", Mark),
         Gender = ifelse(Gender ==1, "Girls", "Boys"),
         Event = case_when(
           str_detect(Mark, "^ ?[019][0129]?[\\.:][0-9]*$") ~ "100m",
           str_detect(Mark, "[12]:[0-9][0-9][\\.:][0-9]*") ~ "800m",
           str_detect(Mark, "[345]:[0-9][0-9]\\.?[0-9]*") ~ "1600m",
           str_detect(Mark, "^ ?([4567][’\\'][0-9]|[12][0-9\\.]*m$)") ~ "High Jump",
           str_detect(Mark, "^ ?(1?[0-9][’\\'-][0-9]|[345]\\.+[0-9]*m?$)") ~ "Pole Vault",
           TRUE ~ NA
         ),
         Event = ifelse(Event == "100m" & Year < 1986, "100y", Event),
        Event = ifelse(Event == "800m" & Year < 1990, "880y", Event),
        Event = ifelse(Event == "1600m" & Year < 1990, "Mile", Event),
        State = "Florida") 

write.csv(cleandata, "florida_clean.csv", row.names = F)

cleandata |> 
  count(Year,Gender,Class) |> 
  filter(n!=5, Gender=="Girls")

str_extract("1996 6A Kelly Greeno, Boone (Orlando), 9’6”",
            "[1-6]?[ABC]? ?[A-Za-z ,\\(\\)\\.0-9:\\.\"\'’”\\(\\)]*$")
Name="James Hall, Robinson (Tampa), 6’6 1/4”"
str_extract(Name, "[0-9\\.\"\':”’  \\-/m]+$")

cleandata %>% 
  filter(Year == 2018 &
           str_detect(Name, "Stone Baker, River Ridge \\(New Port Richey\\)") &
           str_detect(Mark, "5.09"))
