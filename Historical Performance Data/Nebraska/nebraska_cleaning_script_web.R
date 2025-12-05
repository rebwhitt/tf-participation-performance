library(tidyverse)
library(readxl)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Nebraska\\")
rawdata <- read_excel("Nebraska.xlsx")

cleandata1 <- rawdata |> 
  rename(Year = Year...6,
         Gender = Gender...7,
         Class = Class...8,
         Text = Text.Column1) |> 
  select(Year, Gender, Class, Text) |> 
  filter(Year < 2012) |> 
  mutate(Text2 = str_split(Text, "Return to (Event )?INDEX")) |> 
  unnest(Text2) |> 
  mutate(Text = coalesce(Text2, Text)) |> 
  select(-Text2) |> 
  mutate(Text = str_replace(Text, "( |\r\n)METER", "m"),
         Text = str_replace(Text, "RUN", "Run"),
         Text = str_replace(Text, "DASH", "Dash"),
         Text = str_replace(Text, "HIGH ?(\r\n)?JUMP", "High Jump"),
         Text = str_replace(Text, "POLE( |\r\n)VAULT", "Pole Vault")) |> 
  mutate(Text = ifelse(str_detect(Text, "^[12][0-9][0-9][0-9] NSAA"),
                       str_extract(Text, regex("(High Jump|Triple Jump|Pole Vault|Long Jump|Discus) *Class [A-D] State Record.*", dotall=T)),
                       Text)) |> 
  mutate(Text = trimws(Text)) |>
  filter(str_detect(Text, "^(100m Dash|800m Run|1600m Run|High Jump|Pole Vault)"),
         !str_detect(Text, "PRELIM")) |> 
  mutate(
    Event = str_extract(Text, "^(100m|800m|1600m|High Jump|Pole Vault)"),
    Best = 
      str_extract_all(Text, "1st, [0-9\\-:\\.\'\" /]+, ?[A-Za-z \\-\\'\\./]+, ?[A-Za-z \\-\\.\\'/]+, ?[0-9]+")) |>
  select(-Text) |> 
  unnest(Best) |> 
  separate_wider_regex(Best, c(Rank=" ?[0-9]+",
                               "st, +",
                               Mark = "[0-9\\-:\\.\'\" /]+",
                               ", ?",
                               Name = "[A-Za-z \\-\\'\\.]+",
                               ", ?",
                               School = "[A-Za-z \\-\\.\\'/]+",
                               ", ?",
                               Grade = "[0-9]+"
                               )) |> 
  mutate(Gender = ifelse (toupper(Gender)=="G", "Girls", "Boys"),
         State = "Nebraska") |> 
  select(State, Year, Gender, Event, Class, Mark)
  
str_extract(" 1st, 15' 8-1/2\", Van Sanderfer, Lincoln Southwest,12",
            "1st, [0-9\\-:\\.\'\"/ ]+, ?[A-Za-z \\-\\'\\.]+, ?[A-Za-z \\-\\.\\'/]+, ?[0-9]+" )  
  
cleandata2 <- rawdata |> 
  rename(Year = Year...6,
         Gender = Gender...7,
         Class = Class...8,
         Text = Text.Column1) |> 
  select(Year, Gender, Class, Text) |> 
  filter(Year > 2012) |> 
  mutate(Text = str_replace(Text, " METER", "m"),
         Text = str_replace(Text, "RUN", "Run"),
         Text = str_replace(Text, "DASH", "Dash"),
         Text = str_replace(Text, "HIGH JUMP", "High Jump"),
         Text = str_replace(Text, "POLE VAULT", "Pole Vault")) |> 
  mutate(Text = ifelse(str_detect(Text, "^[12][0-9][0-9][0-9] NSAA"),
                       str_extract(Text, regex("(High Jump|Triple Jump|Pole Vault|Long Jump|Discus) *Class [A-D] State Record.*", dotall=T)),
                       Text)) |> 
  mutate(Text = trimws(Text)) |> 
  filter(str_detect(Text, "^(100m Dash|800m Run|1600m Run|High Jump|Pole Vault)"),
         !str_detect(Text, "PRELIM")) |> 
  mutate(
    Event = str_extract(Text, "^(100m|800m|1600m|High Jump|Pole Vault)"),
    Best = 
    str_extract_all(Text, " 1(st,)? +[A-Za-z \\-\\'\\.]+ +[0-9]+ +[A-Za-z \\-\\.\\'/]+ *[0-9\\-:\\.]+")) |> 
  select(-Text) |> 
  unnest(Best) |> 
  separate_wider_regex(Best, c(Rank=" *[0-9]+",
                               " +",
                               Name = "[A-Za-z \\-\\'\\.]+",
                               " +",
                               Grade = "[0-9]+",
                               " +",
                               School = "[A-Za-z \\-\\.\\'/]+",
                               " *",
                               Mark = "[0-9\\-:\\.]+",
                               "(?: *All Class State Rec. *)?")) |> 
  filter(!(Mark %in% c("-", "."))) |> 
  mutate(Gender = ifelse (toupper(Gender)=="G", "Girls", "Boys"),
         State = "Nebraska") |> 
  distinct() %>% 
  select(State, Year, Gender, Event, Class, Mark)

cleandata3 <- rawdata |> 
  rename(Year = Year...6,
         Gender = Gender...7,
         Class = Class...8,
         Text = Text.Column1) |> 
  select(Year, Gender, Class, Text) |> 
  filter(Year == 2012) |> 
  mutate(Text = str_replace(Text, " METER", "m"),
                               Text = str_replace(Text, "RUN", "Run"),
                               Text = str_replace(Text, "DASH", "Dash"),
                               Text = str_replace(Text, "HIGH JUMP", "High Jump"),
                               Text = str_replace(Text, "POLE VAULT", "Pole Vault")) |> 
  mutate(Text = ifelse(str_detect(Text, "^[12][0-9][0-9][0-9] NSAA"),
                       str_extract(Text, regex("(High Jump|Triple Jump|Pole Vault|Long Jump|Discus) *Class [A-D] State Record.*", dotall=T)),
                       Text)) |> 
  mutate(Text = trimws(Text)) |> 
  filter(str_detect(Text, "^(100m Dash|800m Run|1600m Run|High Jump|Pole Vault)"),
         !str_detect(Text, "PRELIM")) |> 
  mutate(
    Event = str_extract(Text, "^(100m|800m|1600m|High Jump|Pole Vault)"),
    Best = 
      str_extract_all(Text, " ?1\\. +[A-Za-z \\-\\.\\'/]+, [A-Za-z \\-\\.\\'/]+, [0-9\\-:\\.]+")) |> 
  select(-Text) |> 
  unnest(Best) |>
  separate_wider_regex(Best, c(Rank=" ?[0-9]+",
                               "\\. +",
                               Name = "[A-Za-z \\-\\'\\.]+",
                               ", ?",
                               School = "[A-Za-z \\-\\.\\'/]+",
                               ", ?",
                               Mark = "[0-9\\-:\'\" /]+(?:\\.[0-9][0-9])",
                               "[0-9\\-]*\\.?"
  ))|> 
  mutate(Gender = ifelse (toupper(Gender)=="G", "Girls", "Boys"),
         State = "Nebraska") |> 
  select(State, Year, Gender, Event, Class, Mark)

cleandata <- bind_rows(list(cleandata1, cleandata2, cleandata3))

write.csv(cleandata, "nebraska_clean_web.csv", row.names = F)

cleandata |> 
  count(Year, Gender, Event) |> 
  print(n=500)

t2<- " 1 Lindsey Blehm                11 Lincoln Southwest                      2:16.42"
t <- "High JumpClass A State Record: 5-11.00 Meredy Porter, Bellevue West 1987Class A Meet Record: 5-11.00 Meredy Porter, Bellevue West 1987All-Class State Record: 5-11.00 Meredy Porter, Bellevue West 1987All-Class Meet Record: 5-11.00 Meredy Porter, Bellevue West 1987==========================================================================================     Name                      Grade School                                   Final H#  Pts ==========================================================================================   1 Erica Broin                  11 Papillion-La Vista                     5-05.00    10   2 Lauren Harris                10 Omaha Marian                           5-04.00     8   3 Bianca Martinez               9 Omaha Central                          5-04.00     6   4 Norah Sis                    10 Papillion-La Vista                     5-03.00     5   5 Saylor Schaefer              10 Lincoln North Star                     5-03.00     4   6 Kaitlin Hellbusch            11 Lincoln North Star                     5-02.00     3  7T Alexis Sovereign             10 Norfolk                                5-00.00     0.6  7T Shelby Bergholz              12 Millard West                           5-00.00     0.6  7T Brianna Eilderts             11 Omaha Marian                           5-00.00     0.6  7T Taylor Yakel                 11 Lincoln Southeast                      5-00.00     0.6  7T Kaylee Berry                 11 Lincoln Southwest                      5-00.00     0.6  12 Isabel Henson                12 Gretna                                 5-00.00  13T Cierra Marks                  9 Omaha Northwest                        5-00.00  13T Jessica Hendrix              12 Papillion-La Vista South               5-00.00   15 Danielle Hall                10 Omaha Burke                            5-00.00  16T Amarie Baerentzen            10 Elkhorn                                5-00.00  16T Rylee Gray                   10 Elkhorn South                          5-00.00   18 Alyssa Peoples                9 Omaha Burke                            4-10.00   -- Emma Hilderbrand             11 Grand Island                                NM "
t3 <- "2014 NSAA State Track & Field Meet ResultsCLASS A BOYSThis page was last updated on May 24, 2014 at 7:06pm EDT. REFRESH this page to see the most recent updates.Field EventsHigh JumpShot PutTriple JumpPole VaultDiscusLong JumpRunning Events3200m Relay FINALS3200m Run FINALS800m Run FINALS400m Relay FINALS110m Hurdles PRELIMS110m Hurdles FINALS100m Dash PRELIMS100m Dash FINALS400m Dash PRELIMS400m Dash FINALS1600m Run FINALS300m Hurdles PRELIMS300m Hurdles FINALS200m Dash PRELIMS200m Dash FINALS1600m Relay FINALSTeam Scores after 17 events:1.Kearney83.0002.Papillion-La Vista77.0003.Millard West76.0004.Millard North55.0005.Creighton Preparatory School48.0006.Grand Island34.0007.Millard South29.0008.Omaha North28.0009.Fremont28.00010.Papillion-La Vista South23.00011.Lincoln East23.00012.Bellevue West19.00013.Lincoln Southwest16.00014.North Platte15.00015.Lincoln North Star14.00016.Lincoln Pius X12.00017.Lincoln Southeast12.00018.South Sioux City12.00019.Norfolk12.00020.Elkhorn South10.00021.Omaha Burke9.00022.Omaha Central8.00023.Omaha Westside7.00024.Lincoln Northeast6.00025.Omaha Benson3.00026.Hastings2.00027.Omaha Northwest1.00028.Omaha Bryan1.000High JumpClass A State Record: 7-04.00 Randal Carter, Papillion-La Vista 2007Class A Meet Record: 7-04.00 Randal Carter, Papillion-La Vista 2007All-Class State Record: 7-04.00 Randal Carter, Papillion-La Vista 2007All-Class Meet Record: 7-04.00 Randal Carter, Papillion-La Vista 2007==========================================================================================     Name                      Grade School                                   Final H#  Pts ==========================================================================================   1 Payton Nelson                11 Omaha North                            6-09.00    10   2 Seth Houfek                  12 Millard North                          6-07.00     8   3 Chandler Petersen            11 Millard West                           6-06.00     6   4 Alonzo Mercier               11 Papillion-La Vista South               6-05.00     5   5 Kendall West                 12 Creighton Preparatory School           6-04.00     4  6T Eric Elder                   12 Kearney                                6-02.00     2  6T Jalen Allison                12 Papillion-La Vista                     6-02.00     2  6T Robert Sullivan              12 Omaha Burke                            6-02.00     2   9 David Jones                  11 Omaha North                            6-02.00   10 Matthew Stankoski            12 Columbus                               6-02.00  11T Amiah Jackson                10 Lincoln High                           6-00.00  11T Sam Morris                   10 Lincoln Southeast                      6-00.00   13 Kade Bowling                 11 Lincoln Southeast                      6-00.00   14 Grant Jackman                12 Lincoln East                           5-10.00  15T Nicholas Crenshaw            12 Omaha Burke                            5-10.00  15T Juquan Cribbs                11 Omaha Benson                           5-10.00    - Marcel Ryan                  12 Papillion"
str_extract(t3, "(High Jump|Triple Jump|Pole Vault|Long Jump|Discus) *Class [A-D] State Record.+")
str_extract(t3, "(High Jump|Triple Jump|Pole Vault|Long Jump|Discus) *Class [A-D] State Record.+\n*")
