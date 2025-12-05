library(tidyverse)
library(readxl)
library(janitor)
library(SAScii)
library(tidyplus)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Participation\ Data\\")

rawdata <- read_excel("ELSI_excel_export_638628856143667004529.xlsx")

list.files(pattern = "ICPSR_")

raw.ccds <- lapply(lapply(list.files(pattern = "ICPSR_"), function(f){
  c(paste(f,"\\DS0001\\",str_extract(f, "[0-9]+"),"-0001-Data.txt", sep=""),
    paste(f,"\\DS0001\\",str_extract(f, "[0-9]+"),"-0001-Setup.sas", sep=""))
}), function(files){
  read.SAScii(files[[1]], files[[2]])
}) %>% 
  bind_rows(.id = "df")

ccds <- raw.ccds %>% 
  select(df, FIPST,
         matches("^ST[0-9][0-9]"), starts_with("NAME"), starts_with("SCHNAM"),
         starts_with("GSLO"), starts_with("GSHI"), starts_with("MEMBER")) %>% 
  mutate_all(as.character) %>% 
  mutate(Name = coalesce(NAME, 
                         coalesce(NAME85,
                                  coalesce(SCHNAM78, coalesce(
                                    SCHNAM80, coalesce(
                                      SCHNAM82, SCHNAM84
                                    )
                                  ))))) %>% 
  pivot_longer(cols=c(starts_with("MEMBER"), starts_with("GS"), starts_with("ST")),
               names_to="Category",
               values_to="Count") %>% 
  drop_na(Count) %>% 
  mutate(CatName = str_extract(Category, "^[A-Z]+"),
         Year = str_extract(Category, "[0-9][0-9]")) %>% 
  select(-Category) %>% 
  pivot_wider(id_cols=c(Name, Year), 
              names_from = "CatName",
              values_from = "Count")

ccds <- raw.ccds %>% 
  select(df, FIPST,
         matches("^ST[0-9][0-9]"), starts_with("NAME"), starts_with("SCHNAM"),
         starts_with("GSLO"), starts_with("GSHI"), starts_with("MEMBER")) %>% 
  coalesce_data(., list(Name = c("SCHNAM78", "SCHNAM80", "SCHNAM82", "SCHNAM84",
                                 "NAME", "NAME85"),
                        GSLO = c("GSLO85", "GSLO78", "GSLO80", "GSLO84",
                                 "GSLO82", "GSLO"),
                        GSHI = c("GSHI78", "GSHI80", "GSHI81", "GSHI82",
                                "GSHI84", "GSHI85"),
                        MEMBER = c("MEMBER78", "MEMBER80", "MEMBER82",
                                   "MEMBER84"))) %>% 
  pivot_longer(cols=c(starts_with("ST")),
               names_to=c("Year"),
               names_pattern="([0-9][0-9])",
               values_to="State") %>% 
  drop_na(State) %>% 
  select(-FIPST, -df) %>% 
  mutate(Year = as.numeric(Year)+1901)

ccd <- ccds %>% 
  filter(GSLO %in% c("09", "10", "11", "12")|
         GSHI %in% c("09", "10", "11", "12")) %>% 
  mutate(lo = as.numeric(gsub("PK","-1",gsub("UC","12",gsub("KG", "0", GSLO)))),
         hi =as.numeric(gsub("UC","12",gsub("KG", "0", GSHI))),
         ratio = (hi-8)/(hi-lo+1),
         ratio = ifelse(ratio > 1, 1, ratio), # ratio: proportion of school years that are within grades 9-12
         count = floor(as.numeric(MEMBER) * ratio)
         ) %>% 
  drop_na(count) %>% 
  group_by(State, Year) %>% 
  summarize(Students = sum(count)) %>% 
  ungroup() %>% 
  left_join(data.frame(State = state.abb, State.Name =state.name), by="State") %>% 
  select(Year, Students,State.Name) %>% 
  rename(State=State.Name)

cleandata <- rawdata %>% 
  drop_na(1) %>% 
  filter(!str_detect(`ELSI Export`, "(National|This|†|–|‡)")) %>% 
  row_to_names(1) %>% 
  pivot_longer(cols=starts_with("Grade")) %>% 
  separate_wider_regex(name, c("Grade ",
                               Grade = "[9120]+",
                               " Students \\[State\\] ",
                               Year = "[12][90][0-9][0-9]\\-[0-9][0-9]", 
                               ".*")) %>% 
  rename(State = `State Name`) %>% 
  group_by(Year, State) %>% 
  summarize(Students = sum(as.numeric(value))) %>% 
  mutate(State = str_to_title(State),
         Year = as.numeric(paste(substr(Year,1,2), substr(Year,6,7), sep="")))
  
comb_data <- bind_rows(cleandata, ccd)
write.csv(comb_data, "nces_clean.csv", row.names=F)

colnames(cleandata)

# Data Source: U.S. Department of Education National Center for Education Statistics Common Core of Data (CCD) "State Nonfiscal Public Elementary/Secondary Education Survey" 2010-11 v.1a  2011-12 v.1a  2012-13 v.1a  2013-14 v.1a  2016-17 v.1a  2017-18 v.1a  2018-19 v.1a  2022-23 v.1a; "State Nonfiscal Public Elementary/Secondary Education Survey Membership Data" 2014-15 v.1a  2015-16 v.1a.
gender <- read_excel("gender_enrollment.xlsx") %>% 
  drop_na(1) %>% 
  filter(!str_detect(`ELSI Export`, "(National|This|†|–|‡)")) %>% 
  row_to_names(1) %>% 
  #pivot_longer(cols=starts_with("(Male"), names_to = "Male.Students") %>% 
  pivot_longer(cols=contains("Students"), values_to = "Students") %>% 
  separate_wider_regex(name, c(Gender="(?:Male|Female)",
                               " Students \\[State\\] ",
                               Year = "[12][90][0-9][0-9]\\-[0-9][0-9]", 
                               ".*")) %>% 
  rename(State = `State Name`) %>% 
  pivot_wider(names_from = c("Gender"), values_from=c("Students")) %>%   
 # group_by(Year, State) %>% 
 # summarize(Students = sum(as.numeric(Students))) %>% 
  mutate(State = str_to_title(State),
         Year = as.numeric(paste(substr(Year,1,2), substr(Year,6,7), sep="")),
         Male = as.numeric(Male),
         Female = as.numeric(Female),
         Prop.F = Female / (Female + Male) ,
         Prop.M = Male / (Male +Female) ) %>% 
  group_by(State) %>% 
  summarize(Prop.F = mean(Prop.F, na.rm=T),
            Prop.M = mean(Prop.M, na.rm=T))
write.csv(gender, "gender_enrollment.csv", row.names=F)

# ACS 2021 High school
8929/(8929+8365) # Male
8365/(8929+8365) # Female

library()
