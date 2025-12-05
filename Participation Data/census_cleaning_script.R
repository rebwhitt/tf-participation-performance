# @author Rebecca Whitten

# Cleaning script for 2020 census demographic data
#   - using to impute gender ratios for historical NCES high school enrollment data
# imports all demographic state files from folder, tidies
# into combined file of 2020 populations of 15-19 year-olds by gender and state.  

library(tidyverse)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Participation\ Data\\")

data<-lapply(list.files(pattern="DECENNIALDP.*"), read.csv) %>% 
  rbind(colnames(.),.) %>% 
  bind_rows(.id="df") %>% 
  mutate(Gender = str_extract(Label..Grouping., "(Total population|Male|Female)")) %>% 
  fill(Gender) %>% 
  filter(str_detect(Label..Grouping.,  "15 to 19 years")) %>% 
  #select(df, Gender, everything()) %>% 
  pivot_longer(cols=ends_with("Count"),
               names_to="State",
               names_pattern=("(^[A-Za-z \\.]+)\\.\\."),
               values_to="Count") %>% 
  mutate(State = gsub("\\.", " ", State),
         Gender = case_when(
           Gender == "Male" ~ "Boys",
           Gender == "Female" ~ "Girls",
           TRUE ~ Gender
         ),
         Count = as.numeric(gsub(",","",Count))) %>% 
  select(df, Gender, State, Count) %>% 
  drop_na(Count)

# export CSV file
write.csv(data, "census_2020_clean.csv", row.names=F)

# testing
#list.files(pattern="DECENNIALDP.*")
           