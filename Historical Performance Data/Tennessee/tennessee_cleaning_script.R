library(tidyverse)
library(tabulapdf)
library(rvest)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Tennessee")

urls <- c("?sport=track-girls&type=100%20meter%20dash%20champion",
          "?sport=track-girls&type=100%20yard%20dash%20champion",
          "?sport=track-girls&type=800%20meter%20run%20champion",
          "?sport=track-girls&type=880%20yard%20run%20champion",
          "?sport=track-girls&type=1600%20meter%20run%20champion",
          "?sport=track-girls&type=mile%20run%20champion",
          "?sport=track-girls&type=high%20jump%20champion",
          "?sport=track-girls&type=pole%20vault%20champion",
          "?sport=track-boys&type=100%20meter%20dash%20champion",
          "?sport=track-boys&type=100%20yard%20dash%20champion",
          "?sport=track-boys&type=800%20meter%20run%20champion",
          "?sport=track-boys&type=880%20yard%20run%20champion",
          "?sport=track-boys&type=1600%20meter%20run%20champion",
          "?sport=track-boys&type=mile%20run%20champion",
          "?sport=track-boys&type=high%20jump%20champion",
          "?sport=track-boys&type=pole%20vault%20champion")
          

rawdata <- lapply(urls, function(url){
  html_table(read_html(paste("https://tssaasports.com/history/results/champions/", url, sep="")))[[1]] |> 
    mutate_all(as.character)
}) |> 
  bind_rows(.id="Gender") 

cleandata <- rawdata |> 
  mutate(Event = case_when(
    !is.na(`100 Meter Dash Champion`) ~ "100m",
    !is.na(`100 Yard Dash Champion`) ~ "100y",
    !is.na(`800 Meter Run Champion`) ~ "800m",
    !is.na(`880 Yard Run Champion`) ~ "880y",
    !is.na(`1600 Meter Run Champion`) ~ "1600m",
    !is.na(`Mile Run Champion`) ~ "Mile",
    !is.na(`High Jump Champion`) ~ "High Jump",
    !is.na(`Pole Vault Champion`) ~ "Pole Vault",
    TRUE ~ NA
  ),
  State = "Tennessee",
  Gender = ifelse(as.numeric(Gender) <= 8, "Girls", "Boys")) |> 
  rename(Mark = Score) |> 
  select(State, Year, Gender, Event, Class, Mark)

write.csv(cleandata, "tennessee_clean.csv", row.names = F)
