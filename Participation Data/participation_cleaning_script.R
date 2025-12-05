library(tidyverse)
library(readxl)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Participation\ Data\\")

rawdata <- read_excel("participation_statistics.xlsx")

cleandata <- rawdata |> 
  left_join(tibble(state.abb,state.name), c("State"="state.abb")) |> 
  mutate(State = state.name,
         Year = as.numeric(str_extract(Year, "[0-9][0-9][0-9][0-9]$"))) |> 
  select(-state.name, -Sport) |> 
  pivot_longer(cols = c(`Boys School`,
                        `Girls School`),
               names_to= c("Gender", "Schools"),
               names_sep = " ") |> 
  mutate(Participation = ifelse(Gender=="Girls", `Girls Participation`, `Boys Participation`),
         Schools = value) |> 
  select(-`Boys Participation`, -`Girls Participation`, -value)


# Pre-2002  data
names <- list()
for(i in 1978:2002){
  for(j in c("B", "G")){
    names[length(names)+1]<-paste(j,substr(as.character(i),3,4),".xlsx",sep="")
  }
}


n<-read_excel(names[[1]])

colnums <- list(c(8,9), #B78
                c(4,5), #G78
                c(4,5), #B79
                c(2,3), #G79
                c(4,5), #B80
                c(2,3),
                c(4,5),
                c(3,4),
                c(4,5),
                c(2,3),
                c(4,5), # B83
                c(2,3), # 10: G83
                c(4,5), # B84
                c(2,3),
                c(4,5), # B85
                c(2,3), # G85
                c(4,5),
                c(2,3),
                c(4,5),
                c(2,3),
                c(4,5),
                c(2,3), # 20:G88
                c(4,5),
                c(2,3),
                c(4,5), # B90
                c(2,3), #G90
                c(4,5),
                c(2,3),
                c(8,9),
                c(4,5),
                c(8,9),
                c(4,5),
                c(6,7),
                c(6,7),
                c(6,7),
                c(6,7),
                c(4,5),
                c(6,7),
                c(4,5),
                c(6,7),
                c(4,5),
                c(6,7),
                c(4,5),
                c(6,7),
                c(4,5), #B00
                c(6,7), #G00
                c(14,15),
                c(13,14),
                c(4,5),
                c(6,7))
files <- list()
for(i in 1:length(colnums)){
  files[[length(files)+1]]<-read_excel(names[[i]], col_types="text") %>% 
    rename(any_of(c(v="Participants",
                    v2="Schools",
                    v3="...")),
           Schools=colnums[[i]][1],
           Participants = colnums[[i]][2])
}

name.join <- data.frame(df=as.character(1:length(names)),title=unlist(names))
rawpdfs <- lapply(files, function(file){
    data.frame(file) %>%
      mutate(Schools = as.character(Schools),
             Participants = as.character(Participants)) %>% 
      rbind(colnames(.), .) %>% 
    rename(any_of(c(v1 = 1)))
})%>% 
  bind_rows(.id="df") %>%
  select(df, v1, Schools, Participants) %>% 
  left_join(.,name.join, by="df") %>% 
  separate_wider_regex(title, c(Gender = "(?:B|G)",
                                Year = "[0-9][0-9]",
                                ".xlsx")) %>% 
  left_join(data.frame(abb=state.abb, State=state.name), by = join_by("v1"=="abb")) %>% 
  mutate(State= str_to_title(coalesce(State, v1)),
         Gender= ifelse(Gender == "G", "Girls", "Boys"),
         Year = as.numeric(ifelse(as.numeric(Year) < 5,
                       paste("20",Year, sep=""),
                       paste("19",Year, sep=""))),
    ) %>% 
  rename(Participation=Participants) %>% 
  mutate(Participation = gsub("00000.*","",Participation),
         Participation = gsub("99999.*","",Participation),
         Participation = gsub("[.,]","", Participation),
         Schools = gsub("00000.*","",Schools),
         Schools = gsub("99999.*","",Schools),
         Schools = gsub("[.,]","", Schools),
         Participation.Note = str_extract(Participation,"\\*+"),
         Participation = gsub("\\*+","", Participation)) %>% 
  drop_na(State) %>% 
  filter(!str_detect(State, "([Ii]nc.+es|St(ate)?|\\.\\.\\.1|[0-9]+)"))

# ** Includes girls playing on boy's teams and boys playing on girl's teams  

gsub("00000.*","","3.1680000000000001")

cleanpdfs <- rawpdfs %>% 
  mutate(
    Schools = as.numeric(Schools),
    Participation = as.numeric(Participation)
  )%>% 
  select(State, Year, Gender, Schools, Participation)


n<-read_excel(names[[4]])

combined_data <- bind_rows(cleanpdfs, cleandata)
write.csv(combined_data, "participation_clean.csv", row.names = F)
