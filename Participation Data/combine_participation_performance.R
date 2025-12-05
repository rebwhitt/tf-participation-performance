library(tidyverse)
library(huxtable)
library(plm)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Participation\ Data\\")
participation <- read.csv("participation_clean.csv")
nces <- read.csv("nces_clean.csv")
census <- read.csv("census_2020_clean.csv")

setwd("..\\Historical\ Performance\ Data\\")
performance <- read.csv("performance_clean.csv") |> 
  select(Year, State, Gender, Event, Mark, Mark.Note)

# Checking for NAs
participation %>% 
  filter(Year >= 1979, is.na(Participation))

participation %>% 
  filter(Year >= 1979) %>% 
  count(State, Gender) %>% 
  filter(n<41)

participation %>% 
  filter(Year >=1979, State=="Maryland", Gender=="Girls")

performance %>% 
  count(State, Event)

# clean participation more
participation <- participation %>% 
  mutate(State = case_when(
    State == "Florida\n" ~ "Florida",
    State == "Wisconsin -" ~ "Wisconsin",
    State == "W. Virginia" ~ "West Virginia",
    TRUE ~ State
  ))

# Select only best winning performance per state-year-event-gender
performance_top <- performance |> 
  group_by(State, Year, Gender, Event) |> 
  summarize(Mark = ifelse(Event %in% c("100m", "800m","1600m"),
                          min(Mark, na.rm=T),
                          max(Mark, na.rm=T))) |> 
  filter(Mark != -Inf, Mark != Inf) %>% 
  distinct() %>% 
  ungroup()

performance_top %>% 
  count(Year, Event, State) %>% 
  filter(Event=="High Jump", Year==2019, n<=1) %>% 
  print(n=300)

data.all <- participation |> 
  right_join(performance, by=c("State","Year","Gender")) %>% 
  left_join(nces, by=c("State", "Year")) %>% 
  left_join(census, by=c("State", "Gender"), relationship = "many-to-many") #%>% 
  #filter(Year >= 1979)

data <- participation |> 
  right_join(performance_top, by=c("State","Year","Gender")) |> 
  left_join(nces, by=c("State", "Year")) %>% 
  left_join(census, by=c("State", "Gender"), relationship = "many-to-many")
  #filter(Year >= 1979) 
  
data <- participation %>% 
  right_join(performance_top, by=c("State","Year","Gender")) |> 
  filter(State != "Ohio") %>% 
  filter(Year >= 1979, Year <= 2019) %>% 
  drop_na(Event) %>% 
  mutate(
    Participation.Thousands = (Participation/1000),
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault"))
  ) %>% 
  group_by(Event) %>% 
  mutate(Mark.Standard = scale(Mark)
  ) %>% 
  ungroup() %>% 
  mutate(Mark.Standard = ifelse(Event %in% c("100m","800m", "1600m"),
                                Mark.Standard * -1,
                                Mark.Standard)) %>% 
  #left_join(gender, by="State") %>% 
  mutate(
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault"))
  )

  
data.wide <- data  %>% 
  select(-Participation.Thousands, -Mark.Standard) %>% 
  pivot_wider(names_from = Gender, names_sep=".", values_from = c(Schools, Participation, Mark)) %>% 
  mutate(Participation.Pct.Diff = (Participation.Boys - Participation.Girls)/Participation.Boys *100,
         Mark.Pct.Diff = (Mark.Boys - Mark.Girls)/((Mark.Boys+Mark.Girls)/2) *100,
        # Prop.Boys = Count.Boys/(Count.Boys+Count.Girls),
        # Prop.Girls = Count.Girls/(Count.Boys+Count.Girls),
         Participation.Boys.Thousands = (Participation.Boys/1000),
         Participation.Girls.Thousands = (Participation.Girls/1000)
  ) %>% 
  drop_na(Event) %>% 
 # filter(State != "Ohio") %>% 
#  filter(Year >= 1979, Year <= 2019) %>% 
  #filter(Participation.Girls > 0, Participation.Boys > 0) %>% 
  mutate(
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault")),
    Boys.to.girls.ratio = Participation.Boys / Participation.Girls) ## Nesburg et al. style



data.analysis <- data.wide %>% 
  mutate(Participation.Pct.Diff = (Participation.Boys - Participation.Girls)/Participation.Boys *100,
         Mark.Pct.Diff = (Mark.Boys - Mark.Girls)/((Mark.Boys+Mark.Girls)/2) *100,
         #Prop.Boys = Count.Boys/(Count.Boys+Count.Girls),
         #Prop.Girls = Count.Girls/(Count.Boys+Count.Girls),
         Participation.Boys.Thousands = (Participation.Boys/1000),
         Participation.Girls.Thousands = (Participation.Girls/1000)
         ) %>% 
  drop_na(Event) %>% 
  filter(State != "Ohio") %>% 
  filter(Year >= 1979, Year <= 2019) %>% 
  #filter(Participation.Girls > 0, Participation.Boys > 0) %>% 
  mutate(
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault")),
    Boys.to.girls.ratio = Participation.Boys / Participation.Girls, ## Nesburg et al. style
  ) 
  
data.analysis.all <- data.all %>% 
  mutate(Participation.Thousands = (Participation/1000))


  #  Students = ifelse(Students < 20000,
  #                    NA,
   #                   Students),
   # Male.Students=Students * Prop.M,
   # Female.Students=Students * Prop.F,
   # Participation.by.Schools = Participation / Schools,
   # Participation.Rate = ifelse(Gender=="Boys",
                       #         (Participation / Male.Students)*100,
                       #         (Participation / Female.Students)*100)
  

data.analysis.2 <- participation %>%  
  right_join(performance_top, by=c("State","Year","Gender")) |> 
  # left_join(nces, by=c("State", "Year")) %>% 
  #  left_join(census, by=c("State", "Gender"), relationship = "many-to-many") %>% 
  filter(State != "Ohio") %>% 
  filter(Year >= 1979, Year <= 2019) %>% 
  #filter(Participation > 0) %>% 
  #filter(!(State=="Kansas"&Year<=1981)) %>% 
  drop_na(Event) %>% 
  mutate(
    Participation.Thousands = (Participation/1000),
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault"))
  ) %>% 
  group_by(Event) %>% 
  mutate(Mark.Standard = scale(Mark)
  ) %>% 
  ungroup() %>% 
  mutate(Mark.Standard = ifelse(Event %in% c("100m","800m", "1600m"),
                                Mark.Standard * -1,
                                Mark.Standard)) %>% 
  #left_join(gender, by="State") %>% 
  mutate(
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault"))
  )

data.analysis.all <- data.analysis.all %>% 
  mutate(Participation.Thousands = Participation / 1000)

data.analysis <- data.analysis %>% 
  filter(Participation.Girls > 0, Participation.Boys > 0) %>% 
  filter(!(State == "Tennessee" & Year == 2008)) %>% 
  mutate(
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault")))
data.analysis.2 <- data.analysis.2 %>% 
  filter(Participation > 0) %>% 
  filter(!(State == "Tennessee" & Year == 2008 & Gender=="Girls")) %>% 
  mutate(
    Event = factor(Event, 
                   levels=c("100m","800m","1600m","High Jump", "Pole Vault")))

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Participation\ Data\\")
save(data.analysis, file="data_analysis.rda")
write.csv(data.analysis, "data_analysis.csv", row.names = F)
write.csv(data.analysis.2, "data_analysis_2.csv", row.names = F)
write.csv(data.analysis.all, "data_analysis_all.csv", row.names = F)

write.csv(data, "data.csv", row.names = F)
write.csv(data.wide, "data_wide.csv", row.names = F)

