library(tidyverse)
library(gt)
library(readxl)
library(lubridate)
library(reprex)
library(skimr)
library(shiny)
library(shinydashboard)


Bio <- read_xlsx("~/onedrive/Athlete Info.xlsx")
Sprint <- read_xlsx("~/onedrive/BPC Clients/Athlete Sprinting.xlsx")
Jump <- read_xlsx("~/onedrive/BPC Clients/Athlete Sprinting.xlsx",
                  sheet = "Jump")

j1 <- left_join(Bio,Sprint, by = c("Name" = "Athlete"))


df2 <- full_join(j1,Jump, by = c("Name" = "Athlete", "Date"))


sprint_long <- Sprint %>%
  mutate(Date = as.character(Date)) %>%
  pivot_longer(Rep1:Rep10, names_to = "Rep", values_to = "Time" )


sprint_long %>%
  group_by(Athlete, Run_In, Fly_D) %>%
  arrange(Time)



##2023
Bio <- read_xlsx("~/onedrive/Athlete Info.xlsx")
Sprint <- read_xlsx("~/onedrive/BPC Clients/Athlete Sprinting.xlsx")
bio_sprint <- left_join(Bio,Sprint, by = c("Name" = "Athlete"))
Jump <- read_xlsx("~/onedrive/BPC Clients/Athlete Sprinting.xlsx",
                  sheet = "Jump")
bio_slj <- left_join(Bio,Jump, by = c("Name" = "Athlete"))
CMJ <- read_csv("~/onedrive/Athlete Data/cmj.csv") %>% clean_names()
bio_cmj<- left_join(Bio,CMJ, by = c("Name" = "name"))
Drop_Jump <- read_csv("~/onedrive/Athlete Data/drop_jump.csv") %>% clean_names()
bio_drop_jump <- left_join(Bio,Drop_Jump, by = c("Name" = "name"))



Sprint %>%
  filter(!is.na(Top_mph), Run_In == 25) %>%
  max(Sprint$Top_mph)
  





