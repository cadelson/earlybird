# PURPOSE: Generate visualization differentiating rumours by month by species
# AUTHOR: Cody Adelson | Data Consultant
# LICENSE: MIT
# DATE: Nov 7, 2024
# NOTES: 

library(tidyverse)
library(scales)
library(glitr)
library(readxl)
library(janitor)

df_21_24 <- read.csv("~/Github/dewormr/Dataout/gwsd_21_24.txt")
df_rumours_species_23 <- read_excel("~/Github/dewormr/Data/2023_raw_data/12. Databases (December 2023)/animal_rumours_december_23_v1.xlsx") %>% mutate(year = "2023")
df_rumours_species_22 <- read_excel("~/Github/dewormr/Data/2022_raw_data/12. Databases (December 2022)/animal_rumours_december_22_v1.xlsx") %>% mutate(year = "2022")

month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 

df_22_23 <- df_rumours_species_23 %>% 
  bind_rows(df_rumours_species_22) %>% 
  clean_names() %>% 
  select(year, contains("reported"), contains("animal_types")) %>% 
  mutate(across(contains("rumors_reported"), as.character)) %>% 
  pivot_longer(cols = starts_with("no_rumors_reported") | starts_with("animal_types"), names_to = c(".value", "month"), names_pattern = "(.*)_(\\d+)" ) %>%
  mutate(
    animal_types = tolower(animal_types),
    animal_types = str_replace_all(animal_types, "dogs", "dog"),
    animal_types = str_replace_all(animal_types, "cats", "cat"),
    animal_types = str_remove_all(animal_types, ","),
    animal_types = str_remove_all(animal_types, " "),
    animal_types = case_when(
      animal_types == "dog" ~ paste0(no_rumors_reported, animal_types),
      TRUE ~ animal_types)) %>% 
  filter(!is.na(animal_types)) %>% 
  mutate(
         dog = str_extract(animal_types, "(?<=^|\\D)(\\d+)(?=dog)"), 
         cat = str_extract(animal_types, "(?<=^|\\D)(\\d+)(?=cat)"),
         wild_animal = str_extract(animal_types, "(?<=^|\\D)(\\d+)(?=fish|bull|antelopes|goat|dikdik|gazel)"),
         across(c(dog, cat, wild_animal), as.numeric),
         dog = case_when(
           year == "2023" & month == "9" & dog == "1" ~ as.numeric(no_rumors_reported),
           year == "2023" & month == "12" & dog == "148" ~ 158,
           year == "2022" & month == "2" & dog == "41" ~ 44,
           TRUE ~ dog),
         wild_animal = case_when(
           year == "2022" & month =="10" & wild_animal == "3" ~ 5,
           TRUE ~ wild_animal),
         month=month.abb[as.numeric(month)]) %>% 
  # rowwise() %>%
  # mutate(total_check = sum(dog, cat, wild_animal, na.rm = TRUE)) %>%
  # filter(total_check!=no_rumors_reported) %>%
  # View()
  select(year, month, dog, cat, wild_animal) %>% 
  pivot_longer(cols = c(dog, cat, wild_animal), names_to = c("host"), values_to = c("value")) %>%
  summarise(value = sum(value, na.rm = TRUE), .by = c(month, host, year)) %>% 
  mutate(host = str_replace(host, "_", " "),
         host = str_to_title(host))

df_rumours_species <- df_21_24 %>% 
  filter(!is.na(host),
         indicator == "rumours_total",
         #host == "Wild Animal"
         ) %>% 
  select(year, month, host, value) %>% 
  summarise(value = sum(value, na.rm = TRUE), .by = c(month, host)) %>% 
  mutate(year = "2024",
         month=match(month, month.name),
         month=month.abb[month]) %>% 
  bind_rows(df_22_23) %>% 
  mutate(
    month=fct_relevel(month, month_order),
    host = reorder(host, -value),
    endpoints = case_when(
      month %in% c("Jan", "Dec") ~ value,
      year == "2024" & month == "Nov" ~ value,
      TRUE ~ NA_real_),) 



df_rumours_species %>% 
  ggplot(aes(month, value, group = year))+
  #geom_line()
  geom_area(color = scooter, fill = scooter_light, alpha = .4, na.rm = TRUE)+
  geom_point(aes(y = endpoints), na.rm = TRUE, color = scooter) +
  #geom_area(data=subset(df_rumours, type=="human"), color=genoa, fill=genoa_light, alpha = .4, na.rm=TRUE)+
  #geom_vline(xintercept = c(5.5, 11.5), color="white")+
  #geom_rug(aes(color = factor(cases_color)), size=3, sides="b", na.rm = TRUE) +
  facet_grid(host~year, 
             scales = "free_y", 
             switch = "y", 
             labeller = labeller(type = function(x) {rep("", length(x))}))+
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))+
  scale_y_continuous(labels=comma)+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(panel.spacing.x = unit(.5, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        strip.text.x = element_text(size = 16),
        legend.position="none",
        strip.text.y.left = element_text(angle = 0, size = 16, margin = margin(t = 0, r = 0, b = 0, l = 0)),
        strip.placement = "outside"
        )+
  scale_color_identity()

si_save("Images/2024_arm/rumours_monthly_species", width = 12)

df_rumours_species %>% 
  summarise(value = sum(value), .by = c(year, host)) %>% 
  arrange(host, year) %>% 
  group_by(host) %>% 
  mutate(change = percent((value-lag(value))/lag(value)))
