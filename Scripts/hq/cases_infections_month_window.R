# PURPOSE: Generate visualization for number of animal infections, September 2023 - February 2024 
# AUTHOR: Cody Adelson | Data Consultant
# LICENSE: MIT
# DATE: June 24, 2024
# NOTES: Added case in February 2024 for Angola infection not yet in line list

#load packages
library(tidyverse)
library(glitr)
library(scales)
library(lubridate)
library(stringr)
library(extrafont)

#load data
global_patient_animal_dataset <- readRDS("~/Github/gw/case_and_infection/global_case_data/Dataout/global_patient_animal_dataset.rds")
  

df_animal_infections_month <- global_patient_animal_dataset %>% 
  filter(worm_number == "1",
         host != "Human") %>% 
  mutate(month = month.abb[month_emerge]) %>% 
  summarise(infections = n(), .by = c(year_event, month_emerge, month)) %>% 
  filter(year_event > 2022 & month_emerge > 8 | year_event == "2024" & month_emerge < 3) %>%
  arrange(year_event, month_emerge) %>% 
  mutate(month_year = paste0(month, " `", str_sub(year_event, -2, -1)),
         n = row_number(),
         ### CHANGE THIS WHEN ANGOLA INFECTION DATA IS UPDATED
         infections = case_when(
           month == "Feb" & year_event == "2024" ~ 61,
           TRUE ~ infections
         ))



df_animal_infections_month %>% 
  ggplot(aes(x = reorder(month_year, n), y = infections))+
  geom_area(aes(group = 1), alpha = .5, fill = moody_blue_light)+
  geom_vline(xintercept = seq(from=2, to=5, by = 1), size=1, color="white", linetype="dotted")+
  # geom_vline(xintercept = seq(from=1995, to=2021, by = 5), size=1, color="white", linetype="dotted")+
  # geom_hline(yintercept = seq(from=0, to=100000, by = 25000), size=.5, color="grey90")+
  geom_line(aes(group = 1), size=1.2, colour = moody_blue)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.2, 
             colour = moody_blue,
             stroke=1.2) +
  si_style_ygrid()+
  scale_y_continuous(breaks=seq(0, 125, 25), limits = c(0, 125), expand = c(0, 0))+
  #scale_y_continuous(labels = label_number(suffix="K", scale=1e-3))+
  scale_x_discrete(expand = c(.04, .04))+
  labs(x = NULL, y = NULL, color = NULL)+
  theme(axis.text.y  = element_text(size= 15, family = "Source Sans Pro"),
        axis.text.x  = element_text(size= 15, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        # axis.text.x = element_text(size = 15, family = "Source Sans Pro", margin = margin(t = -.18, unit = "in")),
        # axis.text.y = element_text(size = 15, family = "Source Sans Pro", margin = margin(t = -1, unit = "in")),
        axis.ticks.x = element_blank())

si_save("Images/hq/animal_infections_sep23_feb24.png")


df_animal_infections_month_country <- global_patient_animal_dataset %>% 
  filter(worm_number == "1",
         host != "Human") %>% 
  #mutate(month = month.abb[month_emerge]) %>% 
  summarise(infections = n(), .by = c(year_event, month_emerge, country)) %>% 
  filter(year_event == "2023" & month_emerge > 8 | year_event == "2024" & month_emerge < 3) %>% 
  arrange(year_event, month_emerge) %>% 
  complete(month_emerge, year_event, country, fill = list(infections = 0)) %>% 
  mutate(
    month = month.abb[month_emerge],
    month_year = paste0(month, " `", str_sub(year_event, -2, -1)),
    infections = case_when(
      country == "Angola" & month == "Feb" & year_event == "2024" ~ 1,
      TRUE ~ infections),
    infections_label = case_when(
      infections == 0 ~ NA_real_,
      TRUE ~ infections)) %>% 
  filter(year_event == "2023" & month_emerge > 8 | year_event == "2024" & month_emerge < 3) %>% 
  group_by(country) %>% 
  mutate(country_total_infections = sum(infections),
         country_label = paste0(country, " (n=", country_total_infections, ")")) %>% 
  ungroup() %>% 
  arrange(year_event, month_emerge) %>% 
  mutate(n = row_number())

df_animal_infections_month_country %>% 
  ggplot(aes(x = reorder(month_year, n), y = infections))+
  geom_area(aes(group = 1), alpha = .5, fill = moody_blue_light) +
  geom_vline(xintercept = seq(from=2, to=5, by = 1), size=.5, color="white", linetype="dotted")+
  # geom_hline(yintercept = seq(from=0, to=100000, by = 25000), size=.5, color="grey90")+
  geom_line(aes(group = 1), size=1.2, colour = moody_blue)+
  facet_wrap(~factor(country_label, c("Chad (n=147)", "Cameroon (n=59)", "Mali (n=27)", "Angola (n=2)", "South Sudan (n=1)")),
             scales = "free")+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.2, 
             colour = moody_blue,
             stroke=1.2) +
  geom_text(aes(label = infections_label), family = "Source Sans Pro", size = 3, vjust = -1.2)+
  si_style_ygrid()+
  scale_y_continuous(breaks=seq(0, 100, 25), limits = c(-10, 110), expand = c(0, 0))+
  #scale_y_continuous(labels = label_number(suffix="K", scale=1e-3))+
  scale_x_discrete(expand = c(.04, .04))+
  labs(x = NULL, y = NULL, color = NULL)+
  theme(axis.text.y  = element_text(size= 10, family = "Source Sans Pro"),
        axis.text.x  = element_text(size= 9, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        # axis.text.x = element_text(size = 15, family = "Source Sans Pro", margin = margin(t = -.18, unit = "in")),
        # axis.text.y = element_text(size = 15, family = "Source Sans Pro", margin = margin(t = -1, unit = "in")),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size =14, family = "Source Sans Pro SemiBold"))

si_save("~/Github/earlybird/Images/hq/animal_infections_sep23_feb24_country.png")

