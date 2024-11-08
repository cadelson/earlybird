# PURPOSE: Visualization showing the average number of animals screened by county
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Oct 4, 2022
# NOTES:

library(tidyverse)

month_order <- c("April", "May", "June", "July", "August", "September")

df_screened <- df_21_22 %>% 
  filter(indicator == "animals_screened",
         month %in% c("April", "May", "June", "July", "August")) %>%
  group_by(state, county, year, month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE, n = n())) 

df_fos <-
tibble::tribble(
                    ~county,   ~month, ~total_fos, ~year,
         "Awerial/Terekeka",  "April",        25L, 2022L,
         "Awerial/Terekeka",    "May",        25L, 2022L,
         "Awerial/Terekeka",   "June",        25L, 2022L,
         "Awerial/Terekeka",   "July",        25L, 2022L,
         "Awerial/Terekeka", "August",        25L, 2022L,
             "Gogrial West",  "April",         2L, 2022L,
             "Gogrial West",    "May",         2L, 2022L,
             "Gogrial West",   "June",         2L, 2022L,
             "Gogrial West",   "July",         2L, 2022L,
             "Gogrial West", "August",         2L, 2022L,
                "Jur River",  "April",        20L, 2022L,
                "Jur River",    "May",        20L, 2022L,
                "Jur River",   "June",        19L, 2022L,
                "Jur River",   "July",        19L, 2022L,
                "Jur River", "August",        19L, 2022L,
                   "Nyirol",  "April",         9L, 2022L,
                   "Nyirol",    "May",         9L, 2022L,
                   "Nyirol",   "June",         9L, 2022L,
                   "Nyirol",   "July",         9L, 2022L,
                   "Nyirol", "August",         9L, 2022L,
            "Rumbek Centre",  "April",        13L, 2022L,
            "Rumbek Centre",    "May",        13L, 2022L,
            "Rumbek Centre",   "June",        13L, 2022L,
            "Rumbek Centre",   "July",        13L, 2022L,
            "Rumbek Centre", "August",        13L, 2022L,
             "Rumbek North",  "April",        25L, 2022L,
             "Rumbek North",    "May",        25L, 2022L,
             "Rumbek North",   "June",        24L, 2022L,
             "Rumbek North",   "July",        25L, 2022L,
             "Rumbek North", "August",        24L, 2022L,
                "Tonj East",  "April",        40L, 2022L,
                "Tonj East",    "May",        39L, 2022L,
                "Tonj East",   "June",        40L, 2022L,
                "Tonj East",   "July",        40L, 2022L,
                "Tonj East", "August",        40L, 2022L,
               "Tonj North",  "April",         6L, 2022L,
               "Tonj North",    "May",         6L, 2022L,
               "Tonj North",   "June",         6L, 2022L,
               "Tonj North",   "July",         6L, 2022L,
               "Tonj North", "August",         6L, 2022L,
               "Tonj South",  "April",         1L, 2022L,
               "Tonj South",    "May",         1L, 2022L,
               "Tonj South",   "June",         1L, 2022L,
               "Tonj South",   "July",         1L, 2022L,
               "Tonj South", "August",         1L, 2022L,
  "Lopa Lafon/Ikotos/Torit",  "April",        10L, 2022L,
  "Lopa Lafon/Ikotos/Torit",    "May",        10L, 2022L,
  "Lopa Lafon/Ikotos/Torit",   "June",        10L, 2022L,
  "Lopa Lafon/Ikotos/Torit",   "July",        10L, 2022L,
  "Lopa Lafon/Ikotos/Torit", "August",        10L, 2022L,
               "Uror/Akobo",  "April",        22L, 2022L,
               "Uror/Akobo",    "May",        33L, 2022L,
               "Uror/Akobo",   "June",        33L, 2022L,
               "Uror/Akobo",   "July",        33L, 2022L,
               "Uror/Akobo", "August",        32L, 2022L
  )

df_fo <- tibble::tribble(
                       ~county,      ~month, ~total_fos, ~year,
                "Lakes (East)",     "April",        25L, 2022L,
                "Lakes (East)",       "May",        25L, 2022L,
                "Lakes (East)",      "June",        25L, 2022L,
                "Lakes (East)",      "July",        25L, 2022L,
                "Lakes (East)",    "August",        25L, 2022L,
                "Lakes (East)", "September",        25L, 2022L,
                "Lakes (East)",   "October",        25L, 2022L,
                "Lakes (West)",     "April",        38L, 2022L,
                "Lakes (West)",       "May",        38L, 2022L,
                "Lakes (West)",      "June",        38L, 2022L,
                "Lakes (West)",      "July",        38L, 2022L,
                "Lakes (West)",    "August",        38L, 2022L,
                "Lakes (West)", "September",        30L, 2022L,
                "Lakes (West)",   "October",        30L, 2022L,
                      "Warrap",     "April",        44L, 2022L,
                      "Warrap",       "May",        44L, 2022L,
                      "Warrap",      "June",        44L, 2022L,
                      "Warrap",      "July",        44L, 2022L,
                      "Warrap",    "August",        44L, 2022L,
                      "Warrap", "September",        37L, 2022L,
                      "Warrap",   "October",        37L, 2022L,
           "Eastern Equatoria",     "April",        10L, 2022L,
           "Eastern Equatoria",       "May",        10L, 2022L,
           "Eastern Equatoria",      "June",        10L, 2022L,
           "Eastern Equatoria",      "July",        10L, 2022L,
           "Eastern Equatoria",    "August",        10L, 2022L,
           "Eastern Equatoria", "September",        10L, 2022L,
           "Eastern Equatoria",   "October",        19L, 2022L,
                     "Jonglei",     "April",        33L, 2022L,
                     "Jonglei",       "May",        44L, 2022L,
                     "Jonglei",      "June",        44L, 2022L,
                     "Jonglei",      "July",        44L, 2022L,
                     "Jonglei",    "August",        51L, 2022L,
                     "Jonglei", "September",        51L, 2022L,
                     "Jonglei",   "October",        51L, 2022L
           )




df_animals_screened <- df_screened %>% 
  mutate(county = case_when(
    county %in% c("Awerial", "Terekeka") ~ "Awerial/Terekeka",
    county %in% c("Torit", "Lopa/Lafon", "Ikotos") ~ "Lopa Lafon/Ikotos/Torit",
    county %in% c("Akobo", "Uror") ~ "Uror/Akobo",
    TRUE ~ county)) %>% 
  left_join(df_fos) %>% 
  filter(county %notin% c("Wau", "Akobo", "Tonj South")) %>% 
  mutate(average_screened = value / total_fos,
         month = fct_relevel(month, month_order)) %>% 
  group_by(county) %>% 
  mutate(average_total = sum(average_screened)/5,
         value_total = sum(value)/5,
         # average_total = case_when(
         #   month == "April" ~ round(average_total, 0),
         #   TRUE ~ NA_real_),
         county_value_label = paste(county, " (n=", comma(round(value_total, 0), accuracy = 1), ")", sep=""),
         county_label = paste(county, " (n=", comma(round(average_total, 0), accuracy = 1), ")", sep="")) %>% 
         #county_label = reorder(county_label, average_total, sum)) %>% 
  ungroup() %>% 
  mutate(county_label = reorder(county_label, -average_total, sum))
  #arrange(average_total)

df_animals_screened_total <- df_animals_screened %>% 
  mutate(county_value_label = reorder(county_value_label, -value_total, sum))

# Total screened
df_animals_screened_total %>% 
  ggplot(aes(month, value)) +
  geom_bar(stat = "identity", fill = burnt_sienna) +
  #geom_text(aes(label=average_total, hjust=-.05, family = "Source Sans Pro SemiBold"), size=3)+
  facet_wrap(~county_value_label) +
  si_style_ygrid()+
  labs(x=NULL, y=NULL,
       caption= paste("Collection of animal screening data began April 2022"))
  # theme(panel.background = element_rect(fill = "beige", color = "beige"),
  #       plot.background = element_rect(fill = "beige"))

si_save("Images/total_animals_screened.png")

# Average per field officer
df_animals_screened %>% 
  ggplot(aes(month, average_screened)) +
  geom_bar(stat = "identity", fill = burnt_sienna) +
  #geom_text(aes(label=average_total, hjust=-.05, family = "Source Sans Pro SemiBold"), size=3)+
  facet_wrap(~county_label) +
  si_style_ygrid()+
  labs(x=NULL, y=NULL,
       caption= paste("Collection of animal screening data began April 2022"))+
  theme(panel.background = element_rect(fill = "beige", color = "beige"),
        plot.background = element_rect(fill = "beige"))
  #theme(strip.text.x = element_text(size = 30))

si_save("Images/average_animals_screened_fo.png")
  