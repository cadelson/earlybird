# PURPOSE: Visualization showing the average number of animals screened by county
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: December 3, 2022
# NOTES:

month_order <- c("April", "May", "June", "July", "August", "September", "October")

df_screened <- df_21_22 %>% 
  filter(indicator == "animals_screened",
         month %in% c("April", "May", "June", "July", "August", "September", "October"),
         county %in% c("Awerial", "Terekeka", "Nyirol", "Rumbek Centre",
                       "Rumbek North", "Tonj East", "Tonj North", "Tonj South",
                       "Lopa/Lafon", "Lafon", "Ikotos", "Torit", "Uror", "Akobo")) %>%
  mutate(county = case_when(
    county %in% c("Awerial", "Terekeka") ~ "Lakes (East)",
    county %in% c("Nyirol", "Uror", "Akobo") ~ "Jonglei",
    county %in% c("Rumbek Centre", "Rumbek North") ~ "Lakes (West)",
    county %in% c("Tonj East", "Tonj North", "Tonj South") ~ "Warrap",
    county %in% c("Lopa/Lafon", "Lafon", "Ikotos", "Torit") ~ "Eastern Equatoria",
    TRUE ~ county)) %>% 
  group_by(county, year, month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE, n = n()))

df_fos <-
tibble::tribble(
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
  left_join(df_fos) %>% 
  mutate(average_screened = value / total_fos,
         month = fct_relevel(month, month_order)) %>% 
  group_by(county) %>% 
  mutate(average_total = sum(average_screened)/7,
         value_total = sum(value),
         county_value_label = paste(county, " (n=", comma(round(value_total, 0), accuracy = 1), ")", sep=""),
         county_label = paste(county, " (n=", comma(round(average_total, 0), accuracy = 1), ")", sep="")) %>% 
  ungroup() %>% 
  mutate(county_label = reorder(county_label, -average_total, sum))

df_animals_screened_total <- df_animals_screened %>% 
  mutate(county_value_label = reorder(county_value_label, -value_total, sum))


# Average per field officer
df_animals_screened %>% 
  filter(county == "Eastern Equatoria") %>% 
  View()
  ggplot(aes(month, average_screened)) +
  geom_bar(stat = "identity", fill = burnt_sienna) +
  geom_text(aes(label=comma(round(average_screened), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  #facet_wrap(~county_label) +
  si_style_ygrid()+
  theme(axis.text.x  = element_text(size = 14, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(x=NULL, y=NULL,
       caption= paste("Collection of animal screening data began April 2022"))+
  scale_y_continuous(breaks = seq(0, 150, 50), labels=comma, limits = c(0, 150))
  #theme(strip.text.x = element_text(size = 30))

si_save("Images/2022_arm/county_presentations/average_animals_screened_fo_ee.png")




  