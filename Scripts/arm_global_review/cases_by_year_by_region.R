# PURPOSE: Generate visualization for number of cases by month
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Nov 30, 2021
# NOTES: 2022 ARM updated with patient dataset

df_2022_cases <- df_patient_data %>% 
  filter(county %in% c("Lopa/Lafon", "Tonj East", "Awerial", "Nyirol",
                      "Jur River", "Tonj North", "Torit", "Uror", "Rumbek North",
                      "Tonj South", "Rumbek Centre", "Yirol East"),
         year > 2011) %>%
  mutate("county" = case_when(county == "Tonj South" & year == 2020 ~ "Tonj East",
                              TRUE ~ county),
         "county" = trimws(county)) %>% 
  filter(county != "Tonj South") %>% 
  distinct(patient_name, county, year) %>% 
  group_by(county, year) %>% 
  count(name = "value") %>% 
  group_by(county) %>% 
  mutate("county_total" = glue("n=", sum(value))) %>% 
  ungroup() %>% 
  mutate(county_label = paste({county}, {county_total}, sep="\n"))

county_order<- c("Tonj East\nn=7", "Awerial\nn=32", "Lopa/Lafon\nn=1", "Rumbek North\nn=4", 
                 "Uror\nn=2", "Jur River\nn=9", "Torit\nn=1", "Tonj North\nn=7", 
                 "Nyirol\nn=2", "Rumbek Centre\nn=4", "Yirol East\nn=1") 


df_2022_cases %>% 
  mutate("county_label" = fct_relevel(county_label, county_order)) %>% 
  ggplot(aes(x=year, y=value))+
  geom_col(fill=moody_blue, width=.6)+
  #facet_wrap(~county_label, ncol=1)+
  facet_grid(county_label ~., switch = "y") +
  scale_y_continuous(breaks=seq(0, 20, 5), limits = c(0,12.5))+
  scale_x_continuous(breaks=seq(2012, 2022, 1), position = "top")+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 44, family = "Source Sans Pro"),
        strip.text = element_text(size = 38, hjust= 1, family = "Source Sans Pro"),
        axis.text.y  = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y.left = element_text(angle = 0))+
  geom_hline(aes(yintercept=1), colour="white", size= 1)+
  geom_hline(aes(yintercept=2), colour="white", size= 1)+
  geom_hline(aes(yintercept=3), colour="white", size= 1)+
  geom_hline(aes(yintercept=4), colour="white", size= 1)+
  geom_hline(aes(yintercept=5), colour="white", size= 1)+
  geom_hline(aes(yintercept=6), colour="white", size= 1)+
  geom_hline(aes(yintercept=7), colour="white", size= 1)+
  geom_hline(aes(yintercept=8), colour="white", size= 1)+
  geom_hline(aes(yintercept=9), colour="white", size= 1)+
  geom_hline(aes(yintercept=10), colour="white", size= 1)+
  geom_hline(aes(yintercept=11), colour="white", size= 1)+
  geom_text(aes(label = value), family = "Source Sans Pro", size = 12, vjust = -.2, fontface = "italic") 
  #geom_rect(aes(xmin = 2021.5, xmax = 2022.5, ymin = -Inf, ymax = Inf), alpha = 0.03, fill=trolley_grey_light, inherit.aes = TRUE)

#si_save("Images/2022_arm/yearly_cases_1p3")

ggsave("Images/2022_arm/yearly_cases_1p.png",
       height = 24,
       width = 29)
