#Scratch

library(glitr)
library(scales)

#For global review find the change in case median age
south_sudan_human_case_animal_infection %>%
  mutate(year_event = as.numeric(year_event),
         host_age = as.numeric(host_age)) %>% 
  filter(#country == "South Sudan",
         worm_number == 1,
         year_event > 2009,
         host == "Human") %>% 
  group_by(year_event) %>% 
  summarise(value = median(host_age)) %>% 
  ggplot(aes(x=year_event, y=value))+
  geom_area(alpha=.5, fill=burnt_sienna_light)+
  geom_vline(xintercept = seq(from=2010, to=2023, by = 1), size=1, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=30, by = 10), size=.5, color="grey90")+
  geom_line(size=1.2, colour=burnt_sienna)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.4, 
             colour = burnt_sienna,
             stroke=1.4) +
  si_style_ygrid()+
  scale_y_continuous(labels = label_number())+
  scale_x_continuous(breaks=seq(2010, 2023, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  theme(axis.text.y  = element_text( size= 15, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 15, family = "Source Sans Pro", margin = margin(t = -.18, unit = "in")),
        axis.ticks.x = element_blank())

si_save("Images/2024_gr/median_age_cases.png")




df_21_22 %>% 
  filter(sheet=="animals",
         indicator == "rumours_24") %>% 
  View()


df %>% 
  filter(indicator=="rumours_total",
         sheet %in% c("MSR_Surv", "RPIF", "Non_MSR_Surv"),
         month!="Cumulative") %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))
  

df %>% 
  filter(sheet=="animals",
         indicator %in% c("rumours_total", "rumours_invest_24")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>%
  mutate(percent=rumours_invest_24/rumours_total) %>% 
  View()

df %>% 
  filter(indicator=="report_expected",
         vas=="1",
         month=="December") %>% 
  group_by(county) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         month!="Cumulative",
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))

df %>% 
  glimpse()

df %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         month!="Cumulative",
         sheet %in% c("animals")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))
