# PURPOSE: Generate visualization for number of animal rumours by month
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 1, 2021
# NOTES:


df_animal_rumours<- 
tibble::tribble(
     ~month, ~year, ~rumours,
      "Jan", 2018L,       0L,
      "Feb", 2018L,       0L,
      "Mar", 2018L,       0L,
      "Apr", 2018L,       0L,
      "May", 2018L,       1L,
      "Jun", 2018L,       3L,
      "Jul", 2018L,       0L,
      "Aug", 2018L,       1L,
      "Sep", 2018L,       0L,
      "Oct", 2018L,       0L,
      "Nov", 2018L,       1L,
      "Dec", 2018L,       0L,
      "Jan", 2019L,       0L,
      "Feb", 2019L,       2L,
      "Mar", 2019L,       1L,
      "Apr", 2019L,       4L,
      "May", 2019L,       1L,
      "Jun", 2019L,       3L,
      "Jul", 2019L,       0L,
      "Aug", 2019L,       6L,
      "Sep", 2019L,      11L,
      "Oct", 2019L,      27L,
      "Nov", 2019L,      13L,
      "Dec", 2019L,      56L,
      "Jan", 2020L,      42L,
      "Feb", 2020L,      44L,
      "Mar", 2020L,      43L,
      "Apr", 2020L,      49L,
      "May", 2020L,      42L,
      "Jun", 2020L,      89L,
      "Jul", 2020L,      48L,
      "Aug", 2020L,      54L,
      "Sep", 2020L,      32L,
      "Oct", 2020L,      54L,
      "Nov", 2020L,      33L,
      "Dec", 2020L,      40L,
      "Jan", 2021L,      25L,
      "Feb", 2021L,      37L,
      "Mar", 2021L,      52L,
      "Apr", 2021L,      69L,
      "May", 2021L,      48L,
      "Jun", 2021L,      53L,
      "Jul", 2021L,      29L,
      "Aug", 2021L,      86L,
      "Sep", 2021L,      48L,
      "Oct", 2021L,      40L
     )


month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 
year_color_order<- c("#A4DCD1", "#329A89", "#1E5C52")

df_animal_rumours %>% 
  filter(year!="2018") %>% 
  mutate(year = as.character(year)) %>% 
  mutate(year_color=case_when(
    year=="2019" ~"#A4DCD1",
    year=="2020" ~"#329A89",
    year=="2021" ~"#1E5C52",
    TRUE ~ trolley_grey)) %>% 
  mutate(month=fct_relevel(month, month_order),
         year_color=fct_relevel(year_color, year_color_order))%>%
  ggplot(aes(x=month, y=rumours, fill=year_color))+
  #geom_col(position=position_dodge2(preserve="single"))+
  geom_col(width=.7, position=position_dodge(.7))+
  geom_text(aes(label=comma(round(rumours), accuracy=1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(breaks=seq(0, 100, 25))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=36, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=30, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("animal_rumours_by_month.png",
       height = 14,
       width = 24)

df_animal_rumours %>% 
  group_by(year) %>% 
  summarise(across(c(rumours), sum, na.rm = TRUE))
