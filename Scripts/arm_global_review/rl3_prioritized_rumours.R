# PURPOSE: Create visualization for rumours, risk level 3 counties
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: November 24, 2022


`%notin%` <- Negate(`%in%`)
current_month<-c("October")
month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September") 

# Munge data for humans and animals
df_rl3_rumours <- df_21_22 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         year=="2022",
         risk_level %in% c("Risk Level 3"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other")) %>%
  group_by(county, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup() %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  ungroup() %>% 
  mutate(rumours_invest_24=rumours_invest_24/rumours_total,
         rumours_ns=rumours_total-suspects_total) %>% 
  rename("Rumours"=rumours_ns,
         "Rumours Investigated <24 Hours"=rumours_invest_24,
         "Suspects"=suspects_total) %>%
  pivot_longer(c(Rumours, `Rumours Investigated <24 Hours`, Suspects), names_to = "indicator") %>%
  mutate(
    indicator_fill_color=case_when(
      indicator == "Rumours" ~ burnt_sienna_light,
      indicator == "Rumours Investigated <24 Hours" ~ burnt_sienna_light,
      indicator == "Suspects" ~ burnt_sienna),
    value_label = case_when(
      indicator == "Suspects" ~ comma(round(value, digits=0))))

  
df_rl3_rumours %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         county %notin% c("Uror", "Nyirol", "Torit", "Lafon")) %>% 
  mutate(county = fct_reorder(county, rumours_total)) %>% 
  ggplot(aes(county, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  scale_y_continuous(labels = comma, limits = c(0, 3000)) +
  coord_flip() +
  geom_text(aes(label=value_label), size=18/.pt, color="white", family="Source Sans Pro", hjust = 0)+
  geom_text(aes(x=county, y=rumours_total, label=comma(rumours_total)), size=18/.pt, color="#505050", family="Source Sans Pro", hjust=-.1)+
  si_style_xgrid()+
  scale_fill_identity()+
  scale_color_identity()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size=16, family = "Source Sans Pro"),
        axis.text.y  = element_text(size=14, family = "Source Sans Pro", margin = margin(r = -.3, unit = "in")))
  


si_save("Images/2022_arm/risk_level_3/rumours_suspects_rl3.png")
ggsave("Images/2022_arm/risk_level_3/rumours_suspects_rl3.png", height = 5.625, width = 10)
 