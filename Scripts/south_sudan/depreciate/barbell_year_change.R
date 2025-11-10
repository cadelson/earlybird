# PURPOSE: Create a visualization showing changes in indicators between FYs
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: June 4, 2022
# NOTES:

`%notin%` <- Negate(`%in%`)

#Total number of VAS 2021 - 2022
#munge
df_barbell_state <- df_21_22 %>% 
  filter(indicator=="report_expected",
         year =="2022" & month == "April" | year=="2021" & month == "December",
         vas==1,
         state %in% c("Eastern Equatoria", "Jonglei", "Lakes", "Warrap", "Western Bahr El Ghazal"),
         county %notin% c("Rumbek East", "Wulu")) %>% 
  group_by(risk_level, year, month, county) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  mutate(month=fct_relevel(month, "April", "December"),
         county=fct_relevel(county, "Tonj East", "Awerial", "Uror", "Rumbek North",
                            "Tonj South", "Tonj North", "Nyirol", "Terekeka"))
         #cc=fct_relevel(cc, "Village", "Cattle Camp"))


df_barbell_state %>% 
  ggplot(aes(month, value, group=risk_level, color=risk_level))+
  facet_wrap(~county)+
  geom_path(show.legend = FALSE, size=2)+
  geom_point(aes(size=value), size=12, show.legend=FALSE) +
  geom_point(size=12, shape=21, color="white", show.legend = FALSE)+
  geom_text(aes(label=comma(value)), color="black", family="Source Sans Pro")+
  si_style_ygrid()+
  scale_color_manual(values = c("Risk Level 1" = old_rose, "Risk Level 2" = golden_sand, "Risk Level 3" = genoa))+
  labs(x=NULL, y=NULL,
       subtitle = "Change in VAS, FY 21-22")

#Cloth filter coverage

df_cf <- df_21_22 %>% 
  filter(indicator %in% c("filter_hh_cloth", "hh_total", "report_expected", "report_received"),
         !is.na(value),
         year =="2022" & month == "April" | year=="2021" & month == "December",
         vas==1,
         risk_level=="Risk Level 1") %>% 
  group_by(county, risk_level, indicator, month, year) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from=indicator) %>% 
  ungroup() %>% 
  mutate(filter_coverage=filter_hh_cloth/hh_total,
         month=fct_relevel(month, "December", "April"),
         county=fct_relevel(county, "Tonj East", "Awerial", "Uror", "Rumbek North",
                            "Tonj South", "Tonj North", "Nyirol", "Terekeka"),
         county_label=case_when(
           month=="April" ~ county))



df_cf %>% 
  ggplot(aes(month, filter_coverage, group=county, color=risk_level))+
  geom_path(show.legend = FALSE, size=1, colour=grey80k)+
  geom_point(aes(size=value), size=12, show.legend=FALSE) +
  geom_point(size=12, shape=21, color="white", show.legend = FALSE)+
  geom_text(aes(label=percent(filter_coverage, accuracy=1)), color=grey90k, family="Source Sans Pro")+
  geom_text(aes(label=county_label, x=2.05), hjust=0, color=grey90k, family="Source Sans Pro")+
  si_style_ygrid()+
  scale_color_manual(values = c("Risk Level 1" = denim_light))+
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0,1))+
  labs(x=NULL, y=NULL,
       subtitle = "Change in Cloth Filter Coverage, Risk Level 1 Areas FY 21-22")

si_save("Graphics/Risk_Level_1_CF.png")
