# PURPOSE: Request from Sarah -- "Graph of the rumours and suspects for the last two years 
# in all risk level two areas"
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: November 24, 2022

`%notin%` <- Negate(`%in%`)
current_month<-c("October")
month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

df_rumours_level_2 <-df_21_22 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other"),
         risk_level == "Risk Level 2") %>%
  group_by(indicator, month, year) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  ungroup() %>% 
  mutate(rumours_invest_24=rumours_invest_24/rumours_total,
         rumours_ns=rumours_total-suspects_total,
         month=match(month, month.name),
         month=month.abb[month],
         month=fct_relevel(month, month_order)) %>% 
  rename("Rumours"=rumours_ns,
         "Rumours Investigated <24 Hours"=rumours_invest_24,
         "Suspects"=suspects_total) %>%
  pivot_longer(c(Rumours, `Rumours Investigated <24 Hours`, Suspects), names_to = "indicator") %>%
  mutate(
    indicator_fill_color=case_when(
      indicator == "Rumours" ~ golden_sand_light,
      indicator == "Rumours Investigated <24 Hours" ~ golden_sand_light,
      indicator == "Suspects" ~ golden_sand,),
    value_label = case_when(
      indicator == "Suspects" ~ comma(round(value, digits=1))))

df_rumours_level_2 %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total)) %>% 
  mutate(year_label = case_when(
    year == "2021" ~ "",
    year == "2022" ~ "11,214"
  )) %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  facet_wrap(~year) +
  geom_text(aes(label=value_label), size=10/.pt, color="white", family="Source Sans Pro", vjust = -1)+
  geom_text(aes(x=month, y=rumours_total, label=comma(rumours_total)), size=10/.pt, color="#505050", family="Source Sans Pro", vjust=-.3)+
  si_style_ygrid()+
  scale_fill_identity()+
  scale_color_identity()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size=12, vjust = 7.5, family = "Source Sans Pro"),
        axis.text.y  = element_text(size=12, family = "Source Sans Pro" ),
        strip.text = element_text(size =14, family = "Source Sans Pro Semibold"))+
  scale_y_continuous(labels=comma)


si_save("Images/2022_arm/risk_level_2/rumours_suspects_level_2.png")


df_rumours_level_2 %>% 
  filter(indicator=="Rumours Investigated <24 Hours") %>%
  select(month, value, year) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(group=1), size=.5, colour = golden_sand)+
  geom_point(fill = "white", shape = 21, size = 2, colour = golden_sand, stroke = 2)+
  facet_wrap(~year) +
  geom_text(aes(label=percent(value, .1)), size=2.9, vjust=-1, colour="#505050")+
  scale_y_continuous(breaks=seq(0, 1, 1), limits=c(.75, 1.25))+
  labs(X = NULL, y = NULL)+
  si_style_nolines()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

si_save("Images/2022_arm/risk_level_2/invest_24_level_2.png")  
