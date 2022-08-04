# PURPOSE: Generate visualization for number of rumours, invest <24 hours, suspects by month
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: April 29, 2022
# NOTES:
`%notin%` <- Negate(`%in%`)
current_month<-c("June")
month_order<- c("January", "February", "March", "April", "May", "June") 

df_viz<-df_21_22 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total", "cases_new"),
         year=="2022",
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other")) %>%
  group_by(month, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup() %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  ungroup() %>% 
  mutate(rumours_invest_24=rumours_invest_24/rumours_total,
         rumours_ns=rumours_total-suspects_total,
         month=fct_relevel(month, month_order),
         #rumours_invest_24=as.numeric(percent(rumours_invest_24, .1)),
         cases_color=case_when(is.na(cases_new) | cases_new==0 ~ "#D9CDC3",
                               cases_new==1 ~ "#FDAC7A",
                               cases_new==2 ~ "#DA3C6A",
                               TRUE ~ "#A90773"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A"))) %>% 
  rename("Rumours"=rumours_ns,
         "Rumours Investigated <24 Hours"=rumours_invest_24,
         "Suspects"=suspects_total,
         "Cases"=cases_new) %>%
  pivot_longer(c(Rumours, `Rumours Investigated <24 Hours`, Suspects), names_to = "indicator") %>%
  mutate(
    indicator_fill_color=case_when(
      indicator == "Rumours"  ~ genoa_light,
      indicator == "Rumours Investigated <24 Hours" ~ genoa_light,
      indicator == "Suspects" ~ genoa),
    value_label = comma(round(value, digits=1)))

#df_viz$alphayr<-as.factor(ifelse(df_viz$month %notin% current_month, 0.6, 1))

df_viz %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total)) %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    #alpha=factor(alphayr)), 
    position="stack", stat="identity", show.legend=FALSE)+
  geom_text(aes(label=value_label), size=18/.pt, color="white", family="Source Sans Pro", vjust=3.3)+
  geom_text(aes(x=month, y=rumours_total, label=comma(rumours_total)), size=18/.pt, color="#505050", family="Source Sans Pro", vjust=-.3)+
  si_style_ygrid()+
  geom_rug(aes(color = factor(cases_color)), size=3, sides="b", na.rm = TRUE) +
  scale_fill_identity()+
  scale_color_identity()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size=16, family = "Source Sans Pro"),
        axis.text.y  = element_text(size=14, family = "Source Sans Pro" ))+
  scale_y_continuous(labels=comma)
  

si_save("Images/rumours_monthly")  

df_viz %>% 
  filter(indicator=="Rumours Investigated <24 Hours") %>%
  select(month, value) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(month, value))+
  geom_line(aes(group=1), size=.5, colour=genoa)+
  geom_point(fill = "white", shape = 21, size = 3, colour = genoa, stroke = 3)+
  geom_text(aes(label=percent(value, .1)), size=6, vjust=-1, colour="#505050")+
  scale_y_continuous(breaks=seq(0, 1, 1), limits=c(.96, 1.01))+
  labs(X = NULL, y = NULL)+
  si_style_nolines()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
  
si_save("Images/rumours_24_monthly")  
