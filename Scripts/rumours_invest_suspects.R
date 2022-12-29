# PURPOSE: Generate visualization for number of rumours, invest <24 hours, suspects by month for provisional analysis narrative
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: April 29, 2022
# NOTES: November 2022 added animal rumours
`%notin%` <- Negate(`%in%`)
current_month<-c("October")
month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October") 

# Munge data for humans and animals
df_viz<-df_21_22 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total", "cases_new"),
         year=="2022",
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other", "animals")) %>%
  mutate(sheet = case_when(
    sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other") ~ "human",
    TRUE ~ "animals")) %>% 
  group_by(month, indicator, sheet) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup() %>%
  pivot_wider(names_from=indicator, values_from=value) %>%
  ungroup() %>% 
  mutate(rumours_invest_24=rumours_invest_24/rumours_total,
         rumours_ns=rumours_total-suspects_total,
         month=fct_relevel(month, month_order),
         cases_color=case_when(is.na(cases_new) | cases_new==0 ~ "#D9CDC3",
                               cases_new==1 ~ "#FDAC7A",
                               cases_new==2 ~ "#FDAC7A",
                               TRUE ~ "#A90773"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A"))) %>% 
  rename("Rumours"=rumours_ns,
         "Rumours Investigated <24 Hours"=rumours_invest_24,
         "Suspects"=suspects_total,
         "Cases"=cases_new) %>%
  pivot_longer(c(Rumours, `Rumours Investigated <24 Hours`, Suspects), names_to = "indicator") %>%
  mutate(
    indicator_fill_color=case_when(
      indicator == "Rumours" & sheet =="human"  ~ genoa_light,
      indicator == "Rumours Investigated <24 Hours" & sheet =="human" ~ genoa_light,
      indicator == "Suspects" & sheet =="human" ~ genoa,
      indicator == "Rumours" & sheet =="animals"  ~ moody_blue_light,
      indicator == "Rumours Investigated <24 Hours" & sheet =="animals" ~ moody_blue_light,
      indicator == "Suspects" & sheet =="animals" ~ moody_blue),
    value_label = case_when(
      indicator == "Suspects" ~ comma(round(value, digits=1))))
    #value_label = comma(round(value, digits=1)))

# Humans viz
df_viz %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         sheet != "animals") %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  geom_text(aes(label=value_label), size=18/.pt, color="white", family="Source Sans Pro", vjust = -1)+
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
  

si_save("Images/rumours_monthly.png")

df_viz %>% 
  filter(indicator=="Rumours Investigated <24 Hours",
         sheet != "animals") %>%
  select(month, value) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(month, value)) +
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

# Animals viz
df_viz %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         sheet == "animals") %>% 
  mutate(cases_color = case_when(
    month == "August" ~ "#FDAC7A",
    TRUE ~ "#D9CDC3")) %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  geom_text(aes(label=value_label), size=18/.pt, color="white", family="Source Sans Pro", vjust = -1)+
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


si_save("Images/rumours_monthly_animals")  

df_viz %>% 
  filter(indicator=="Rumours Investigated <24 Hours",
         sheet == "animals") %>%
  select(month, value) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(group=1), size=.5, colour= moody_blue)+
  geom_point(fill = "white", shape = 21, size = 3, colour = moody_blue, stroke = 3)+
  geom_text(aes(label=percent(value, .1)), size=6, vjust=-1, colour="#505050")+
  scale_y_continuous(breaks=seq(0, 1, 1), limits=c(.75, 1.01))+
  labs(X = NULL, y = NULL)+
  si_style_nolines()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

si_save("Images/rumours_24_monthly_animals")  


#Calculate total rumours, suspects for title
df_viz %>%
  filter(sheet == "animals", indicator == "Suspects") %>%
  group_by(indicator) %>% 
  summarise(across(c(value, rumours_total), sum, na.rm=TRUE))
