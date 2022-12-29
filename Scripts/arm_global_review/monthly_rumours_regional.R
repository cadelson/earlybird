# PURPOSE: Generate visualization for number of rumours, invest <24 hours, suspects by county for ARM regional presentations
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: November 23, 2022

`%notin%` <- Negate(`%in%`)
current_month<-c("October")
month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") 

# Munge data for humans and animals
df_viz<-df_21_22 %>% 
  mutate(county = case_when(county == "Lopa/Lafon" ~ "Lafon", TRUE ~ county)) %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total", "cases_new"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other", "animals")) %>%
  mutate(sheet = case_when(
    sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other") ~ "human",
    TRUE ~ "animals"),
    county = case_when(county == "Lopa/Lafon" ~ "Lafon", TRUE ~ county)) %>% 
  group_by(month, county, year, indicator, sheet) %>% 
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
      indicator == "Suspects" ~ comma(round(value, digits=0))))


# Humans viz
df_viz %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         sheet != "animals",
         county == "Terekeka") %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  geom_text(aes(label=value_label), size=18/.pt, color="white", family="Source Sans Pro", vjust = -.3)+
  geom_text(aes(x=month, y=rumours_total, label=comma(rumours_total)), size=18/.pt, color="#505050", family="Source Sans Pro", vjust=-.3)+
  si_style_ygrid()+
  facet_wrap(~year) +
  geom_rug(aes(color = factor(cases_color)), size=3, sides="b", na.rm = TRUE) +
  scale_fill_identity()+
  scale_color_identity()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size=10, family = "Source Sans Pro"),
        axis.text.y  = element_text(size=14, family = "Source Sans Pro" ),
        strip.text = element_text(size =18, family = "Source Sans Pro Semibold"))+
  scale_y_continuous(breaks = seq(0, 1000, 250), labels=comma, limits = c(0, 1000))


si_save("Images/2022_arm/county_presentations/rumours_suspects_rc_21_22.png", width = 18)


df_viz %>% 
  filter(indicator=="Rumours Investigated <24 Hours",
         county == "Rumbek Centre",
         sheet != "animals") %>%
  select(month, value, year) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(group=1), size=.5, colour=genoa)+
  geom_point(fill = "white", shape = 21, size = 3, colour = genoa, stroke = 3)+
  facet_wrap(~year) +
  geom_text(aes(label=percent(value, .1)), size=5, vjust=-1, colour="#505050")+
  scale_y_continuous(breaks=seq(0, 1, 1), limits=c(.7, 1.03))+
  labs(X = NULL, y = NULL)+
  si_style_nolines()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

si_save("Images/2022_arm/county_presentations/invest_24_rc_21_22.png", width = 18)

# Animals viz
df_viz %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         sheet == "animals",
         county == "Tonj East") %>% 
  mutate(cases_color = case_when(
    month == "August" ~ "#FDAC7A",
    TRUE ~ "#D9CDC3")) %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  geom_text(aes(label=value_label), size=18/.pt, color = "white", family = "Source Sans Pro", vjust = -.3)+
  geom_text(aes(x=month, y=rumours_total, label=comma(rumours_total)), size=18/.pt, color="#505050", family="Source Sans Pro", vjust=-.3)+
  si_style_ygrid()+
  facet_wrap(~year) +
  geom_rug(aes(color = factor(cases_color)), size=3, sides="b", na.rm = TRUE) +
  scale_fill_identity()+
  scale_color_identity()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size=10, family = "Source Sans Pro"),
        axis.text.y  = element_text(size=14, family = "Source Sans Pro" ),
        strip.text = element_text(size =18, family = "Source Sans Pro Semibold"))+
  scale_y_continuous(labels=comma)


si_save("Images/2022_arm/county_presentations/rumours_suspects_animals_te_21_22.png", width = 18)

df_viz %>% 
  filter(indicator=="Rumours Investigated <24 Hours",
         sheet == "animals",
         county == "Tonj East") %>%
  select(month, value, year) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(group=1), size=.5, colour= moody_blue)+
  geom_point(fill = "white", shape = 21, size = 3, colour = moody_blue, stroke = 3)+
  facet_wrap(~year) +
  geom_text(aes(label=percent(value, .1)), size=5, vjust=-1, colour="#505050")+
  scale_y_continuous(breaks=seq(0, 1, 1), limits=c(.75, 1.01))+
  labs(X = NULL, y = NULL)+
  si_style_nolines()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

si_save("Images/2022_arm/county_presentations/invest_24_animals_te_21_22.png", width = 18)


# For risk level 3 areas
df_viz_3<-df_21_22 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total", "cases_new"),
         year=="2022",
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other", "animals")) %>%
  mutate(sheet = case_when(
    sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other") ~ "human",
    TRUE ~ "animals")) %>% 
  group_by(month, indicator, sheet, risk_level) %>% 
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
      indicator == "Suspects" ~ comma(round(value, digits=0))))


df_viz_3 %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         sheet != "animals",
         risk_level == "Risk Level 3") %>% 
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
  theme(axis.text.x  = element_text(size=14, family = "Source Sans Pro"),
        axis.text.y  = element_text(size=14, family = "Source Sans Pro" ))+
  scale_y_continuous(labels=comma)


si_save("Images/2022_arm/risk_level_3/rumours_suspects_level_3.png")

df_viz_3 %>% 
  filter(indicator=="Rumours Investigated <24 Hours",
         risk_level == "Risk Level 3",
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

si_save("Images/2022_arm/risk_level_3/invest_24_level_3.png")
