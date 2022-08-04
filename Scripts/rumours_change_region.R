# PURPOSE: Visualization showing change in rumours by county, fy21-fy22
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: July 15, 2022
# NOTES:

`%notin%` <- Negate(`%in%`)

rumour_change <- df_21_22 %>%
  filter(indicator %in% c("rumours_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other")) %>%
  mutate(qtr=ceiling(match(month, month.name)/3)) %>% 
  group_by(county, qtr, year) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = c(year, qtr), values_from = value, names_sep="Q") %>% 
  mutate(pct_change=(`2022Q1`+`2022Q2`- (`2021Q4`+`2021Q3` + `2021Q2` + `2021Q1`)/2)/((`2021Q4`+`2021Q3` + `2021Q2` + `2021Q1`)/2),
         total_rumours=comma(`2021Q1`+`2021Q2`+`2021Q3`+`2021Q4`+ `2022Q1` + `2022Q2`)) %>% 
  pivot_longer(c("2021Q1":"2021Q4"), names_to = "yr_qtr") %>% 
  mutate(year=substr(yr_qtr, 1L, 4L),
         yr_color=case_when(year=="2021" ~ grey80k,
                            TRUE ~ denim),
         pct_change=case_when(yr_qtr=="2022Q2" ~ pct_change,
                              TRUE ~ NA_real_),
         pct_change_color=case_when(pct_change > .1 ~ scooter, 
                                    pct_change < .1 & pct_change > -.1 ~ grey60k,
                                    pct_change < -.1 ~ old_rose),
         county_label= paste0(county, " (", total_rumours, ")"),
         county_label = reorder(county_label, -pct_change, sum, na.rm=TRUE),
         yr_qtr_label=substr(yr_qtr, 3L, 7L)) %>% 
  filter(county %notin% c("Abeyi", "Aweil Center", "Aweil Centre", "Aweil West", "Budi", "Cueibet", "Duk", "Fashoda", "Juba", "Kapoeta East", "Kapoeta North", "Kapoeta South",
                          "Magwi", "Mayom", "Mvolo", "Panyijiar", "Raja", "Rubkona", "Rumbek East", "Wulu", "Yirol East", "Yirol West"),
         !is.na(county))

rumour_change %>% 
  ggplot(aes(x=yr_qtr_label, y=value, fill=yr_color))+
  geom_bar(stat='identity', show.legend=FALSE)+
  geom_label(aes(label = percent(pct_change, 1), x = 5.4, y=5000, fill="white", color=pct_change_color),
            label.size = NA, family = "Source Sans Pro Black", size=4) +
  facet_wrap(~county_label)+
  si_style_ygrid()+
  scale_y_continuous(labels=comma, breaks=seq(0, 5000, 2500))+
  theme(axis.text.x  = element_text(family = "Source Sans Pro", size=26/.pt),
        axis.text.y  = element_text(vjust=0.5, family = "Source Sans Pro", size=24/.pt),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 12))+
  scale_fill_identity()+
  scale_color_identity()+
  labs(x=NULL, y=NULL, caption= "growth calculation = (FY22 rumours - (FY21 rumours/2)) / (FY21 rumours/2)")

si_save("Images/rumour_change_county_fy21_fy22.png")


rumour_total <- df_21_22 %>% 
  filter(indicator %in% c("rumours_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other"), 
         year==2022) %>% 
  mutate(transition_county = case_when(
    county %in% c("Ikotos", "Jur River", "Kapoeta North", "Lopa/Lafon", "Tonj North", "Tonj South", "Torit", "Wau", "Gogrial West") ~ "transition",
    county == "Rumbek Centre" ~ "partial_transition",
    TRUE ~ "no transition")) %>% 
  group_by(transition_county) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(rumour_share=percent(value/sum(value), 1),
         transition_color = case_when(
           transition_county == "transition" ~ genoa,
           transition_county == "partial_transition" ~ moody_blue,
           TRUE ~ grey30k),
         "county" = "county",
         rumour_label_1 = case_when(
           transition_county %in% c("transition") ~ `rumour_share`,
           TRUE ~ ""),
         rumour_label_2 = case_when(
           transition_county %in% c("partial_transition") ~ `rumour_share`,
           TRUE ~ ""),
         transition_color = fct_relevel(transition_color, c(genoa, moody_blue, grey30k))) %>% 
  filter(value != 0)

rumour_total %>% 
  ggplot(aes(county, value, fill = transition_color)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = rumour_label_1), family = "Source Sans Pro", size=12, vjust = 3.5)+
  geom_text(aes(label = rumour_label_2), family = "Source Sans Pro", size=12, vjust = -4.5)+
  si_style_nolines()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())+
  scale_fill_identity()

si_save("Images/rumour_share_transition.png", width=5, height=10)

transition_rumours_share <- df_21_22 %>% 
  filter(indicator %in% c("rumours_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other"), 
         year==2022) %>% 
  group_by(county) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  mutate(rumour_share=value/sum(value)) %>% 
  #mutate(rumour_share=percent(value/sum(value), 1)) %>% 
  filter(county %in% c("Ikotos", "Jur River", "Kapoeta North", "Lopa/Lafon", "Rumbek Centre", "Tonj North", "Tonj South", "Torit", "Wau", "Gogrial West")) %>% 
  mutate(#rumour_share=percent(value/sum(value), 1),
         transition_county = case_when(
           county == "Rumbek Centre" ~ "partial_transition",
           TRUE ~ "transition"),
         transition_color = case_when(
           transition_county == "transition" ~ genoa,
           TRUE ~ moody_blue),
         county = reorder(county, -rumour_share, sum, na.rm = TRUE))

transition_rumours_share %>% 
  ggplot(aes(county, rumour_share, fill=transition_color))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = percent(rumour_share, 1)), family = "Source Sans Pro", vjust = -.5)+
  si_style_ygrid()+
  scale_y_continuous(labels = scales::percent, breaks=seq(0, .15, .05), limits = c(0,.15))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "Source Sans Pro"))+
  scale_fill_identity()
  
si_save("Images/rumour_share_transition_county.png")
