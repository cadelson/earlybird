# PURPOSE: Cash reward reached visuals for ARM/GR
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: November 24, 2022

df_cr <-df_21_22 %>% 
  filter(indicator %in% c("cr_reached", "cases_new"),
         year == "2022") %>% 
  group_by(indicator, month, risk_level) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(month=match(month, month.name),
         month=month.abb[month],
         month_short = substr(month,1,1),
         value_lab=paste((round(cr_reached/1000)), "k", sep=""))

month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October") 
month_order_ab<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
rl_order<- c("risk level 3", "risk level 2", "risk_level 1")
rl_color_order<- c(genoa, golden_sand, old_rose)

early_year <- c("Jan", "Feb", "Mar", "Apr", "May")
late_year<- c("Jun", "Jul", "Aug", "Sep", "Oct")

df_cr %>% 
  mutate(risk_level = as.character(risk_level)) %>% 
  mutate(rl_color=case_when(
    risk_level=="Risk Level 1" & month %in% c(early_year) ~old_rose_light,
    risk_level=="Risk Level 1" & month %in% c(late_year) ~old_rose,
    risk_level=="Risk Level 2" ~golden_sand_light,
    risk_level=="Risk Level 3" & month %in% c(early_year) ~ genoa_light,
    risk_level=="Risk Level 3" & month %in% c(late_year) ~ genoa)) %>%
  mutate(month=fct_relevel(month, month_order_ab),
         rl_color=fct_relevel(rl_color, rl_color_order),
         cases_color=case_when(is.na(cases_new) | cases_new == 0 ~ "#D9CDC3",
                               cases_new == 1 ~ "#FDAC7A",
                               cases_new == 2 ~ "#DA3C6A",
                               TRUE ~ "#A90773"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A")))%>%
  ggplot(aes(x=month, y = cr_reached, fill = rl_color))+
  geom_col(width=.7, position=position_dodge(.7))+
  #geom_col(width=.7, position=position_dodge(.7), alpha=.8)+
  facet_wrap(~risk_level)+
  geom_text(aes(label=value_lab), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=2.75, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  scale_y_continuous(breaks=seq(0, 350000, 50000), limits=c(0, 350000), labels=label_number(suffix="k", scale = 1e-3))+
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))+
  geom_rug(aes(color = factor(cases_color)), size=2, sides="b", na.rm = TRUE) +
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, family = "Source Sans Pro" ),
        strip.text = element_text(size=16, hjust=.02, vjust=-.8, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  #theme(axis.text.y = element_text(margin = margin(r = 0)))
  scale_fill_identity()+
  scale_color_identity()

si_save("Images/2022_arm/cr_month")  

ggsave("cr_by_month_fy21.png",
       height = 9,
       width = 17)

df_cr_waf<-df_cr %>% 
  group_by(risk_level) %>% 
  summarise(across(c(cr_reached), sum, na.rm=TRUE)) %>% 
  group_by(risk_level) %>% #do calculations by siteID
  ungroup() %>% 
  mutate(cr_reached = round(cr_reached / sum(cr_reached) * 100)) %>% 
  pivot_wider(names_from= risk_level, values_from = cr_reached)

waffle(df_cr_waf, rows=10, size=1.25, flip=TRUE, reverse=TRUE,
       colors= c("#D06471", "#F5C966", "#53968C"), legend_pos = "none")

ggsave("Images/2022_arm/cr_gr_waffle.png",
       height = 7,
       width = 7)

#Tonj East
df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Tonj East",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 125000, 25000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_te.png")

#Uror

df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Uror",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 200000, 20000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_ur.png")

# Nyirol and Akobo

df_21_22 %>% 
  filter(indicator == "cr_reached",
         county %in% c("Nyirol", "Akobo"),
         year == "2022") %>%
  group_by(month, county) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  facet_wrap(~county, ncol = 1)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 40000, 10000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 20, hjust=.02, family = "Source Sans Pro"))+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_ny_ak.png")

# Akobo

df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Akobo",
         year == "2022") %>%
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 10000, 2000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_ak.png")


#Rumbek North

df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Rumbek North",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 200000, 10000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 12, vjust = 4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust = 0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_rn.png")


#Awerial
df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Awerial",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 200000, 20000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 12, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_aw.png")

# Tonj North
df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Tonj North",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 15000, 5000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 12, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_tn.png")

# Rumbek Centre
df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Rumbek Centre",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 30000, 5000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 12, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_rc.png")


# Terekeka
df_21_22 %>% 
  filter(indicator == "cr_reached",
         county == "Terekeka",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 8000, 2000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 12, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_ter.png")


# Lafon
df_21_22 %>% 
  filter(indicator == "cr_reached",
         county %in% c("Lafon", "Lopa/Lafon"),
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 8000, 2000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 12, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cr_ll.png")

#Risk Level 3
df_21_22 %>% 
  filter(indicator == "cr_reached",
         risk_level == "Risk Level 3",
         year == "2022") %>% 
  group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 200000, 20000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/risk_level_3/cr_rl3.png")

#Risk Level 2
df_21_22 %>% 
  filter(indicator == "cr_reached",
         risk_level == "Risk Level 2",
         year == "2022") %>% 
  #group_by(month) %>% 
  summarise(across(c(value), sum, na.rm = TRUE)) %>%
  View()
  ungroup() %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), size = 5, na.rm=TRUE, color=trolley_grey, vjust=-.12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 200000, 20000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, vjust=4, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, vjust=0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/risk_level_2/cr_rl2.png")


df_cr_r2<-
tibble::tribble(
             ~region, ~month, ~value,
             "Akobo",  "Jan",  8697L,
             "Akobo",  "Feb",  8975L,
             "Akobo",  "Mar", 11620L,
             "Akobo",  "Apr",  7289L,
             "Akobo",  "May",  1572L,
             "Akobo",  "Jun",  1923L,
             "Akobo",  "Jul",  4308L,
             "Akobo",  "Aug",  1269L,
             "Akobo",  "Sep",  1488L,
             "Akobo",  "Oct",  2527L,
            "Nyirol",  "Jan", 18152L,
            "Nyirol",  "Feb", 16123L,
            "Nyirol",  "Mar", 22080L,
            "Nyirol",  "Apr", 15702L,
            "Nyirol",  "May", 15936L,
            "Nyirol",  "Jun", 16093L,
            "Nyirol",  "Jul", 15176L,
            "Nyirol",  "Aug", 12358L,
            "Nyirol",  "Sep", 13559L,
            "Nyirol",  "Oct", 15620L,
     "Rumbek Center",  "Jan", 13393L,
     "Rumbek Center",  "Feb",  9537L,
     "Rumbek Center",  "Mar", 19938L,
     "Rumbek Center",  "Apr", 16501L,
     "Rumbek Center",  "May", 16998L,
     "Rumbek Center",  "Jun", 17031L,
     "Rumbek Center",  "Jul", 17222L,
     "Rumbek Center",  "Aug", 19885L,
     "Rumbek Center",  "Sep", 18853L,
     "Rumbek Center",  "Oct", 18182L,
        "Tonj North",  "Jan", 34377L,
        "Tonj North",  "Feb", 29498L,
        "Tonj North",  "Mar", 22943L,
        "Tonj North",  "Apr", 34533L,
        "Tonj North",  "May", 18356L,
        "Tonj North",  "Jun", 23213L,
        "Tonj North",  "Jul", 37516L,
        "Tonj North",  "Aug", 29508L,
        "Tonj North",  "Sep", 20301L,
        "Tonj North",  "Oct", 15288L,
        "Tonj South",  "Jan",  9165L,
        "Tonj South",  "Feb", 12857L,
        "Tonj South",  "Mar",  9134L,
        "Tonj South",  "Apr",  8056L,
        "Tonj South",  "May",  8822L,
        "Tonj South",  "Jun",  7122L,
        "Tonj South",  "Jul",  6244L,
        "Tonj South",  "Aug",  5143L,
        "Tonj South",  "Sep",  5659L,
        "Tonj South",  "Oct",  5872L,
              "Wulu",  "Jan",  5302L,
              "Wulu",  "Feb",  5464L,
              "Wulu",  "Mar",  4766L,
              "Wulu",  "Apr",  5544L,
              "Wulu",  "May",  6066L,
              "Wulu",  "Jun",  4763L,
              "Wulu",  "Jul",  5124L,
              "Wulu",  "Aug",  4949L,
              "Wulu",  "Sep",  5242L,
              "Wulu",  "Oct",  4277L
     )

df_cr_r2 %>% 
  filter(region %in% c("Akobo", "Nyirol"))%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  facet_wrap(~region, ncol=1)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 25000, 5000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l2_UR.png",
       height = 16,
       width = 20)

df_cr_r2 %>% 
  filter(region %in% c("Tonj North", "Tonj South"))%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  facet_wrap(~region, ncol=1)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 30000, 10000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l2_TE.png",
       height = 16,
       width = 20)

df_cr_r2 %>% 
  filter(region %in% c("Rumbek Center", "Wulu"))%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  facet_wrap(~region, ncol=1)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 40000, 5000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l2_RN.png",
       height = 18,
       width = 20)


df_cr_r3<-
  tibble::tribble(
    ~month,  ~value,
     "Jan", 136654L,
     "Feb", 131934L,
     "Mar", 122028L,
     "Apr", 122749L,
     "May", 140380L,
     "Jun", 128387L,
     "Jul", 151552L,
     "Aug", 139605L,
     "Sep", 150444L,
     "Oct", 141866L
    )

month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")

df_cr_r3 %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 200000, 50000), limits=c(0, 175000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l3.png",
       height = 10,
       width = 19)

df_cr_aware_rl3<-
tibble::tribble(
                 ~region,     ~type, ~value,
            "Duk (n=35)",  "humans",      0,
     "Jur River (n=120)",  "humans",   0.98,
  "Kapoeta North (n=59)",  "humans",   0.85,
     "Lopa/Lafon (n=50)",  "humans",   0.92,
  "Rumbek Center (n=20)",  "humans",   0.75,
           "Wulu (n=15)",  "humans",      1,
            "Duk (n=35)", "animals",      0,
     "Jur River (n=120)", "animals",   0.92,
  "Kapoeta North (n=59)", "animals",   0.34,
     "Lopa/Lafon (n=50)", "animals",   0.82,
  "Rumbek Center (n=20)", "animals",   0.55,
           "Wulu (n=15)", "animals",   0.67
  )


type_color_order<- c("#287c6f", "#2057a7")

df_cr_aware_rl3 %>% 
  mutate(rl = as.character(region),
         value=as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type=="animals" ~denim,
    type=="humans" ~genoa)) %>% 
  #mutate(type_color=fct_relevel(type, type_color_order))%>%
  ggplot(aes(x=region, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.7, position=position_dodge(.7), alpha=.9)+
  # geom_text_repel(aes(label=comma(round(Cases), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", nudge_y=8000, nudge_x=.1, family="Source Sans Pro SemiBold",
  #            data = . %>%
  #              filter(row_number() %% 5 == 3))+
  geom_text(aes(label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", hjust=-.05, size=8, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_ygrid()+
  coord_flip()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=24, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("cr_aware.png",
       height = 12,
       width = 10)
