# PURPOSE: Generate visualization for key indicators
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 2021
# NOTES: February 2022 update with dewormr dataset for Global Review


`%notin%` <- Negate(`%in%`)
current_month <- "October"
current_year <- "2022"


#MUNGE ==============================================================

df_ki<-df_21_22 %>% 
  filter(indicator %in% c("hp_working", "staff_vv", "activity_cra", "visit_cso_po_spo_ta",
                          "visit_as_hw", "visit_pso_fo", "visit_secretariat", "report_expected",
                          "report_received", "hh_total", "filter_hh_cloth"),
         vas=="1",
         #risk_level != "Risk Level 3",
         month == current_month,
         year == current_year,
         sheet=="MSR_Surv") %>% 
  select(state, county, payam, boma, reporting_unit, indicator, risk_level, value, month, sheet) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  select(-row) %>%
  group_by(state, county, payam, boma, reporting_unit, risk_level, month, sheet) %>%
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>%
  mutate(supervision=(visit_as_hw+visit_pso_fo+visit_secretariat+visit_cso_po_spo_ta),
         filter_coverage=filter_hh_cloth/hh_total,
         filter_coverage=case_when(
           filter_coverage>.95 ~ 1,
           filter_coverage<.95 ~ 0)) %>%
  mutate_if(is.numeric, ~1 * (. > 0)) %>% 
  ungroup() %>% 
  mutate(count=1)

df_ki1<-df_ki %>% 
  filter(report_expected==1,
         report_received==1,
         hh_total==1) %>% 
  group_by(risk_level, month, sheet) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  select(-hh_total, -visit_as_hw, -visit_pso_fo, -visit_secretariat, -visit_cso_po_spo_ta, -filter_hh_cloth) %>% 
  mutate(across(where(is.numeric), ~ ./count)) %>% 
  pivot_longer(where(is.numeric), names_to="indicator", values_to="value") %>% 
  filter(indicator %notin% c("count", "report_received", "report_expected"))

df_ki2<-df_21_22 %>% 
  filter(indicator %in% c("report_expected", "report_received"),
         vas==1,
         #risk_level != "Risk Level 3",
         year == current_year,
         sheet=="MSR_Surv") %>% 
  select(state, county, payam, boma, reporting_unit, indicator, risk_level, value, month, sheet) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  select(-row) %>%
  group_by(risk_level, sheet) %>% 
  #group_by(state, county, payam, boma, reporting_unit, risk_level, sheet) %>%
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>%
  mutate("report_received" = report_received/report_expected) %>%
  mutate("month" = current_month) %>% 
  select(risk_level, sheet, report_received, month) %>% 
  pivot_longer(where(is.numeric), names_to="indicator", values_to="value") 

df_ki_final<-df_ki1 %>% 
  bind_rows(df_ki2) %>% 
  mutate(indicator=case_when(
    indicator=="staff_vv" ~ "Trained Village Volunteer",
    indicator=="hp_working" ~"Safe Water Access",
    indicator=="activity_cra" ~ "Cash Reward Activities",
    indicator=="supervision" ~ "Supervisory Visit",
    indicator=="report_received" ~ "Submitted Surveillance Reports",
    indicator=="filter_coverage" ~ "Full Cloth Filter Coverage"))

indicator_order<-c("Safe Water Access", "Cash Reward Activities", "Trained Village Volunteer", "Supervisory Visit", "Submitted Surveillance Reports", "Full Cloth Filter Coverage")

df_ki_final %>% 
  mutate(indicator=fct_relevel(indicator, indicator_order)) %>% 
  ggplot(aes(value, indicator))+
  geom_path(color="gray50")+
  #geom_point(aes(size = value)) +
  geom_point(aes(size=value), size=8) +
  geom_point(aes(fill=risk_level), size=8, shape=21, color="white", show.legend = FALSE)+
  scale_fill_manual(values = c("Risk Level 1" = old_rose, "Risk Level 2" = golden_sand, "Risk Level 3" = genoa))+
  scale_x_continuous(label = percent) +
  scale_size_continuous(range = c(3, 10)) +
  expand_limits(x = c(0, 1.05)) +
  si_style_xgrid() +
  labs(x = NULL, y = NULL,
       subtitle = paste(current_month, "Status of VAS in Risk Level 1 (n=1,575) 2 (n=469) and 3 (n=447) counties")) +
  theme(axis.text.y  = element_text(size = 14, family = "Source Sans Pro" ),
        axis.text.x  = element_text(size = 14, family = "Source Sans Pro" ),
        plot.subtitle = element_text(size = 16))

si_save("Images/2022_arm/vas_key_indicators")

  


df_vas_1_2<-
tibble::tribble(
                          ~indicator,         ~type, ~value,
     "Access to Safe Drinking Water", "Level 1 VAS (n=869)",    0.2,
         "Trained Village Volunteer", "Level 1 VAS (n=869)",   0.95,
  "Monthly Health Education Session", "Level 1 VAS (n=869)",   0.73,
                  "100% of Monthly Surveillance Reports", "Level 1 VAS (n=869)",   0.99,
                 "Supervisory Visit", "Level 1 VAS (n=869)",   0.95,
     "Access to Safe Drinking Water", "Level 2 VAS (n=613)",   0.27,
         "Trained Village Volunteer", "Level 2 VAS (n=613)",   0.77,
  "Monthly Health Education Session", "Level 2 VAS (n=613)",   0.67,
                  "100% of Monthly Surveillance Reports", "Level 2 VAS (n=613)",   0.97,
                 "Supervisory Visit", "Level 2 VAS (n=613)",   0.88
  )

df_vas_1_2 %>% 
  mutate(value = as.numeric(value)) %>%
  # mutate(month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=indicator, y=value, fill=denim))+
  geom_col(alpha=.9, show.legend = FALSE)+
  facet_wrap(~type, ncol=2)+
  geom_text(aes(label=scales::percent(value, accuracy=1)), na.rm=TRUE, color=trolley_grey, hjust=0, size=18, family="Source Sans Pro SemiBold")+
  coord_flip(ylim = c(0, 1.1))+
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=50, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=50, family = "Source Sans Pro" ),
        strip.text = element_text(size = 72, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("key_indicators_lvl1_2.png",
       height = 18,
       width = 40, limitsize = FALSE)
