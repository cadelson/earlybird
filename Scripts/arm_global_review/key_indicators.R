# PURPOSE: Generate visualization for key indicators
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 2021
# NOTES: February 2022 update with dewormr dataset for Global Review


`%notin%` <- Negate(`%in%`)

# df_ki1p<-
# tibble::tribble(
#                                     ~indicator, ~value,
#                       "1+ Safe Drinking Water",  ".25",
#                            "Trained Volunteer", "1",
#                     "Monthly Health Education", "1",
#   "Pipe Filter Coverage (Eligible Population)", "1",
#                   "100% Cloth Filter Coverage", "1",
#                             "Abate Treatments", "1",
#                       "Geographic Coordinates", "1"
#   )
# 
# df_ki1p %>% 
#   mutate(value = as.numeric(value)) %>%
#   # mutate(month=fct_relevel(month, month_order)) %>% 
#   ggplot(aes(x=indicator, y=value, fill=scooter_med))+
#   geom_col(alpha=.9, show.legend = FALSE)+
#   geom_text(aes(label=scales::percent(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, hjust=-.3, size=12, family="Source Sans Pro SemiBold")+
#   coord_flip()+
#   scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25))+
#   si_style_ygrid()+
#   labs(x = NULL, y = NULL)+
#   theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
#         axis.text.y  = element_text(vjust=0.5, size=32, family = "Source Sans Pro" ),
#         strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
#         axis.line.x = element_blank(),
#         axis.ticks.x = element_blank())
# 
# ggsave("key_indicators_1plus.png",
#        height = 14,
#        width = 42)

rl1<-c("Uror", "Rumbek North", "Tonj East", "Awerial")
rl2<-c("Nyirol", "Tonj North", "Tonj South", "Cueibet", "Rumbek Centre", "Mayendit",
       "Panyijiar", "Yirol East", "Yirol West", "Terekeka")


#MUNGE ==============================================================

df_ki<-df %>% 
  mutate(
    rl=case_when(
      county %in% c(rl1) ~ "rl1",
      county %in% c(rl2) ~ "rl2")) %>% 
  filter(indicator %in% c("hp_working", "staff_vv", "activity_cra", "visit_cso_po_spo_ta",
                          "visit_as_hw", "visit_pso_fo", "visit_secretariat", "report_expected",
                          "report_received", "hh_total", "filter_hh_cloth"),
         vas=="1",
         !is.na(rl),
         month=="December",
         sheet=="MSR_Surv") %>% 
  select(state, county, payam, boma, reporting_unit, indicator, rl, value, month, sheet) %>%
  mutate(row = row_number()) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  select(-row) %>%
  group_by(state, county, payam, boma, reporting_unit, rl, month, sheet) %>%
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
  group_by(rl, month, sheet) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  select(-hh_total, -visit_as_hw, -visit_pso_fo, -visit_secretariat, -visit_cso_po_spo_ta, -filter_hh_cloth) %>% 
  mutate(across(where(is.numeric), ~ ./count)) %>% 
  pivot_longer(where(is.numeric), names_to="indicator", values_to="value") %>% 
  filter(indicator %notin% c("count", "report_received", "report_expected")) %>% 
  View()
  
df_ki2<-df_ki %>% 
  filter(report_expected==1) %>% 
  group_by(rl, month, sheet) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  select(rl, month, sheet, report_received, count) %>% 
  mutate(across(where(is.numeric), ~ ./count)) %>% 
  pivot_longer(where(is.numeric), names_to="indicator", values_to="value") %>% 
  filter(indicator %notin% c("count"))

df_ki_final<-df_ki1 %>% 
  bind_rows(df_ki2) %>% 
  mutate(indicator=case_when(
    indicator=="staff_vv" ~ "Trained Village Volunteer",
    indicator=="hp_working" ~"Safe Water Access",
    indicator=="activity_cra" ~ "Cash Reward Activities",
    indicator=="supervision" ~ "Supervisory Visit",
    indicator=="report_received" ~ "100% of Monthly Surveillance Reports",
    indicator=="filter_coverage" ~ "Complete Cloth Filter Coverage"))

indicator_order<-c("Safe Water Access", "Cash Reward Activities", "Trained Village Volunteer", "Supervisory Visit", "100% of Monthly Surveillance Reports",
                   "Complete Cloth Filter Coverage")

df_ki_final %>% 
  mutate(
    lab_rl1=case_when(
      rl=="rl2" & indicator == "Trained Village Volunteer" ~ value,
      rl=="rl1" & indicator == "Supervisory Visit" ~ value,
      #rl=="rl1" & indicator =="Safe Water Access" ~ value,
      rl=="rl1" & indicator == "100% of Monthly Surveillance Reports" ~ value,
      rl=="rl2" & indicator =="Cash Reward Activities" ~value),
    lab_rl2=case_when(
      rl=="rl1" & indicator == "Trained Village Volunteer" ~ value,
      rl=="rl2" & indicator == "Supervisory Visit" ~ value,
      #rl=="rl2" & indicator =="Safe Water Access" ~ value,
      rl=="rl2" & indicator == "100% of Monthly Surveillance Reports" ~ value,
      rl=="rl1" & indicator =="Cash Reward Activities" ~value),
    lab_r1_overlap=case_when(
      rl=="rl1" & indicator=="Safe Water Access" ~ value),
    lab_r2_overlap=case_when(
      rl=="rl2" & indicator=="Safe Water Access" ~ value),
    lab_r1_overlap2=case_when(
      rl=="rl2" & indicator=="Complete Cloth Filter Coverage" ~ value),
    lab_r2_overlap2=case_when(
      rl=="rl1" & indicator=="Complete Cloth Filter Coverage" ~ value),
    indicator=fct_relevel(indicator, indicator_order)) %>% 
  ggplot(aes(value, indicator))+
  geom_path(color="gray50")+
  #geom_point(aes(size = value)) +
  geom_point(aes(size=value), size=8) +
  geom_point(aes(fill=rl), size=8, shape=21, color="white", show.legend = FALSE)+
  geom_text(aes(label = percent(lab_rl1, 1)), family = "Source Sans Pro",
           color = trolley_grey, hjust = -.6, na.rm = TRUE)+
  geom_text(aes(label = percent(lab_rl2, 1)), family = "Source Sans Pro",
            color = trolley_grey, hjust = 1.6, na.rm = TRUE)+
  geom_text(aes(label=percent(lab_r1_overlap, 1)), family="Source Sans Pro",
                 color=trolley_grey, hjust=1.6, fill="white", na.rm=TRUE)+
  geom_text(aes(label=percent(lab_r2_overlap, 1)), family="Source Sans Pro",
                 color=trolley_grey, hjust= -.8, fill="white", na.rm=TRUE)+  
  geom_label(aes(label=percent(lab_r1_overlap2, 1)), family="Source Sans Pro",
             color=trolley_grey, hjust=-1.6, fill="white", na.rm=TRUE, label.size=NA, label.padding = unit(.05, "lines"))+
  geom_label(aes(label=percent(lab_r2_overlap2, 1)), family="Source Sans Pro",
             color=trolley_grey, hjust= 2, fill="white", na.rm=TRUE, label.size=NA, label.padding= unit(.05, "lines"))+
  scale_fill_manual(values = c("rl1" = old_rose, "rl2" = golden_sand))+
  scale_x_continuous(label = percent) +
  scale_size_continuous(range = c(3, 10)) +
  expand_limits(x = c(0, 1.05)) +
  labs(x = NULL, y = NULL,
       subtitle = "December Indicator Status of Villages Under Surveillance in Risk Level 1 (n=1,401) and 2 Counties (n=611)") +
  # labs(x = NULL, y = NULL,
  #      subtitle = "December Key Indicator Status in Risk Level 1 and 2 Counties",
  #      caption = "Estimated ART Coverage = FY21 TX_CURR_SUBNAT / FY21 PLHIV
  #          DRC removed with no PLHIV estimates for FY21; Regional programs also removed
  #          Nigeria removed due to discrepencies between TX_CURR_SUBNAT and TX_CURR
  #          Source: FY21Q1i NAT_SUBNAT SD") +
  si_style_xgrid()

si_save("Images/2022_gr/vas_key_indicators_updated")

  


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
