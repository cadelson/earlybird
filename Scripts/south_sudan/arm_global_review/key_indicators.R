# PURPOSE: Generate visualization for key indicators
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 2021
# NOTES: February 2022 update with dewormr dataset for Global Review


`%notin%` <- Negate(`%in%`)
current_month <- "July"
current_year <- "2025"

pacman::p_load(tidyverse, glitr, scales)

df_21_25 <- read.csv("~/Github/dewormr/Dataout/gwsd_21_25.txt")

#MUNGE ==============================================================

df_ki<-df_21_25 %>% 
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

df_ki2<-df_21_25 %>% 
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

df_abate_eligible <- df_21_25 %>% 
  filter(indicator %in% c("abate_treated", "abate_eligible"),
         month == current_month,
         year == current_year) %>% 
  group_by(indicator, risk_level, sheet, month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(value = abate_eligible/abate_treated,
         indicator = "Eligible Water Sources Abated") %>% 
  select(-abate_eligible, -abate_treated)

df_abate <- df_21_25 %>% 
  filter(indicator %in% c("abate_targeted", "abate_eligible"),
         month == current_month,
         year == current_year) %>% 
  group_by(indicator, risk_level, sheet, month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(value = abate_eligible/abate_targeted,
         indicator = "Targeted Water Sources Eligible for Abate") %>% 
  select(-abate_eligible, -abate_targeted)

df_ki_final<-df_ki1 %>% 
  bind_rows(df_ki2, df_abate, df_abate_eligible) %>% 
  mutate(indicator=case_when(
    indicator=="staff_vv" ~ "Trained Village Volunteer",
    indicator=="hp_working" ~"Safe Water Access",
    indicator=="activity_cra" ~ "Cash Reward Activities",
    indicator=="supervision" ~ "Supervisory Visit",
    indicator=="report_received" ~ "Submitted Surveillance Reports",
    indicator=="filter_coverage" ~ "Full Cloth Filter Coverage",
    TRUE ~ indicator))

indicator_order<-c("Targeted Water Sources Eligible for Abate", "Safe Water Access", "Full Cloth Filter Coverage", "Cash Reward Activities", "Trained Village Volunteer", "Supervisory Visit", "Submitted Surveillance Reports", "Eligible Water Sources Abated")

df_ki_final %>% 
  filter(indicator != "Targeted Water Sources Eligible for Abate") %>% 
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
       subtitle = paste(current_month, "Status of VAS in Risk Level 1 (n=1,638) 2 (n=505) and 3 (n=342) Counties")) +
  theme(axis.text.y  = element_text(size = 14, family = "Source Sans Pro" ),
        axis.text.x  = element_text(size = 14, family = "Source Sans Pro" ),
        plot.subtitle = element_text(size = 16))

si_save("Images/south_sudan/arm_gr/2025_arm/vas_key_indicators_no_abate", width = 12)

# VAS Totals
df_21_25 %>% 
  filter(year == "2024",
         indicator == "report_expected",
         value == 1,
         sheet == "MSR_Surv",
         month == "November") %>% 
  distinct(state, county, payam, boma, reporting_unit, reporting_unit_code, risk_level, value) %>% 
  summarise(value = sum(value), .by = risk_level)
  
