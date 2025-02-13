#Scratch

df_21_22 %>% 
  filter(sheet=="animals",
         indicator == "rumours_24") %>% 
  View()


df %>% 
  filter(indicator=="rumours_total",
         sheet %in% c("MSR_Surv", "RPIF", "Non_MSR_Surv"),
         month!="Cumulative") %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))
  

df %>% 
  filter(sheet=="animals",
         indicator %in% c("rumours_total", "rumours_invest_24")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>%
  mutate(percent=rumours_invest_24/rumours_total) %>% 
  View()

df %>% 
  filter(indicator=="report_expected",
         vas=="1",
         month=="December") %>% 
  group_by(county) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  View()

df %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         month!="Cumulative",
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))

df %>% 
  glimpse()

df %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         month!="Cumulative",
         sheet %in% c("animals")) %>% 
  group_by(indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE))
