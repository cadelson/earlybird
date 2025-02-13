# PURPOSE: Generate visualization for number of rumours, invest <24 hours, suspects by county for ARM regional presentations
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: November 23, 2022

library(tidyverse)
library(janitor)
library(AzureStor)
library(glitr)
library(scales)

df_22_24 <- read.csv("~/Github/dewormr/Dataout/gwsd_22_24.txt")

# Grab and load recent cases from Azure
bl_endp_key <- storage_endpoint(Sys.getenv("CC_GW_AZURE_BLOB_URL"),
                                key = Sys.getenv("CC_GW_AZURE_KEY"))

cont <- storage_container(bl_endp_key, "gwep")

# List all files in the directory
files_list <- list_storage_files(cont, "DEVELOPMENT/rds files")

rds_file_path <- "DEVELOPMENT/rds files/global_patient_animal_dataset.rds"

temp_rds <- tempfile(fileext = ".rds")
storage_download(cont, src = rds_file_path, dest = temp_rds, overwrite = TRUE)

df_cases_global <- readRDS(temp_rds) %>% 
  mutate(snu2_detect = case_when(
    snu2_detect == "Lopa/Lafon" ~ "Lafon",
    TRUE ~ snu2_detect))

`%notin%` <- Negate(`%in%`)

current_year <- df_22_24 %>% 
  summarise(max_year = max(year)) %>% 
  pull(max_year)

current_month <- df_22_24 %>% 
  filter(year == current_year) %>% 
  mutate(month = match(month, month.name)) %>% 
  summarise(max_month = max(month)) %>% 
  mutate(max_month = month.name[as.numeric(max_month)]) %>%
  pull(max_month)

month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 

# Munge data for humans and animals
df_viz <- df_22_24 %>% 
  mutate(county = case_when(county == "Lopa/Lafon" ~ "Lafon", 
                            TRUE ~ county)) %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other", "animals"),
         year %in% c(current_year, current_year - 1)) %>%
  mutate(sheet = case_when(
    sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other") ~ "human",
    TRUE ~ "animals"),
    county = case_when(county == "Lopa/Lafon" ~ "Lafon", 
                       TRUE ~ county),
    month=match(month, month.name),
    month=month.abb[month]) %>% 
  group_by(month, county, year, indicator, sheet) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  ungroup() %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  ungroup() %>% 
  mutate(rumours_invest_24 = rumours_invest_24/rumours_total,
         rumours_ns = rumours_total-suspects_total,
         month = fct_relevel(month, month_order)) %>% 
  rename("Rumours"=rumours_ns,
         "Rumours Investigated <24 Hours"=rumours_invest_24,
         "Suspects"=suspects_total) %>%
  pivot_longer(c(Rumours, `Rumours Investigated <24 Hours`, Suspects), names_to = "indicator") %>%
  complete(year = c(current_year - 1, current_year), month, indicator, county, sheet, fill = list(value = 0, 
                                                                                                  rumours_total = 0)) %>% 
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

county_selection <- "Lafon"

### Humans viz

df_cases_munge <- df_cases_global %>% 
  filter(#snu2_detect == county_selection,
         year_event %in% c(current_year, current_year-1),
         country == "South Sudan",
         worm_number == 1) %>%
  mutate(host = case_when(
    host != "Human" ~ "animal",
    TRUE ~ "human")) %>% 
  distinct(host, year_event, id_event, month_emerge, snu2_detect) %>% 
  group_by(year_event, month_emerge, host, snu2_detect) %>% 
  tally() %>% 
  pivot_wider(names_from = host, values_from = n) %>% 
  ungroup() %>%
  complete(snu2_detect, year_event = c(current_year - 1, current_year), month_emerge = 1:12, fill = list(animal = 0,
                                                                                                  human = 0)) %>%
  mutate(month_emerge = factor(month.abb[month_emerge], levels = month_order),
         across(c("human", "animal"), ~ case_when(
           is.na(.) | . == 0 ~ "#D9CDC3",
           . == 1 ~ "#FDAC7A",
           . == 2 ~ "#DA3C6A",
           . == 3 ~ "#A90773",
           TRUE ~ "#673ab7")),
         across(c("human", "animal"), ~ factor(., levels = c("#D9CDC3", "#FDAC7A", "#DA3C6A", "#A90773", "#673ab7")))) %>% 
  filter(snu2_detect == county_selection)


df_viz_human_munge <- df_viz %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         sheet != "animals",
         county == county_selection) %>% 
  left_join(df_cases_munge, by = c("year" = "year_event",
                                   "month" = "month_emerge")) %>% 
  mutate(across(c("human", "animal"), ~ case_when(
    is.na(.) ~ "#D9CDC3",
    TRUE ~ .)))

human_max <- df_viz_human_munge %>% 
  summarise(max_rumours = ceiling(max(rumours_total) / 1000) * 1000) %>% 
  pull(max_rumours)

df_viz_human_munge %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  geom_text(aes(label=value_label), size=13/.pt, color="white", family="Source Sans Pro", vjust = -.3)+
  geom_text(aes(x=month, y = rumours_total, label = comma(rumours_total)), size = 13/.pt, color="#505050", family="Source Sans Pro", vjust=-.3)+
  si_style_ygrid()+
  facet_wrap(~year) +
  geom_rug(aes(color = human), size = 3, sides = "b", na.rm = TRUE) +
  scale_fill_identity()+
  scale_color_identity()+
  #scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, family = "Source Sans Pro" ),
        strip.text = element_text(size = 20, family = "Source Sans Pro Semibold"))+
  scale_y_continuous(breaks = seq(0, human_max, human_max/4), labels=comma, limits = c(0, human_max))


si_save(paste0("Images/2024_arm/county_presentations/rumours/rumours_suspects_", 
               make_clean_names(county_selection), "_", current_year - 1, "_", current_year, ".png"), 
        width = 15)

# Custom rounding function
custom_percent <- function(x) {
  ifelse(x == 1, "100%", percent(x, accuracy = 0.1))
}

# Investigated in 24 hours
df_viz %>% 
  filter(indicator == "Rumours Investigated <24 Hours",
         county == county_selection,
         sheet != "animals") %>%
  select(month, value, year) %>% 
  mutate(value = as.numeric(value)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(group = 1), size = .5, colour = genoa) +
  geom_point(fill = "white", shape = 21, size = 3, colour = genoa, stroke = 3) +
  facet_wrap(~year) +
  geom_text(aes(label = custom_percent(value)), size = 5, vjust = -1, colour = "#505050") +
  scale_y_continuous(breaks = seq(0, 1, 1), limits = c(.7, 1.03)) +
  labs(x = NULL, y = NULL) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 20, family = "Source Sans Pro Semibold"))

si_save(paste0("Images/2024_arm/county_presentations/rumours/invest_24_", 
               make_clean_names(county_selection), "_", current_year - 1, "_", current_year, ".png"), 
        width = 22)

### Animals viz

df_viz_animal_munge <- df_viz %>% 
  filter(indicator %in% c("Rumours", "Suspects", rumours_total),
         sheet == "animals",
         county == county_selection) %>% 
  left_join(df_cases_munge, by = c("year" = "year_event",
                                   "month" = "month_emerge")) %>% 
  mutate(across(c("human", "animal"), ~ case_when(
    is.na(.) ~ "#D9CDC3",
    TRUE ~ .)))


animal_max <- df_viz_animal_munge %>% 
  summarise(max_rumours = ceiling(max(rumours_total) / 100) * 100) %>% 
  pull(max_rumours)

df_viz_animal_munge %>% 
  ggplot(aes(month, value, fill=forcats::fct_rev(indicator_fill_color)))+
  geom_bar(
    position="stack", stat="identity", show.legend=FALSE)+
  geom_text(aes(label=value_label), size = 13/.pt, color = "white", family = "Source Sans Pro", vjust = -.3)+
  geom_text(aes(x=month, y = rumours_total, label = comma(rumours_total)), size = 13/.pt, color="#505050", family="Source Sans Pro", vjust=-.3)+
  si_style_ygrid()+
  facet_wrap(~year) +
  #geom_rug(aes(color = factor(animal)), size=3, sides= "b", na.rm = TRUE) +
  geom_rug(aes(color = animal), size = 3, sides = "b", na.rm = TRUE) +
  scale_fill_identity()+
  scale_color_identity()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 14, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 14, family = "Source Sans Pro" ),
        strip.text = element_text(size = 20, family = "Source Sans Pro Semibold"))+
  scale_y_continuous(breaks = seq(0, animal_max, animal_max/4), labels=comma, limits = c(0, animal_max))


si_save(paste0("Images/2024_arm/county_presentations/rumours/rumours_suspects_animal_", 
               make_clean_names(county_selection), "_", current_year - 1, "_", current_year, ".png"), 
        width = 15)


df_viz %>% 
  filter(indicator=="Rumours Investigated <24 Hours",
         sheet == "animals",
         county == county_selection) %>%
  select(month, value, year) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(group=1), size=.5, colour= moody_blue)+
  geom_point(fill = "white", shape = 21, size = 3, colour = moody_blue, stroke = 3)+
  facet_wrap(~year) +
  #geom_text(aes(label=percent(value, .1)), size=5, vjust=-1, colour="#505050")+
  geom_text(aes(label = custom_percent(value)), size = 5, vjust = -1, colour = "#505050") +
  scale_y_continuous(breaks=seq(0, 1, 1), limits=c(.75, 1.01))+
  labs(X = NULL, y = NULL)+
  si_style_nolines()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        strip.text = element_text(size = 20, family = "Source Sans Pro Semibold"))

si_save(paste0("Images/2024_arm/county_presentations/rumours/invest_24_animal_", 
               make_clean_names(county_selection), "_", current_year - 1, "_", current_year, ".png"), 
        width = 22)


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
