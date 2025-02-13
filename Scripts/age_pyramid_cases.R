# PURPOSE: Generate population pyramid visualization for total cases from 2012 to 2022 of
# total cases by age and sex
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Apr 11, 2022
# NOTES: To delete

df_cases_path <- "~/Github/earlybird/Data/2007_2022_patient_data.xlsx"
df_cases<- read.xlsx(df_cases_path)
`%notin%` <- Negate(`%in%`)

df_cases %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  mutate(sex=toupper(sex)) %>% 
  filter(!is.na(age),
         !is.na(sex))%>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         fill_color = case_when(
           sex =="M" & age_cat %in% c("5-9", "10-14") ~ genoa,
           sex =="F"& age_cat %in% c("5-9", "10-14", "15-19", "25-29") ~ moody_blue,
           sex =="F"& age_cat %notin% c("5-9", "10-14", "15-19", "25-29") ~ "#cfc3ff",
           sex =="M"& age_cat %notin% c("5-9", "10-14") ~ "#89dacb")) %>%
  mutate("total_cases" = sum(n, na.rm = TRUE)) %>%
  # mutate("n_total" = sum(n, na.rm=TRUE),
  #        county_label = glue("{county_of_detection} (n={comma(n_total)})")) %>% 
  # View()
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>%
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_male, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = percent(label_female, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  #geom_text(aes(label=percent(value, .1)), size=6, vjust=-1, colour="#505050")+
  si_style_nolines()+
  scale_x_continuous(limits=c(-.15, .15),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper("confirmed cases 2007-2022 (n=16,009)"),
       x = NULL, y= NULL)

si_save("Graphics/cases_pop_pyramid_share.png")


df_cases %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  mutate(sex=toupper(sex)) %>% 
  filter(!is.na(age),
         !is.na(sex))%>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         fill_color = case_when(
           sex =="F"& age_cat %in% c("5-9", "20-24", "25-29", "30-34") ~ moody_blue,
           sex =="F"& age_cat %notin% c("5-9", "20-24", "25-29", "30-34") ~ "#cfc3ff",
           sex =="M" ~ "#89dacb")) %>% 
  mutate("total_cases" = sum(n, na.rm = TRUE)) %>%
  # mutate("n_total" = sum(n, na.rm=TRUE),
  #        county_label = glue("{county_of_detection} (n={comma(n_total)})")) %>% 
  # View()
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>%
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_male, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = percent(label_female, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  #geom_text(aes(label=percent(value, .1)), size=6, vjust=-1, colour="#505050")+
  si_style_nolines()+
  scale_x_continuous(limits=c(-.15, .15),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = .45))+
  labs(title = toupper("confirmed cases 2007-2022 (n=16,009)"),
       x = NULL, y= NULL)

si_save("Graphics/cases_pop_pyramid_share.png", width=8, height=7)


