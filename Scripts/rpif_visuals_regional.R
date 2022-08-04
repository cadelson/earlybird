# PURPOSE: Create a function to generate visualization of RPIF and case data for level one areas
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: May 18, 2022
# NOTES:

df_rpif_path <- "~/Github/dracunculiaviz/Data/rpif_february_22_v1.xlsx"
df_rpif<- read.xlsx(df_rpif_path)
df_cases_path <- "~/Github/dracunculiaviz/Data/2007_2022_patient_data.xlsx"
df_cases<- read.xlsx(df_cases_path)
`%notin%` <- Negate(`%in%`)
highlight_thresh <- .065


#============== RPIF Regional Analysis
df_rpif %>% get_rpif_pyramid("Tonj East")

get_rpif_pyramid <- function(mydf, county) {
df_rpif <- df_rpif %>% 
  clean_names() %>% 
  mutate(county_of_detection=str_to_title(sub("_", " ", county_of_detection)),
         date=as.Date(paste('01', month, year), format='%d %b %Y'),
         date=format(max(date), format="%B %Y")) %>%
  filter(age!="NA",
         county_of_detection=={{county}}) %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex, date) %>% 
  tally(sort=TRUE) %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         total_rumours=sum(n)) %>%
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>%
  mutate(fill_color=case_when(
    sex=="M" & n>highlight_thresh ~ genoa,
    sex=="M" & n<highlight_thresh ~ "#89dacb",
    sex=="F" & n>highlight_thresh ~ moody_blue,
    sex=="F" & n<highlight_thresh ~ "#cfc3ff")) %>% 
  filter(!is.na(fill_color))

df_rpif %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_male, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = percent(label_female, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  si_style_nolines()+
  scale_x_continuous(limits=c(-.2, .2),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper(glue("{county} total guinea worm rumours through {max(df_rpif$date)} (n={df_rpif$total_rumours})")),
       x = NULL, y= NULL)
  
si_save(glue("Graphics/rpif_rumours_pop_pyramid_share_{county}.png"))
}

#============== Case Regional Analysis
df_cases %>% get_case_pyramid("Jur River")

get_case_pyramid <- function(mydf, county) {
df_cases <- df_cases %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  mutate(county=str_to_title(county),
         sex=toupper(sex)) %>% 
  filter(!is.na(age),
         !is.na(sex),
         county=={{county}})%>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n)) %>% 
  mutate("total_cases" = sum(n, na.rm = TRUE)) %>%
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>% 
  mutate(fill_color=case_when(
    sex=="M" & n>highlight_thresh ~ genoa,
    sex=="M" & n<highlight_thresh ~ "#89dacb",
    sex=="F" & n>highlight_thresh ~ moody_blue,
    sex=="F" & n<highlight_thresh ~ "#cfc3ff"))
  
df_cases %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_male, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = percent(label_female, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  si_style_nolines()+
  scale_x_continuous(limits=c(-.15, .15),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper(glue("{county} confirmed cases 2007-2022 (n={df_cases$total_cases})")),
       x = NULL, y= NULL)

si_save(glue("Graphics/cases_pop_pyramid_share_{county}.png"))
}

#============== Rumour/Case Comparison

df_rpif %>% compare_rumours_cases("Awerial")

compare_rumours_cases <- function(mydf, county) {

df_rpif_facet <- df_rpif %>% 
  clean_names() %>% 
  mutate(county_of_detection=str_to_title(sub("_", " ", county_of_detection)),
         date=as.Date(paste('01', month, year), format='%d %b %Y'),
         date=format(max(date), format="%B %Y")) %>%
  filter(age!="NA",
         county_of_detection=={{county}}) %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex, date) %>% 
  tally(sort=TRUE) %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         total_rumours=comma(sum(n))) %>%
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>%
  mutate(
    fill_color=case_when(
      sex=="M" & n>highlight_thresh ~ genoa,
      sex=="M" & n<highlight_thresh ~ "#89dacb",
      sex=="F" & n>highlight_thresh ~ moody_blue,
      sex=="F" & n<highlight_thresh ~ "#cfc3ff"),
    "type"="Rumours") %>% 
  filter(!is.na(fill_color))

df_cases_facet <- df_cases %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  mutate(county=str_to_title(county),
         county=case_when(
           county=="Maper" ~ "Rumbek North",
           county=="Wuror" ~ "Uror",
           TRUE ~ county),
         sex=toupper(sex)) %>% 
  filter(!is.na(age),
         !is.na(sex),
         county=={{county}})%>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n)) %>% 
  mutate("total_cases" = comma(sum(n, na.rm = TRUE))) %>%
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>% 
  mutate("type"="Cases")

df_case_rumour_dif <- bind_rows(df_cases_facet, df_rpif_facet) %>% 
  pivot_wider(names_from = c(type), values_from = n) %>% 
  group_by(age_cat, sex) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(n_diff=Cases-Rumours) %>% 
  select(age_cat, sex, n_diff)

df_cases_rumours <- bind_rows(df_cases_facet, df_rpif_facet) %>% 
  left_join(df_case_rumour_dif) %>% 
  mutate(
    fill_color=case_when(
      sex=="M" & n_diff>.02 ~ genoa,
      sex=="M" & n_diff<.02  ~ "#89dacb",
      sex=="F" & n_diff>.02  ~ moody_blue,
      sex=="F" & n_diff<.02 ~ "#cfc3ff")) %>%
  mutate(
    type=case_when(
      type == "Cases" ~ glue("{type} 2007-2022 (n={total_cases})"),
      type == "Rumours" ~ glue("{type} through February 2022 (n={total_rumours})"))) 

df_cases_rumours %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  facet_wrap(~type, nrow=1) +
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_male, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = percent(label_female, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  si_style_nolines()+
  scale_x_continuous(limits=c(-.2, .2),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper(glue("{county} underrepresented demographics")),
       x = NULL, y= NULL)
  
  si_save(glue("Graphics/cases_rumours_comparison_{county}.png"))
}

df_rpif_facet <- df_rpif %>% 
  clean_names() %>% 
  mutate(county_of_detection=str_to_title(sub("_", " ", county_of_detection)),
         date=as.Date(paste('01', month, year), format='%d %b %Y'),
         date=format(max(date), format="%B %Y")) %>%
  filter(age!="NA",
         county_of_detection=="Awerial") %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex, date) %>% 
  tally(sort=TRUE) %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         total_rumours=sum(n)) %>%
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>%
  mutate(
    fill_color=case_when(
      sex=="M" & n>highlight_thresh ~ genoa,
      sex=="M" & n<highlight_thresh ~ "#89dacb",
      sex=="F" & n>highlight_thresh ~ moody_blue,
      sex=="F" & n<highlight_thresh ~ "#cfc3ff"),
    "type"="Rumours") %>% 
  filter(!is.na(fill_color))

df_cases_facet <- df_cases %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  mutate(county=str_to_title(county),
         county=case_when(
           county=="Maper" ~ "Rumbek North",
           county=="Wuror" ~ "Uror",
           TRUE ~ county),
         sex=toupper(sex)) %>% 
  filter(!is.na(age),
         !is.na(sex),
         county=="Awerial")%>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n)) %>% 
  mutate("total_cases" = sum(n, na.rm = TRUE)) %>%
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>% 
  mutate("type"="Cases") %>% 
  View()

df_case_rumour_dif <- bind_rows(df_cases_facet, df_rpif_facet) %>% 
  pivot_wider(names_from = c(type), values_from = n) %>% 
  group_by(age_cat, sex) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(n_diff=Cases-Rumours) %>% 
  select(age_cat, sex, n_diff)

df_cases_rumours <- bind_rows(df_cases_facet, df_rpif_facet) %>% 
  left_join(df_case_rumour_dif) %>% 
  mutate(
    fill_color=case_when(
      sex=="M" & n_diff>.02 ~ genoa,
      sex=="M" & n_diff<.02  ~ "#89dacb",
      sex=="F" & n_diff>.02  ~ moody_blue,
      sex=="F" & n_diff<.02 ~ "#cfc3ff")) %>%
  mutate(
    type=case_when(
      type == "Cases" ~ glue("{type} 2007-2022 (n={total_cases})"),
      type == "Rumours" ~ glue("{type} through February 2022 (n={total_rumours})"))) 

df_cases_rumours %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  facet_wrap(~type, nrow=1) +
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_male, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = percent(label_female, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  si_style_nolines()+
  scale_x_continuous(limits=c(-.15, .15),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper(glue("{county} underrepresented demographics")),
       x = NULL, y= NULL)

si_save(glue("Graphics/cases_rumours_comparison_{county}.png"))