# PURPOSE: Generate visualization for RPIF data
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Apr 22, 2022
# NOTES: 

df_rpif_path <- "~/Github/earlybird/Data/rpif_april_22_v1.xlsx"
df_rpif<- read.xlsx(df_rpif_path)
`%notin%` <- Negate(`%in%`)


#Total Population Pyramid with larger age buckets
df_rpif %>% 
  clean_names() %>% 
  filter(age!="NA") %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 10, 20, 30, 40, 50, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally(sort=TRUE) %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         fill_color = case_when(
           sex =="M" & age_cat %in% c("10-19", "20-29", "30-39") ~ genoa,
           sex =="F"& age_cat %in% c("10-19", "20-29", "30-39") ~ moody_blue,
           sex =="F"& age_cat %notin% c("10-19", "20-29", "30-39") ~ "#cfc3ff",
           sex =="M"& age_cat %notin% c("10-19", "20-29", "30-39") ~ "#89dacb")) %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = label_male),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = label_female),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  si_style_nolines()+
  scale_x_continuous(limits=c(-1200, 1200),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper("total guinea worm rumours through April 2022"),
       x = NULL, y= NULL)

si_save("Graphics/rpif_rumours_pop_pyramid_ten_yr.png")

#Total Population Pyramid with smaller age buckets
df_rpif %>% 
  clean_names() %>% 
  filter(age!="NA") %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally(sort=TRUE) %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         fill_color = case_when(
           sex =="M" & age_cat %in% c("10-14", "15-19") ~ genoa,
           sex =="F"& age_cat %in% c("10-14", "15-19") ~ moody_blue,
           sex =="F"& age_cat %notin% c("10-14", "15-19") ~ "#cfc3ff",
           sex =="M"& age_cat %notin% c("10-14", "15-19") ~ "#89dacb")) %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = label_male),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = label_female),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  si_style_nolines()+
  scale_x_continuous(limits=c(-800, 800),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper("total guinea worm rumours through February 2022"),
       x = NULL, y= NULL)

si_save("Graphics/rpif_rumours_pop_pyramid.png")

county_order <- c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol", "Uror", "Torit", "Lopa Lafon", "Terekeka")
axis_color <- df_rpif2$color[order(df_rpif2$age_cat)]

#Population Pyramid by county with smaller age buckets
df_rpif %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  filter(age!="NA",
         county_of_detection %notin% c("CUEIBET", "LEER", "WULU", "TONJ_SOUTH", "WAU", "KAPOETA_NORTH", "IKOTOS", "AKOBO")) %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)),
         county_of_detection=str_replace(county_of_detection, "_", " "),
         county_of_detection=str_to_title(county_of_detection)) %>% 
  group_by(age_cat, sex, county_of_detection) %>% 
  tally(sort=TRUE) %>% 
  mutate(label_female_10_19=case_when(sex == "F" & county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19")  ~ n), 
         label_male_10_19=case_when(sex == "M" & county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19")  ~ n),
         label_female_20_34=case_when(sex == "F" & county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("20-24", "25-29", "30-34")  ~ n), 
         label_male_20_34=case_when(sex == "M" & county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("20-24", "25-29", "30-34")  ~ n),
         fill_color = case_when(
           sex =="M" & county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19") ~ genoa,
           sex =="F"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19") ~ moody_blue,
           sex =="F"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-14", "15-19") ~ "#cfc3ff",
           sex =="M"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-14", "15-19") ~ "#89dacb",
           sex =="M" & county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("20-24", "25-29", "30-34") ~ genoa,
           sex =="F"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("20-24", "25-29", "30-34") ~ moody_blue,
           sex =="F"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("20-24", "25-29", "30-34") ~ "#cfc3ff",
           sex =="M"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("20-24", "25-29", "30-34") ~ "#89dacb")) %>% 
  ungroup()%>% 
  mutate(county_of_detection=fct_relevel(county_of_detection, county_order)) %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  facet_wrap(~county_of_detection)+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = label_male_10_19),
            family = "Source Sans Pro", size = 7/.pt, hjust = 1.2) +
  geom_text(aes(label = label_male_20_34),
            family = "Source Sans Pro", size = 7/.pt, hjust = 1.2) +
  geom_text(aes(label = label_female_10_19),
            family = "Source Sans Pro", size = 7/.pt, hjust = -0.2) +
  geom_text(aes(label = label_female_20_34),
            family = "Source Sans Pro", size = 7/.pt, hjust = -0.2) +
  si_style_nolines()+
  scale_x_continuous(labels = label_number_si())+
  scale_fill_identity()+
  theme(panel.spacing = unit(0.25, "lines"),
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=7))+
  labs(title = toupper("total guinea worm rumours by county through February 2022"),
       x = NULL, y= NULL)

si_save("Graphics/rpif_rumours_pop_pyramid_county_small_bucket.png")

#Population Pyramid by county with larger age buckets
df_rpif %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  filter(age!="NA",
         county_of_detection %notin% c("CUEIBET", "LEER", "WULU", "TONJ_SOUTH", "WAU", "KAPOETA_NORTH", "IKOTOS", "AKOBO")) %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 10, 20, 30, 40, 50, 60)),
         county_of_detection=str_replace(county_of_detection, "_", " "),
         county_of_detection=str_to_title(county_of_detection)) %>% 
  group_by(age_cat, sex, county_of_detection) %>% 
  tally(sort=TRUE) %>% 
  mutate(label_female_10_19=case_when(sex == "F" & county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39")  ~ n), 
         label_male_10_19=case_when(sex == "M" & county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39")  ~ n),
         label_female_20_34=case_when(sex == "F" & county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39")  ~ n), 
         label_male_20_34=case_when(sex == "M" & county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39")  ~ n),
         fill_color = case_when(
           sex =="M" & county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39") ~ genoa,
           sex =="F"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39") ~ moody_blue,
           sex =="F"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-19", "20-29", "30-39") ~ "#cfc3ff",
           sex =="M"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-19", "20-29", "30-39") ~ "#89dacb",
           sex =="M" & county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39") ~ genoa,
           sex =="F"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-19", "20-29", "30-39") ~ moody_blue,
           sex =="F"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-19", "20-29", "30-39") ~ "#cfc3ff",
           sex =="M"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-19", "20-29", "30-39") ~ "#89dacb")) %>% 
  ungroup()%>% 
  mutate(county_of_detection=fct_relevel(county_of_detection, county_order)) %>% 
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  facet_wrap(~county_of_detection)+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = label_male_10_19),
            family = "Source Sans Pro", size = 7/.pt, hjust = 1.2) +
  geom_text(aes(label = label_male_20_34),
            family = "Source Sans Pro", size = 7/.pt, hjust = 1.2) +
  geom_text(aes(label = label_female_10_19),
            family = "Source Sans Pro", size = 7/.pt, hjust = -0.2) +
  geom_text(aes(label = label_female_20_34),
            family = "Source Sans Pro", size = 7/.pt, hjust = -0.2) +
  si_style_nolines()+
  scale_x_continuous(labels = label_number_si())+
  scale_fill_identity()+
  theme(panel.spacing = unit(0.25, "lines"),
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=7))+
  labs(title = toupper("total guinea worm rumours by county through February 2022"),
       x = NULL, y= NULL)

si_save("Graphics/rpif_rumours_pop_pyramid_county_large_bucket.png")


#Total Population Pyramid SHARE with smaller age buckets
df_rpif %>% 
  clean_names() %>% 
  filter(age!="NA") %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally(sort=TRUE) %>% 
  ungroup() %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         fill_color = case_when(
           sex =="M" & age_cat %in% c("10-14", "15-19") ~ genoa,
           sex =="F"& age_cat %in% c("10-14", "15-19") ~ moody_blue,
           sex =="F"& age_cat %notin% c("10-14", "15-19") ~ "#cfc3ff",
           sex =="M"& age_cat %notin% c("10-14", "15-19") ~ "#89dacb"),
         total_rumours=sum(n)) %>%
  filter(!is.na(fill_color)) %>% 
  ungroup() %>%
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
  scale_x_continuous(breaks = seq(-.15, .15, .5),
                     limits=c(-.15, .15),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank())+
  labs(title = toupper("2022 guinea worm rumours through april (n=15,474)"),
       x = NULL, y= NULL)

si_save("Graphics/rpif_rumours_pop_pyramid_share.png")

df_rpif %>% 
  clean_names() %>% 
  filter(age!="NA") %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally(sort=TRUE) %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         fill_color = case_when(
           sex =="F"& age_cat %in% c("5-9", "20-24", "25-29", "30-34") ~ moody_blue,
           sex =="F"& age_cat %notin% c("5-9", "20-24", "25-29", "30-34") ~ "#cfc3ff",
           sex =="M" ~ "#89dacb")) %>% 
  filter(!is.na(fill_color)) %>% 
  ungroup() %>%
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
  scale_x_continuous(breaks = seq(-.15, .15, .5),
                     limits=c(-.15, .15),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = .15))+
  labs(title = toupper("2022 guinea worm rumours through april (n=15,474)"),
       x = NULL, y= NULL)

si_save("Images/rpif_rumours_pop_pyramid_share.png", width=8, height=7)

#Total Population Pyramid SHARE with larger age buckets
df_rpif %>% 
  clean_names() %>% 
  filter(age!="NA") %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 10, 20, 30, 40, 50, 60))) %>% 
  group_by(age_cat, sex) %>% 
  tally(sort=TRUE) %>% 
  mutate(label_female=case_when(sex == "F" ~ n), 
         label_male=case_when(sex == "M"  ~ n),
         fill_color = case_when(
           sex =="M" & age_cat %in% c("10-19", "20-29", "30-39") ~ genoa,
           sex =="F"& age_cat %in% c("10-19", "20-29", "30-39") ~ moody_blue,
           sex =="F"& age_cat %notin% c("10-19", "20-29", "30-39") ~ "#cfc3ff",
           sex =="M"& age_cat %notin% c("10-19", "20-29", "30-39") ~ "#89dacb")) %>% 
  filter(!is.na(fill_color)) %>% 
  ungroup() %>%
  mutate_at(vars(label_female, label_male, n), funs(./sum(n))) %>%
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_male, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  geom_text(aes(label = percent(label_female, 1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = -.2) +
  si_style_nolines()+
  scale_x_continuous(breaks = seq(-.25, .25, .5),
                     limits=c(-.25, .25),
                     labels = label_number_si())+
  scale_fill_identity()+
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = .15))+
  labs(title = toupper("total guinea worm rumours through February 2022"),
       x = NULL, y= NULL)

si_save("Graphics/rpif_rumours_pop_pyramid_ten_yr_share.png")


#Population Pyramid SHARE by county with smaller age buckets

df_rpif %>% 
  clean_names() %>% remove_empty() %>%
  mutate_all(trimws, which="both") %>% 
  filter(age!="NA",
         county_of_detection %notin% c("Cueibet", "Leer", "Wulu", "Tonj South", "Wau", "Kapoeta North", "Ikotos", "Akobo", "Kapoeta South")) %>% 
  group_by(county_of_detection) %>% 
  mutate(age=as.double(age),
         avg_age=mean(age)) %>%
  ungroup() %>% 
  mutate(age_cat=age_categories(age, breakers = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)),
         county_of_detection=str_replace(county_of_detection, "_", " "),
         county_of_detection=str_to_title(county_of_detection)) %>% 
  group_by(age_cat, sex, county_of_detection, avg_age, .drop=FALSE) %>% 
  tally(sort=TRUE) %>% 
  ungroup() %>% 
  filter(sex!="f") %>% 
  complete(age_cat, county_of_detection, sex, fill=list(n=0)) %>%
  group_by(county_of_detection) %>% 
  fill(avg_age, .direction = "up") %>% 
  ungroup() %>% 
  mutate(label_female_10_19=case_when(sex == "F" & age_cat %in% c("10-14", "15-19")  ~ n), 
         label_male_10_19=case_when(sex == "M" & age_cat %in% c("10-14", "15-19")  ~ n),
         fill_color = case_when(
           sex =="M" & county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19") ~ genoa,
           sex =="F"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19") ~ moody_blue,
           sex =="F"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-14", "15-19") ~ "#cfc3ff",
           sex =="M"& county_of_detection %in% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-14", "15-19") ~ "#89dacb",
           sex =="M" & county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19") ~ genoa,
           sex =="F"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %in% c("10-14", "15-19") ~ moody_blue,
           sex =="F"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-14", "15-19") ~ "#cfc3ff",
           sex =="M"& county_of_detection %notin% c("Rumbek Centre", "Rumbek North", "Awerial", "Jur River", "Tonj East", "Gogrial West", "Tonj North", "Nyirol") & age_cat %notin% c("10-14", "15-19") ~ "#89dacb")) %>% 
  filter(!is.na(fill_color)) %>% 
  group_by(age_cat, sex) %>% 
  mutate(n_nat=sum(n)) %>% 
  ungroup() %>% 
  mutate(n_nat=n_nat/sum(n)) %>% 
  group_by(county_of_detection) %>%
  mutate("n_total" = sum(n, na.rm=TRUE),
         county_label = glue("{county_of_detection} (n={comma(n_total)})")) %>% 
  mutate_at(vars(label_female_10_19, label_male_10_19, n), funs(./sum(n))) %>%
  ungroup() %>% 
  # arrange(avg_age) %>%
  mutate(county_label=fct_reorder(county_label, desc(avg_age))) %>%
  ggplot(aes(ifelse(sex == "M", -n, n), age_cat, fill=fill_color)) +
  geom_col(position = position_stack(reverse = TRUE))+
  geom_col(aes(ifelse(sex == "M", -n_nat, n_nat), age_cat), color="black", fill="transparent", size=.01, linetype="dashed")+
  facet_wrap(~county_label)+
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(aes(label = percent(label_female_10_19, 1)),
            family = "Source Sans Pro", size = 7/.pt, hjust = -.2) +
  geom_text(aes(label = percent(label_male_10_19, 1)),
            family = "Source Sans Pro", size = 7/.pt, hjust = 1.1) +
  si_style_nolines()+
  scale_x_continuous(labels = label_number_si(), limits=c(-.24, .24))+
  scale_fill_identity()+
  theme(panel.spacing = unit(0.01, "lines"),
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=7))+
  labs(title = toupper("guinea worm rumours by age and sex through april 2022 (n>150)"),
       x = NULL, y= NULL)

si_save("Images/rpif_rumours_april_pop_pyramid_county_small_bucket_share.png")
