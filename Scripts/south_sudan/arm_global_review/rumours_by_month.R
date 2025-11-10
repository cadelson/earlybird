# PURPOSE: Generate visualization for number of rumours by month - updated script for 2022 ARM
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Oct 6, 2022
# NOTES: Streamlining rumours by month script from Global Review 2022, which was a mess
# This script does not use showtext package. If showtext is loaded font size will be too small

pacman::p_load(tidyverse, glitr, scales, gwepr
               #showtext, extrafont, systemfonts
               )
#showtext_auto()


month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") 
type_order<-c("human", "animal")
cases_color_order<-c(c("#D9CDC3", "#FDAC7A", "#DA3C6A", "#673ab7"))
current_year <- 2025

# font_add(family = "Source Sans Pro", 
#          regular = "~/Github/Source_Sans_Pro/SourceSansPro-Regular.ttf", 
#          bold = "~/Github/Source_Sans_Pro/SourceSansPro-Bold.ttf", 
#          italic = "~/Github/Source_Sans_Pro/SourceSansPro-Italic.ttf")

# df_rumours_20 <-
#   tibble::tribble(
#          ~month,    ~type, ~year, ~rumours,
#       "January",  "human", 2020L,    5557L,
#      "February",  "human", 2020L,    4754L,
#         "March",  "human", 2020L,    4064L,
#         "April",  "human", 2020L,    4340L,
#           "May",  "human", 2020L,    5187L,
#          "June",  "human", 2020L,    5728L,
#          "July",  "human", 2020L,    6210L,
#        "August",  "human", 2020L,    6122L,
#     "September",  "human", 2020L,    4202L,
#       "October",  "human", 2020L,    4785L,
#      "November",  "human", 2020L,    4087L,
#      "December",  "human", 2020L,    3015L,
#       "January", "animal", 2020L,      42L,
#      "February", "animal", 2020L,      44L,
#         "March", "animal", 2020L,      43L,
#         "April", "animal", 2020L,      49L,
#           "May", "animal", 2020L,      42L,
#          "June", "animal", 2020L,      89L,
#          "July", "animal", 2020L,      48L,
#        "August", "animal", 2020L,      54L,
#     "September", "animal", 2020L,      32L,
#       "October", "animal", 2020L,      54L,
#      "November", "animal", 2020L,      33L,
#      "December", "animal", 2020L,      40L
#     )

df_21_25 <- read.csv("~/Github/dewormr/Dataout/gwsd_21_25.txt")

df_rumours_21_25 <- df_21_25 %>% 
  filter(sheet %in% c("animals", "MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other"),
         indicator == "rumours_total") %>% 
  mutate(type = case_when(
    sheet == "animals" ~ "animal",
    TRUE ~ "human")) %>% 
  dplyr::rename("rumours" = value) %>% 
  group_by(month, type, year) %>% 
  summarise(across(c(rumours), sum, na.rm=TRUE)) %>% 
  ungroup() # %>% 
  #bind_rows(df_rumours_20)

gwep_load("cases_infections")

df_cases <- df_cases_infections %>% 
  dplyr::rename(month = month_emerge,
         year = year_event) %>% 
  filter(country == "South Sudan",
         year > current_year - 5,
         is.na(first_worm) | first_worm == 1) %>%
  distinct(host, month, year, id_event) %>% 
  #filter(year == "2022", host == "Human") %>% View()
  mutate(type = case_when(
    host == "Human" ~ "human",
    TRUE ~ "animal")) %>% 
  group_by(month, type, year) %>% 
  #distinct(id_event) %>% 
  tally() %>% 
  ungroup() %>% 
  complete(year = current_year-5:current_year, month = 1:12, type, fill = list(n=0)) %>%
  dplyr::rename(cases = n) %>% 
  mutate(month = month.name[as.numeric(month)])


df_rumours <- df_rumours_21_25 %>% 
  #mutate(year = as.character(year)) %>% 
  mutate(month_short = substr(month,1,1),
         rumours_lab=paste((round(rumours/1000, digits=1)), "k", sep="")) %>% 
  left_join(df_cases) %>% 
  mutate(month=fct_relevel(month, month_order),
         type=fct_relevel(type, type_order),
         cases=as.numeric(cases),
         # cases=case_when(type=="animal" ~ 0,
         #                 type == "animal" & month == "August" & year == "2022" ~ 1,
         #                 is.na(cases) ~ 0,
         #                 TRUE~cases),
         cases_color=case_when(is.na(cases) | cases==0 ~ "#D9CDC3",
                               cases == 1 ~ "#FDAC7A",
                               cases == 2 ~ "#DA3C6A",
                               cases == 3 ~ "#A90773",
                               TRUE ~ "#673ab7"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A", "#A90773", "#673ab7")),
         #order doesn't work unless it's included twice, unsure why this is
         month=fct_relevel(month, month_order),
         type=fct_relevel(type, type_order))
  #write_xlsx(file.path(data_out, "rumours_for_global_review.xlsx"))

df_rumours %>% 
  ggplot(aes(month, rumours, group=year, color=type))+
  geom_area(data=subset(df_rumours, type=="animal"), color=moody_blue, fill=moody_blue_light, alpha = .4, na.rm=TRUE)+
  geom_area(data=subset(df_rumours, type=="human"), color=genoa, fill=genoa_light, alpha = .4, na.rm=TRUE)+
  geom_vline(xintercept = c(5.5, 11.5), color="white")+
  geom_rug(aes(color = factor(cases_color)), size=3, sides="b", na.rm = TRUE) +
  facet_grid(type~year, scales="free_y", labeller = labeller(type = function(x) {rep("", length(x))}))+
  scale_y_continuous(labels=comma)+
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(panel.spacing.x = unit(.5, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        strip.text.x = element_text(size = 16),
        legend.position="none")+
  scale_color_identity()
  #scale_color_manual(values= c("#D9CDC3", "#FDAC7A", "#DA3C6A", "#A90773", "#673ab7", genoa, moody_blue)) 

si_save("Images/south_sudan/arm_gr/2025_arm/rumours_monthly", width = 9)


# For labels, calculate total number of rumours by host/year

df_21_25 %>% 
  filter(indicator %in% c("rumours_total", "rumours_invest_24", "suspects_total"),
         sheet == "animals"
         #sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other")
         #sheet %in% c("MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other", "animals")
         ) %>% 
  group_by(year, indicator) %>% 
  summarise(across(c(value), sum, na.rm = TRUE))
  glimpse()
         