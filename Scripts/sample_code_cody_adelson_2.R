# PURPOSE: Generate visualization for number of rumours by month - updated script for 2022 ARM
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Oct 6, 2022
# NOTES: Streamlining rumours by month script from Global Review 2022, which was a mess

library(tidyverse)
library(glitr)
library(scales)

df_21_23<- read.csv("~/Github/dewormr/Dataout/gwsd_21_23.txt")

month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") 
type_order<-c("human", "animal")
cases_color_order<-c(c("#D9CDC3", "#FDAC7A", "#DA3C6A", "#673ab7"))

df_rumours_21_23 <- df_21_23 %>% 
  filter(sheet %in% c("animals", "MSR_Surv", "Non_MSR_Surv", "Non_MSR_Other"),
         indicator == "rumours_total") %>% 
  mutate(type = case_when(
    sheet == "animals" ~ "animal",
    TRUE ~ "human")) %>% 
  rename("rumours" = value) %>% 
  group_by(month, type, year) %>% 
  summarise(across(c(rumours), sum, na.rm=TRUE)) 

df_cases <- 
  tibble::tribble(
    ~month,    ~type, ~year, ~cases,
    "January",  "human", 2021L,     0L,
    "February",  "human", 2021L,     0L,
    "March",  "human", 2021L,     0L,
    "April",  "human", 2021L,     0L,
    "May",  "human", 2021L,     0L,
    "June",  "human", 2021L,     0L,
    "July",  "human", 2021L,     2L,
    "August",  "human", 2021L,     1L,
    "September",  "human", 2021L,     0L,
    "October",  "human", 2021L,     1L,
    "November",  "human", 2021L,     0L,
    "December",  "human", 2021L,     0L,
    "January",  "human", 2022L,     0L,
    "February",  "human", 2022L,     0L,
    "March",  "human", 2022L,     0L,
    "April",  "human", 2022L,     0L,
    "May",  "human", 2022L,     0L,
    "June",  "human", 2022L,     0L,
    "July",  "human", 2022L,     1L,
    "August",  "human", 2022L,     1L,
    "September",  "human", 2022L,     2L,
    "October",  "human", 2022L,     1L,
    "November",  "human", 2022L,     0L,
    "December",  "human", 2022L,     0L,
    "January", "animal", 2020L,     0L,
    "February", "animal", 2020L,     0L,
    "March", "animal", 2020L,     0L,
    "April", "animal", 2020L,     0L,
    "May", "animal", 2020L,     0L,
    "June", "animal", 2020L,     0L,
    "July", "animal", 2020L,     0L,
    "August", "animal", 2020L,     0L,
    "September", "animal", 2020L,     0L,
    "October", "animal", 2020L,     0L,
    "November", "animal", 2020L,     0L,
    "December", "animal", 2020L,     0L,
    "January", "animal", 2021L,     0L,
    "February", "animal", 2021L,     0L,
    "March", "animal", 2021L,     0L,
    "April", "animal", 2021L,     0L,
    "May", "animal", 2021L,     0L,
    "June", "animal", 2021L,     0L,
    "July", "animal", 2021L,     0L,
    "August", "animal", 2021L,     0L,
    "September", "animal", 2021L,     0L,
    "October", "animal", 2021L,     0L,
    "November", "animal", 2021L,     0L,
    "December", "animal", 2021L,     0L,
    "January", "animal", 2022L,     0L,
    "February", "animal", 2022L,     0L,
    "March", "animal", 2022L,     0L,
    "April", "animal", 2022L,     0L,
    "May", "animal", 2022L,     0L,
    "June", "animal", 2022L,     0L,
    "July", "animal", 2022L,     0L,
    "August", "animal", 2022L,     1L,
    "September", "animal", 2022L,     0L,
    "October", "animal", 2022L,     0L,
    "November", "animal", 2022L,     0L,
    "December", "animal", 2022L,     0L,
    "January", "animal", 2023L,     0L,
    "February", "animal", 2023L,     0L,
    "March", "animal", 2023L,     0L,
    "April", "animal", 2023L,     0L,
    "May", "animal", 2023L,     0L,
    "June", "animal", 2023L,     0L,
    "July", "animal", 2023L,     0L,
    "August", "animal", 2023L,     0L,
    "September", "animal", 2023L,     0L,
    "October", "animal", 2023L,     0L,
    "November", "animal", 2023L,     0L,
    "December", "animal", 2023L,     0L,
    "January",  "human", 2023L,     0L,
    "February",  "human", 2023L,     0L,
    "March",  "human", 2023L,     0L,
    "April",  "human", 2023L,     0L,
    "May",  "human", 2023L,     0L,
    "June",  "human", 2023L,     0L,
    "July",  "human", 2023L,     0L,
    "August",  "human", 2023L,     1L,
    "September",  "human", 2023L,     0L,
    "October",  "human", 2023L,     0L,
    "November",  "human", 2023L,     0L,
    "December",  "human", 2023L,     0L) %>% 
  mutate(year=as.character(year))

df_rumours <- df_rumours_21_23 %>% 
  mutate(year = as.character(year)) %>% 
  mutate(month_short = substr(month,1,1),
         rumours_lab=paste((round(rumours/1000, digits=1)), "k", sep="")) %>% 
  left_join(df_cases) %>% 
  mutate(month=fct_relevel(month, month_order),
         type=fct_relevel(type, type_order),
         cases=as.numeric(cases),
         cases_color=case_when(is.na(cases) | cases==0 ~ "#D9CDC3",
                               cases == 1 ~ "#FDAC7A",
                               cases == 2 ~ "#DA3C6A",
                               cases == 3 ~ "#673ab7",
                               TRUE ~ "#A90773"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A", "#673ab7")),
         month=fct_relevel(month, month_order),
         type=fct_relevel(type, type_order))

df_rumours %>% 
  ggplot(aes(month, rumours, group=year, color=type))+
  geom_area(data=subset(df_rumours, type=="animal"), color=moody_blue, fill=moody_blue_light, alpha = .4, na.rm=TRUE)+
  geom_area(data=subset(df_rumours, type=="human"), color=genoa, fill=genoa_light, alpha = .4, na.rm=TRUE)+
  geom_vline(xintercept = c(6.5, 10.5), color="white")+
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
  scale_color_manual(values= c("#D9CDC3", "#FDAC7A", "#DA3C6A","#673ab7", genoa, moody_blue)) 

si_save("Images/2023_arm/rumours_monthly")

