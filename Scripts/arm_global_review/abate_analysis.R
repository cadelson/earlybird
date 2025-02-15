# PURPOSE: Generate visualization for abate eligible and treated
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 1, 2021
# NOTES: February 2022 update with dewormr dataset for Global Review
# Updated November 22 2022 for ARM

library(tidyverse)
library(scales)
library(glitr)

df_22_24 <- read.csv("~/Github/dewormr/Dataout/gwsd_22_24.txt")

data_out <- "~/Github/earlybird/Dataout"
month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")
status_order<-c("Targeted", "Eligible", "Treated")
status_color_order<-c("#03045e", "#0077b6", "#00b4d8")
current_year <- "2024"

# Water sources abated, eligible and treated

df_abate <- df_22_24 %>% 
  filter(indicator %in% c("abate_targeted", "abate_eligible", "abate_treated"),
         year == current_year) %>% 
  mutate(indicator=case_when(
    indicator=="abate_targeted" ~ "Targeted",
    indicator=="abate_eligible" ~ "Eligible",
    indicator=="abate_treated" ~ "Treated"),
    month=match(month, month.name),
    month=month.abb[month]) %>% 
  group_by(month, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE), .by = c(month, indicator)) %>% 
  # summarise(across(c(value), sum, na.rm=TRUE), .by = indicator) %>% 
  View()
  ungroup() %>% 
  mutate(month=fct_relevel(month, month_order),
         status=fct_relevel(indicator, status_order),
         status_color=case_when(
           status=="Targeted" ~ "#03045e",
           status=="Eligible" ~ "#0077b6",
           status == "Treated" ~ "#00b4d8"),
         status_color=fct_relevel(status_color, status_color_order))

df_abate %>% 
  #filter(indicator != "Targeted") %>% 
  ggplot(aes(x=month, y=value, fill=status_color))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_y_continuous(labels=comma, breaks=seq(0, 30000, 10000), limits=c(0,30000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust = 4, family = "Source Sans Pro", size = 18),
        axis.text.y  = element_text(family = "Source Sans Pro", size = 18),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()+
  scale_color_identity() +
  geom_text(aes(label = comma(value)), position=position_dodge2(width=.9, preserve="single"), size = 3, vjust = -.3, family = "Source Sans Pro", fontface = "italic") 

#df_abate %>% group_by(status) %>% summarise(across(c(value), sum, na.rm=TRUE)) %>% View()

si_save("Images/2024_arm/abate_updated", width = 11)  

# Total Abate Used

df_22_24 %>% 
  filter(indicator == "abate_used",
         year == current_year) %>% 
  # summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  # View()
  summarise(across(c(value), sum, na.rm=TRUE), .by = month) %>% 
  mutate(value = value/1000,
         month=match(month, month.name),
         month=month.abb[month],
         month = fct_relevel(month, month_order)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(group=1), size=.5, colour=denim)+
  geom_point(fill = "white", shape = 21, size = 3, colour = denim, stroke = 3)+
  geom_text(aes(label=comma(round(value, .1))), size=6, vjust=-1, colour="#505050")+
  scale_y_continuous(breaks=seq(0, 1, 1), limits=c(0, 5000))+
  labs(X = NULL, y = NULL)+
  si_style_nolines()+
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())

si_save("Images/2024_arm/abate_used", height = 2, width = 13)  
# ggsave("Images/2024_arm/abate_used_test.png",
#        height = 5.625,
#        width = 15)
