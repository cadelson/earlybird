# PURPOSE: Generate visualization for number of cases by month for 2021 ARM
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Nov 23, 2021
# NOTES: Oct 2022 - Updated for 2022 ARM. Needs to be updated to sync with patient level dataset
# May have to add font for show text to get correct sizing

library(tidyverse)
library(glitr)
library(reshape2)
library(showtext)
showtext_auto()
library(extrafont)

month_order<- c("Jan-Mar", "April", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") 

df<-
  tibble::tribble(
    ~Year, ~`Jan-Mar`, ~April, ~May, ~Jun, ~Jul, ~Aug, ~Sep, ~Oct, ~Nov, ~Dec,
    # 2015L,           0L,   0L,   1L,   2L,   1L,   1L,   0L,   0L,   0L,
    # 2016L,           0L,   0L,   4L,   0L,   0L,   1L,   0L,   1L,   0L,
    # 2017L,           0L,   0L,   0L,   0L,   0L,   0L,   0L,   0L,   0L,
    2018L,           0L, 0L,   2L,   2L,   3L,   2L,   1L,   0L,   0L,   0L,
    2019L,           0L, 0L,   0L,   0L,   1L,   1L,   2L,   0L,   0L,   0L,
    2020L,           0L, 0L,   0L,   0L,   1L,   0L,   0L,   0L,   0L,   0L,
    2021L,           0L, 0L,   0L,   0L,   2L,   1L,   0L,   1L,   0L,   0L,
    2022L,           0L, 0L,   0L,   0L,   1L,   2L,   2L,   1L,   0L,   0L,
    2023L,           0, 0L,   0L,   0L,   0L,   1L,   1L,   0L,   1L,   0L,
    2024L,           0L, 1L,   0L,   9L,   5L,   7L,   1L,   0L,   0L,   0L,
  )


df1<-df %>% 
  mutate(sum = rowSums(across('Jan-Mar':"Dec")),
         n=("(n="),
         endn=(")"),
         year_1 = paste(Year, n, sep = ' '), 
         Year = paste(year_1, sum, endn, sep = '')) %>% 
  pivot_longer(cols="Jan-Mar":"sum", names_to="month", values_to="value") %>%
  mutate(month=fct_relevel(month, month_order)) %>% 
  select(-n, -endn, -year_1) %>% 
  filter(month!="sum") %>% 
  mutate(case_type=case_when(
    Year=="2015 (n=5)" & month=="Sep" ~ "dog",
    Year=="2022 (n=6)" & month=="Aug" ~ "dog",
    Year=="2023 (n=3)" & month=="Nov" ~ "dog",
    Year == "2024 (n=23)" ~ "subcutaneous",
    TRUE ~ "human"),
    case_color=case_when(
      case_type=="human" ~old_rose,
      case_type=="dog" ~ moody_blue,
      TRUE ~ scooter))
        
df1 %>% 
  #filter(Year %in% c("2018", "2019", "2020", "2021")) %>% 
  filter(Year!="2017 (n=0)") %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=case_color, width=.75), show.legend = FALSE)+
  #facet_wrap(~Year, ncol=1)+
  facet_grid(Year ~ ., switch = "y", scales="free_y")+
  scale_y_continuous(breaks=seq(0, 4, 2))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL,
       #caption= paste("animal infections detected from dogs")
       )+
  theme(axis.text.x  = element_text(vjust = 0.5, size = 110, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust = 0.5, size = 80, family = "Source Sans Pro" ),
        #plot.caption = element_text(vjust = -2, size = 18, family = "Source Sans Pro" ),
        strip.text = element_text(size = 130, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside")+
  geom_hline(yintercept = c(1, 2, 3, 4, 5, 6, 7, 8), size = 1, colour="white")+
  # geom_rect(data = data.frame(Year = "2015 (n=5)"), aes(xmin = 2.5, xmax = 6.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  # geom_rect(data = data.frame(Year = "2016 (n=6)"), aes(xmin = 2.5, xmax = 8.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Year = "2018 (n=10)"), aes(xmin = 2.5, xmax = 7.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Year = "2019 (n=4)"), aes(xmin = 4.5, xmax = 7.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Year = "2020 (n=1)"), aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Year = "2021 (n=4)"), aes(xmin = 4.5, xmax = 8.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Year = "2022 (n=6)"), aes(xmin = 4.5, xmax = 8.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Year = "2023 (n=3)"), aes(xmin = 5.5, xmax = 9.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Year = "2024 (n=23)"), aes(xmin = 1.5, xmax = 7.5, ymin = -Inf, ymax = Inf), alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  scale_color_identity()+
  scale_fill_identity()

ggsave("Images/2024_arm/gw_monthly_cases.png",
       height = 14,
       width = 24)

