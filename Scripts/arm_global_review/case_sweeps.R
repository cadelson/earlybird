# PURPOSE: Case sweep visuals for ARM
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: November 24, 2022

df_case_sweep <-
  tibble::tribble(
        ~county,     ~type, ~value,
     "Pochalla",  "people",  8695L,
     "Pochalla", "animals",   899L,
        "Akobo",  "people",  9715L,
        "Akobo", "animals",  1828L
     )

type_order<-c("people", "animals")
type_order<-c("animals", "people")
type_color_order <- c("#8980cb", "#c43d4d")
type_color_order <- c("#c43d4d", "#8980cb")

df_case_sweep %>% 
  mutate(type = fct_relevel(type, type_order),
         type_color = case_when(
           type == "people" ~ old_rose,
           type == "animals" ~ moody_blue),
         type_color = fct_relevel(type_color, type_color_order)) %>% 
  ggplot(aes(x=county, y=value, fill=type_color))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = comma(value)), position=position_dodge2(width=.9, preserve="single"), size = 6, vjust = -.3, family = "Source Sans Pro", fontface = "italic") +
  scale_y_continuous(labels=comma, breaks=seq(0, 10000, 2500), limits=c(0, 10000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(size = 22, vjust = 0.5, family = "Source Sans Pro"),
        axis.text.y  = element_text(size = 16, vjust = 0.5, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  #scale_color_identity()+
  scale_fill_identity()

si_save("Images/2022_arm/risk_level_3/case_sweeps") 
