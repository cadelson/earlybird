df_p_rumours<-
tibble::tribble(
      ~category, ~value, ~day,
  "prioritized", 16621L,  "a",
        "other",  5347L,  "a"
  )



category_color_order<-c(moody_blue_light, moody_blue)


df_p_rumours %>% 
  mutate(value = as.numeric(value)) %>%
  mutate(category_color=case_when(
           category=="prioritized" ~moody_blue,
           category=="other" ~moody_blue_light),
         category_color=fct_relevel(category_color, category_color_order)) %>% 
  ggplot(aes(x=day, y=value, fill=category_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE, width=.5)+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 20000, 10000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=42, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


 ggsave("rl3_prioritized_rumours.png",
       height = 10,
       width = 7)
 
priority_color<-c(genoa_light, trolley_grey_light)
region_order<-c("Jur River", "Torit", "Lopa/Lafon", "Ikotos", "Gogrial West", "Wulu", "Wau", "Other (<500 Rumours")
region_order<-c("Other (<500 Rumours)", "Wau", "Wulu", "Gogrial West", "Ikotos", "Lopa/Lafon", "Torit", "Jur River")
 
 df_rl3_rumours_county<-
   tibble::tribble(
                    ~region, ~value, ~prioritized,
                "Jur River", 11598L,        "yes",
                    "Torit",  4486L,        "yes",
               "Lopa/Lafon",  1478L,         "no",
                   "Ikotos",  1090L,         "no",
             "Gogrial West",  1082L,         "no",
                     "Wulu",   583L,         "no",
                      "Wau",   521L,        "yes",
     "Other (<500 Rumours)",  1130L,         "a"
     )

 df_rl3_rumours_county %>% 
   mutate(value = as.numeric(value)) %>%
   mutate(category_color=case_when(
     prioritized=="yes" ~moody_blue,
     prioritized=="no" ~moody_blue_light,
     TRUE ~ trolley_grey), 
     region=fct_relevel(region, region_order)) %>% 
   ggplot(aes(x=value, y=region, fill=category_color), leg)+
   geom_col(aes(fill=category_color, width=.85), show.legend = FALSE)+
   #geom_col(alpha=.9, show.legend = FALSE)+
   geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, hjust=-.05, size=9, family="Source Sans Pro SemiBold")+
   scale_x_continuous(breaks=seq(0, 15000, 5000), limits=c(0, 15000), label=comma)+
   si_style_xgrid()+
   labs(x = NULL, y = NULL)+
   theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
         axis.text.y  = element_text(vjust=0.5, size=24, family = "Source Sans Pro" ),
         strip.text = element_text(size = 38, hjust=.01, family = "Source Sans Pro"),
         axis.line.x = element_blank(),
         axis.ticks.x = element_blank())+
   scale_fill_identity()
 
 ggsave("rl3_county_rumours.png",
        height = 6,
        width = 10)
 