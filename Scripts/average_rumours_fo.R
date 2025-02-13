# PURPOSE: Visualization showing the average number of rumours per FO
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: June 4, 2022
# NOTES:

df_fo <-tibble::tribble(
          ~county, ~rumours, ~fos, ~average_rumours,
         "Ikotos",       5L,   1L,                5,
          "Akobo",      14L,   2L,                7,
     "Tonj South",      15L,   1L,               15,
     "Lopa/Lafon",      21L,   2L,             10.5,
      "Jur River",      42L,  19L,      2.210526316,
       "Terekeka",      30L,   4L,              7.5,
     "Tonj North",      76L,   6L,      12.66666667,
         "Nyirol",      73L,  11L,      6.636363636,
          "Torit",      82L,   6L,      13.66666667,
           "Uror",     289L,  29L,      9.965517241,
      "Tonj East",     266L,  38L,                7,
   "Rumbek North",     239L,  25L,             9.56,
  "Rumbek Center",     270L,  13L,      20.76923077,
        "Awerial",     293L,  21L,      13.95238095
  )


week_number <- 14

ggplot(data = df_fo) +
  geom_col(mapping = aes(x = county, y = fos), fill = "#2057a7") +
  labs(title = paste("Total Guinea Worm Rumours Week", week_number, sep = " "))+
  theme(plot.title = element_text(size=20))

ggsave(paste("images/test/rumours_week_", week_number, ".png", sep = ""),
       height = 5.625,
       width = 10)

mean_total<-round(sum(df_fo$rumours)/sum(df_fo$fos, na.rm=TRUE), digits=1)


df_fo %>% 
  mutate(county = fct_reorder(county, average_rumours),
         total_average = sum(rumours)/sum(fos),
         average_rumours = round(average_rumours, digits = 1),
         average_rumours_label=paste0(average_rumours, " (", rumours, " rumours", ", ", fos, " FOs", ")")) %>%
  #ggplot(aes(x=average_rumours, y=county, group=total_average))+
  ggplot(aes(x=average_rumours, y=reorder(county, desc(-average_rumours)), group=total_average))+
  geom_bar(stat="identity", fill = denim) +
  #geom_text(aes(label=round(average_rumours, digits = 1), hjust=-1, family = "Source Sans Pro SemiBold"))+
  geom_text(aes(label=average_rumours_label, hjust=-.05, family = "Source Sans Pro SemiBold"), size=3)+
  scale_x_continuous(breaks=seq(0, 25, 5), limits=c(0, 25))+
  si_style_xgrid()+
  labs(x=NULL, y=NULL, title = "Average Weekly Rumours Reported Per Field Officer — Week 28",
       caption= paste("program weekly average =", mean_total))+
  theme(axis.text.y = element_text(margin = margin(r = -.3, unit = "in"))) 
       
si_save("Images/fo_average_rumours.png")
            