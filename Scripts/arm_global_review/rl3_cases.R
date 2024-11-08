df_rl3_cases<-
tibble::tribble(
     ~year, ~value,            ~rl,
     2018L,     5L,            "3",
     2018L,     5L, "other levels",
     2019L,     4L,            "3",
     2019L,     0L, "other levels",
     2020L,     1L,            "3",
     2020L,     0L, "other levels",
     2021L,     2L,            "3",
     2021L,     2L, "other levels",
     2022L,     1L,            "3",
     2022L,     4L, "other levels",
     )


# month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
rl_order<-c("Targeted", "Eligible and Treated")
rl_color_order<-c(grey40k, scooter)


 df_rl3_cases%>% 
  mutate(value = as.numeric(value)) %>%
  mutate(rl_color=case_when(
           rl=="3" ~scooter,
           rl=="other levels" ~ grey40k),
         rl_color=fct_relevel(rl_color, rl_color_order)) %>% 
  #ggplot(aes(x=month, y=value, fill=scooter_med))+
  ggplot(aes(x=year, y=value, fill=rl_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE)+
  geom_hline(aes(yintercept=1), colour="white", size=1)+
  geom_hline(aes(yintercept=2), colour="white", size=1)+
  geom_hline(aes(yintercept=3), colour="white", size=1)+
  geom_hline(aes(yintercept=4), colour="white", size=1)+
  geom_hline(aes(yintercept=5), colour="white", size=1)+
  geom_hline(aes(yintercept=6), colour="white", size=1)+
  geom_hline(aes(yintercept=7), colour="white", size=1)+
  geom_hline(aes(yintercept=8), colour="white", size=1)+
  geom_hline(aes(yintercept=9), colour="white", size=1)+
  geom_hline(aes(yintercept=10), colour="white", size=1)+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=12, family="Source Sans Pro SemiBold")+
  scale_y_continuous(breaks=seq(0, 10, 2))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=24, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


ggsave("Images/2022_arm/risk_level_3/cases_rl.png",
       height = 5,
       width = 10)
