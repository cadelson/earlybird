df_casesweep<-
tibble::tribble(
         ~month,        ~region,  ~value,
          "Feb",         "Budi",   62705,
          "Mar",           "dm",  528106,
  "Jan/May/Sep", "Kapoeta East",   16963,
          "Jul",         "none",       0,
          "Apr",         "none",       0,
          "Jun",         "none",       0,
          "Aug",         "none",       0,
          "Oct",         "none",       0
  )


month_order_cs<- c("Jan/May/Sep", "Feb", "Mar", "Apr", "Jun", "Jul", "Aug", "Oct") 

df_casesweep %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order_cs)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=golden_sand, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 550000, 100000), limits=c(0, 550000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cs.png",
       height = 10,
       width = 19)
