# PURPOSE: Generate visualization for number of cases by month
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Nov 30, 2021
# NOTES:

df_year_region<-
tibble::tribble(
  ~year, ~value,      ~location,
  2006L,   286L,    "Tonj East",
  2007L,   364L,    "Tonj East",
  2008L,   418L,    "Tonj East",
  2009L,   365L,    "Tonj East",
  2010L,   265L,    "Tonj East",
  2011L,    47L,    "Tonj East",
  2012L,     0L,    "Tonj East",
  2013L,     1L,    "Tonj East",
  2014L,     0L,    "Tonj East",
  2015L,     1L,    "Tonj East",
  2016L,     2L,    "Tonj East",
  2017L,     0L,    "Tonj East",
  2018L,     0L,    "Tonj East",
  2019L,     0L,    "Tonj East",
  2020L,     1L,    "Tonj East",
  2021L,     1L,    "Tonj East",
  2006L,   106L,         "Uror",
  2007L,   157L,         "Uror",
  2008L,    17L,         "Uror",
  2009L,     2L,         "Uror",
  2010L,     0L,         "Uror",
  2011L,     0L,         "Uror",
  2012L,     0L,         "Uror",
  2013L,     1L,         "Uror",
  2014L,     0L,         "Uror",
  2015L,     0L,         "Uror",
  2016L,     0L,         "Uror",
  2017L,     0L,         "Uror",
  2018L,     0L,         "Uror",
  2019L,     0L,         "Uror",
  2020L,     0L,         "Uror",
  2021L,     1L,         "Uror",
  2006L,     6L, "Rumbek North",
  2007L,    19L, "Rumbek North",
  2008L,     4L, "Rumbek North",
  2009L,     6L, "Rumbek North",
  2010L,     0L, "Rumbek North",
  2011L,     0L, "Rumbek North",
  2012L,     0L, "Rumbek North",
  2013L,     0L, "Rumbek North",
  2014L,     0L, "Rumbek North",
  2015L,     0L, "Rumbek North",
  2016L,     0L, "Rumbek North",
  2017L,     0L, "Rumbek North",
  2018L,     4L, "Rumbek North",
  2019L,     0L, "Rumbek North",
  2020L,     0L, "Rumbek North",
  2021L,     1L, "Rumbek North",
  2006L,   435L,      "Awerial",
  2007L,   254L,      "Awerial",
  2008L,   375L,      "Awerial",
  2009L,   415L,      "Awerial",
  2010L,   262L,      "Awerial",
  2011L,    58L,      "Awerial",
  2012L,     7L,      "Awerial",
  2013L,     9L,      "Awerial",
  2014L,    11L,      "Awerial",
  2015L,     1L,      "Awerial",
  2016L,     0L,      "Awerial",
  2017L,     0L,      "Awerial",
  2018L,     0L,      "Awerial",
  2019L,     0L,      "Awerial",
  2020L,     0L,      "Awerial",
  2021L,     1L,      "Awerial"
  )


district_order<- c("Uror (n=2)", "Rumbek North (n=5)", "Tonj East (n=6)", "Awerial (n=29)") 


df_year_region %>% 
  filter(year>"2011") %>% 
  mutate(location=case_when(location=="Uror" ~ "Uror (n=2)",
                            location=="Rumbek North" ~ "Rumbek North (n=5)",
                            location=="Tonj East" ~ "Tonj East (n=6)",
                            location=="Awerial" ~ "Awerial (n=29)"),
         location=fct_relevel(location, district_order),
         value=as.double(value),
         value_lab=case_when(value==0 ~ NA_real_,
                             value>0 ~ value)) %>% 
  ggplot(aes(x=year, y=value))+
  geom_col(fill=moody_blue, alpha=.8, width=.65)+
  facet_wrap(~location, ncol=1)+
  scale_y_continuous(breaks=seq(0, 15, 5))+
  scale_x_continuous(breaks=seq(2012, 2021, 1))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, family = "Source Sans Pro" ),
        strip.text = element_text(size=16, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  geom_hline(aes(yintercept=1), colour="white", size=.5)+
  geom_hline(aes(yintercept=2), colour="white", size=.5)+
  geom_hline(aes(yintercept=3), colour="white", size=.5)+
  geom_hline(aes(yintercept=4), colour="white", size=.5)+
  geom_hline(aes(yintercept=5), colour="white", size=.5)+
  geom_hline(aes(yintercept=6), colour="white", size=.5)+
  geom_hline(aes(yintercept=7), colour="white", size=.5)+
  geom_hline(aes(yintercept=8), colour="white", size=.5)+
  geom_hline(aes(yintercept=9), colour="white", size=.5)+
  geom_hline(aes(yintercept=10), colour="white", size=.5)+
  geom_hline(aes(yintercept=11), colour="white", size=.5)+
  geom_rect(aes(xmin = 2020.5, xmax = 2021.5, ymin = -Inf, ymax = Inf), alpha = 0.03, fill=trolley_grey_light, inherit.aes = TRUE)+
  geom_text(aes(label=value_lab), na.rm=TRUE, color=trolley_grey, vjust=-.3, family="Source Sans Pro SemiBold")

si_save("Images/2022_gr/yearly_cases_1p")

ggsave("gw_yearly_cases_region.png",
       height = 14,
       width = 24)
