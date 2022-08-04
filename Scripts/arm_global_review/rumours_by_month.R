# PURPOSE: Generate visualization for number of rumours by month
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 1, 2021
# NOTES: February 2022 update with dewormr dataset for Global Review

df_animal_rumours_2021<-df %>% 
  filter(indicator=="rumours_total",
         sheet %in% c("animals")) %>% 
  group_by(month, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  mutate(year="2021") %>% 
  rename(rumours=value) %>% 
  mutate(type="animal")

df_rumours_2021 <-df %>% 
  filter(indicator=="rumours_total",
         sheet %in% c("MSR_Surv", "Non_MSR_Surv", "RPIF")) %>% 
  group_by(month, indicator) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  mutate(year="2021") %>% 
  rename(rumours=value) %>% 
  mutate(type="human") %>% 
  bind_rows(df_animal_rumours_2021)



df_hr<-tibble::tribble(
  ~year, ~month, ~rumours,
  2019L,  "January",    3552L,
  2019L,  "February",    3554L,
  2019L,  "March",    4112L,
  2019L,  "April",    4083L,
  2019L,  "May",    4732L,
  2019L,  "June",    5815L,
  2019L,  "July",    6964L,
  2019L,  "August",    6045L,
  2019L,  "September",    6915L,
  2019L,  "October",    8430L,
  2019L,  "November",    6263L,
  2019L,  "December",    5532L,
  2020L,  "January",    5557L,
  2020L,  "February",    4754L,
  2020L,  "March",    4064L,
  2020L,  "April",    4340L,
  2020L,  "May",    5187L,
  2020L,  "June",    5728L,
  2020L,  "July",    6210L,
  2020L,  "August",    6122L,
  2020L,  "September",    4202L,
  2020L,  "October",    4785L,
  2020L,  "November",    4087L,
  2020L,  "December",    3015L,
  2021L,  "January",    3375L,
  2021L,  "February",    3442L,
  2021L,  "March",    3325L,
  2021L,  "April",    3636L,
  2021L,  "May",    3714L,
  2021L,  "June",    4190L,
  2021L,  "July",    4112L,
  2021L,  "August",    4494L,
  2021L,  "September",    4727L,
  2021L,  "October",    4474L
  )

df_ar<- 
  tibble::tribble(
    ~month, ~year, ~rumours,
    "January", 2018L,       0L,
    "February", 2018L,       0L,
    "March", 2018L,       0L,
    "April", 2018L,       0L,
    "May", 2018L,       1L,
    "June", 2018L,       3L,
    "July", 2018L,       0L,
    "August", 2018L,       1L,
    "September", 2018L,       0L,
    "October", 2018L,       0L,
    "November", 2018L,       1L,
    "December", 2018L,       0L,
    "January", 2019L,       0L,
    "February", 2019L,       2L,
    "March", 2019L,       1L,
    "April", 2019L,       4L,
    "May", 2019L,       1L,
    "June", 2019L,       3L,
    "July", 2019L,       0L,
    "August", 2019L,       6L,
    "September", 2019L,      11L,
    "October", 2019L,      27L,
    "November", 2019L,      13L,
    "December", 2019L,      56L,
    "January", 2020L,      42L,
    "February", 2020L,      44L,
    "March", 2020L,      43L,
    "April", 2020L,      49L,
    "May", 2020L,      42L,
    "June", 2020L,      89L,
    "July", 2020L,      48L,
    "August", 2020L,      54L,
    "September", 2020L,      32L,
    "October", 2020L,      54L,
    "November", 2020L,      33L,
    "December", 2020L,      40L,
    "January", 2021L,      25L,
    "February", 2021L,      37L,
    "March", 2021L,      52L,
    "April", 2021L,      69L,
    "May", 2021L,      48L,
    "June", 2021L,      53L,
    "July", 2021L,      29L,
    "August", 2021L,      86L,
    "September", 2021L,      48L,
    "October", 2021L,      40L
  )

df_cases<-
  tibble::tribble(
    ~year, ~`Jan-Apr`, ~May, ~June, ~July, ~August, ~September, ~October, ~November, ~December,
    2015L,           0L,   0L,   1L,   2L,   1L,   1L,   0L,   0L,   0L,
    2016L,           0L,   0L,   4L,   0L,   0L,   1L,   0L,   1L,   0L,
    2017L,           0L,   0L,   0L,   0L,   0L,   0L,   0L,   0L,   0L,
    2018L,           0L,   2L,   2L,   3L,   2L,   1L,   0L,   0L,   0L,
    2019L,           0L,   0L,   0L,   1L,   1L,   2L,   0L,   0L,   0L,
    2020L,           0L,   0L,   0L,   1L,   0L,   0L,   0L,   0L,   0L,
    2021L,           0L,   0L,   0L,   2L,   1L,   0L,   1L,   0L,   0L
  )
df_cases<-df_cases %>% 
  pivot_longer("Jan-Apr":"December", names_to="month", values_to="cases") %>%
  mutate(month=case_when(month=="Jan-Apr"~"January", TRUE~month), month=as.character(month), year=as.character(year))

df_ar<-df_ar %>% mutate(type="animal") %>% filter(year!="2018")

df_rumours<-df_hr %>% 
  mutate(type="human") %>% 
  bind_rows(df_ar) %>% 
  filter(year!=2021) %>% 
  mutate(year=as.character(year)) %>% 
  bind_rows(df_rumours_2021) %>% 
  mutate(month_short = substr(month,1,1),
         rumours_lab=paste((round(rumours/1000, digits=1)), "k", sep="")) %>% 
  mutate(year_color=case_when(
    year=="2019" ~denim_light,
    year=="2020" ~denim,
    TRUE ~ "#133F7F")) %>%
  mutate(month=fct_relevel(month, month_order)) %>% 
  left_join(df_cases) %>% 
  mutate(cases=as.numeric(cases),
         cases=case_when(type=="animal" ~ 0,
                         is.na(cases) ~ 0,
                         TRUE~cases),
         cases_color=case_when(is.na(cases) | cases==0 ~ "#D9CDC3",
                               cases==1 ~ "#FDAC7A",
                               cases==2 ~ "#DA3C6A",
                               TRUE ~ "#A90773"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A")),
         month=fct_relevel(month, month_order),
         type=fct_relevel(type, type_order))

month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") 
type_order<-c("human", "animal")
cases_color_order<-c(c("#D9CDC3", "#FDAC7A", "#DA3C6A"))

# df_rumours %>% 
#   mutate(year_color=case_when(
#       year=="2019" ~denim_light,
#       year=="2020" ~denim,
#       TRUE ~ "#133F7F")) %>% 
#   mutate(month=fct_relevel(month, month_order),
#          year_color=fct_relevel(year_color, year_color_order))%>%
#   ggplot(aes(x=month, y=rumours, fill=year_color))+
#   #geom_col(position=position_dodge2(width=1, preserve="single"))+
#   geom_col(width=.7, position=position_dodge(.7))+
#   # geom_text_repel(aes(label=comma(round(Cases), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", nudge_y=8000, nudge_x=.1, family="Source Sans Pro SemiBold",
#   #            data = . %>%
#   #              filter(row_number() %% 5 == 3))+
#   geom_text(aes(label=comma(round(rumours), accuracy=1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
#   scale_y_continuous(breaks=seq(0, 10000, 2000))+
#   si_style_ygrid()+
#   labs(x = NULL, y = NULL)+
#   theme(axis.text.x  = element_text(vjust=0.5, size=36, family = "Source Sans Pro"),
#         axis.text.y  = element_text(vjust=0.5, size=30, family = "Source Sans Pro" ),
#         strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
#         axis.line.x = element_blank(),
#         axis.ticks.x = element_blank())+
#   scale_fill_identity()

# ggsave("rumours_by_month.png",
#        height = 14,
#        width = 24)
#Update for global review =======================================================


# df_rumours %>% 
#   ggplot(aes(month, rumours), na.rm=TRUE)+
#   geom_area(aes(group=year), alpha = .4, color = genoa, fill = genoa_light, na.rm=TRUE)+
#   geom_vline(xintercept = c(6.5, 10.5), color="white")+
#   geom_rug(aes(color = factor(cases_color)), size=2, sides="b", na.rm = TRUE) +
#   facet_grid(type~year, scales="free_y")+
#   scale_y_continuous(labels=comma)+
#   scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))+
#   si_style_ygrid()+
#   labs(x = NULL, y = NULL)+
#   theme(panel.spacing.x = unit(.5, "lines"),
#         panel.spacing.y = unit(.5, "lines"))+
#   scale_color_identity()

df_rumours %>% 
  ggplot(aes(month, rumours, group=year, color=type))+
  geom_area(data=subset(df_rumours, type=="animal"), color=moody_blue, fill=moody_blue_light, alpha = .4, na.rm=TRUE)+
  geom_area(data=subset(df_rumours, type=="human"), color=genoa, fill=genoa_light, alpha = .4, na.rm=TRUE)+
  #geom_area(aes(fill=type), alpha = .4, na.rm=TRUE)+
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
  scale_color_manual(values= c("#D9CDC3", "#FDAC7A", "#DA3C6A", genoa, moody_blue))
  scale_color_identity()

si_save("Images/2022_gr/rumours_monthly")

#Total Rumours by year
df_rumours %>% 
  group_by(year, type) %>% 
  summarise(across(c(rumours), sum, na.rm=TRUE))

#Rumours/24 hours investigated/ suspects

df_suspects_district<-
tibble::tribble(
  ~month,                              ~type,        ~region, ~value,
   "Jan",                 "Rumours Reported",    "Tonj East",   241L,
   "Feb",                 "Rumours Reported",    "Tonj East",   247L,
   "Mar",                 "Rumours Reported",    "Tonj East",   383L,
   "Apr",                 "Rumours Reported",    "Tonj East",   515L,
   "May",                 "Rumours Reported",    "Tonj East",   509L,
   "Jun",                 "Rumours Reported",    "Tonj East",   465L,
   "Jul",                 "Rumours Reported",    "Tonj East",   132L,
   "Aug",                 "Rumours Reported",    "Tonj East",   422L,
   "Sep",                 "Rumours Reported",    "Tonj East",   331L,
   "Oct",                 "Rumours Reported",    "Tonj East",   218L,
   "Jan", "Rumours Investigated in 24 Hours",    "Tonj East",   240L,
   "Feb", "Rumours Investigated in 24 Hours",    "Tonj East",   244L,
   "Mar", "Rumours Investigated in 24 Hours",    "Tonj East",   378L,
   "Apr", "Rumours Investigated in 24 Hours",    "Tonj East",   493L,
   "May", "Rumours Investigated in 24 Hours",    "Tonj East",   481L,
   "Jun", "Rumours Investigated in 24 Hours",    "Tonj East",   456L,
   "Jul", "Rumours Investigated in 24 Hours",    "Tonj East",   132L,
   "Aug", "Rumours Investigated in 24 Hours",    "Tonj East",   420L,
   "Sep", "Rumours Investigated in 24 Hours",    "Tonj East",   330L,
   "Oct", "Rumours Investigated in 24 Hours",    "Tonj East",   217L,
   "Jan",     "Rumours that Became Suspects",    "Tonj East",    53L,
   "Feb",     "Rumours that Became Suspects",    "Tonj East",    58L,
   "Mar",     "Rumours that Became Suspects",    "Tonj East",    98L,
   "Apr",     "Rumours that Became Suspects",    "Tonj East",   132L,
   "May",     "Rumours that Became Suspects",    "Tonj East",   182L,
   "Jun",     "Rumours that Became Suspects",    "Tonj East",   188L,
   "Jul",     "Rumours that Became Suspects",    "Tonj East",    50L,
   "Aug",     "Rumours that Became Suspects",    "Tonj East",   156L,
   "Sep",     "Rumours that Became Suspects",    "Tonj East",   119L,
   "Oct",     "Rumours that Became Suspects",    "Tonj East",    83L,
   "Jan",                 "Rumours Reported",         "Uror",    58L,
   "Feb",                 "Rumours Reported",         "Uror",    60L,
   "Mar",                 "Rumours Reported",         "Uror",    64L,
   "Apr",                 "Rumours Reported",         "Uror",    58L,
   "May",                 "Rumours Reported",         "Uror",    64L,
   "Jun",                 "Rumours Reported",         "Uror",    59L,
   "Jul",                 "Rumours Reported",         "Uror",    79L,
   "Aug",                 "Rumours Reported",         "Uror",    93L,
   "Sep",                 "Rumours Reported",         "Uror",   225L,
   "Oct",                 "Rumours Reported",         "Uror",   236L,
   "Jan", "Rumours Investigated in 24 Hours",         "Uror",    57L,
   "Feb", "Rumours Investigated in 24 Hours",         "Uror",    60L,
   "Mar", "Rumours Investigated in 24 Hours",         "Uror",    64L,
   "Apr", "Rumours Investigated in 24 Hours",         "Uror",    57L,
   "May", "Rumours Investigated in 24 Hours",         "Uror",    63L,
   "Jun", "Rumours Investigated in 24 Hours",         "Uror",    59L,
   "Jul", "Rumours Investigated in 24 Hours",         "Uror",    79L,
   "Aug", "Rumours Investigated in 24 Hours",         "Uror",    93L,
   "Sep", "Rumours Investigated in 24 Hours",         "Uror",   225L,
   "Oct", "Rumours Investigated in 24 Hours",         "Uror",   233L,
   "Jan",     "Rumours that Became Suspects",         "Uror",    17L,
   "Feb",     "Rumours that Became Suspects",         "Uror",    17L,
   "Mar",     "Rumours that Became Suspects",         "Uror",    14L,
   "Apr",     "Rumours that Became Suspects",         "Uror",    20L,
   "May",     "Rumours that Became Suspects",         "Uror",    21L,
   "Jun",     "Rumours that Became Suspects",         "Uror",    23L,
   "Jul",     "Rumours that Became Suspects",         "Uror",    37L,
   "Aug",     "Rumours that Became Suspects",         "Uror",    37L,
   "Sep",     "Rumours that Became Suspects",         "Uror",    62L,
   "Oct",     "Rumours that Became Suspects",         "Uror",    85L,
   "Jan",                 "Rumours Reported", "Rumbek North",   394L,
   "Feb",                 "Rumours Reported", "Rumbek North",   374L,
   "Mar",                 "Rumours Reported", "Rumbek North",   407L,
   "Apr",                 "Rumours Reported", "Rumbek North",   314L,
   "May",                 "Rumours Reported", "Rumbek North",   374L,
   "Jun",                 "Rumours Reported", "Rumbek North",   470L,
   "Jul",                 "Rumours Reported", "Rumbek North",   398L,
   "Aug",                 "Rumours Reported", "Rumbek North",   355L,
   "Sep",                 "Rumours Reported", "Rumbek North",   381L,
   "Oct",                 "Rumours Reported", "Rumbek North",   415L,
   "Jan", "Rumours Investigated in 24 Hours", "Rumbek North",   394L,
   "Feb", "Rumours Investigated in 24 Hours", "Rumbek North",   374L,
   "Mar", "Rumours Investigated in 24 Hours", "Rumbek North",   406L,
   "Apr", "Rumours Investigated in 24 Hours", "Rumbek North",   314L,
   "May", "Rumours Investigated in 24 Hours", "Rumbek North",   372L,
   "Jun", "Rumours Investigated in 24 Hours", "Rumbek North",   470L,
   "Jul", "Rumours Investigated in 24 Hours", "Rumbek North",   393L,
   "Aug", "Rumours Investigated in 24 Hours", "Rumbek North",   354L,
   "Sep", "Rumours Investigated in 24 Hours", "Rumbek North",   381L,
   "Oct", "Rumours Investigated in 24 Hours", "Rumbek North",   415L,
   "Jan",     "Rumours that Became Suspects", "Rumbek North",    55L,
   "Feb",     "Rumours that Became Suspects", "Rumbek North",    78L,
   "Mar",     "Rumours that Became Suspects", "Rumbek North",    91L,
   "Apr",     "Rumours that Became Suspects", "Rumbek North",    78L,
   "May",     "Rumours that Became Suspects", "Rumbek North",   115L,
   "Jun",     "Rumours that Became Suspects", "Rumbek North",   118L,
   "Jul",     "Rumours that Became Suspects", "Rumbek North",   128L,
   "Aug",     "Rumours that Became Suspects", "Rumbek North",   120L,
   "Sep",     "Rumours that Became Suspects", "Rumbek North",   157L,
   "Oct",     "Rumours that Became Suspects", "Rumbek North",   116L
  )




View(df_suspects_district)

type_order<-c(`Rumours that Became Suspects`, `Rumours Investigated in 24 Hours`, `Rumours that Became Suspects` )
month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
type_color_order<- c(old_rose_light, denim_light, denim)

df_suspects_district %>% 
  filter(region=="Uror") %>% 
  mutate(value=as.numeric(value)) %>% 
  pivot_wider(names_from=type, values_from=value) %>%
  mutate(
    `Rumours Reported`=as.numeric(`Rumours Reported`),
    `Rumours Investigated in 24 Hours`=as.numeric(`Rumours Investigated in 24 Hours`),
    `Rumours that Became Suspects`=as.numeric(`Rumours that Became Suspects`),
    `Rumours Investigated in 24 Hours`=`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    `Rumours Reported`=`Rumours Reported`-`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    month=fct_relevel(month, month_order)) %>% 
  pivot_longer(c( `Rumours Reported`:`Rumours that Became Suspects`), names_to = "type") %>%
  mutate(type_color=case_when(
    type=="Rumours that Became Suspects" ~denim_light,
    type=="Rumours Investigated in 24 Hours" ~denim,
    type=="Rumours Reported"~old_rose_light),
    type_color=fct_relevel(type_color, type_color_order)) %>% 
  ggplot(aes(x=month, y=value, fill=type_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE)+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=8, family="Source Sans Pro SemiBold")+
  scale_y_continuous(breaks=seq(0, 800, 100))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=24, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


ggsave("suspects_UR.png",
       height = 5,
       width = 10)

df_suspects_district %>% 
  filter(region=="Tonj East") %>% 
  mutate(value=as.numeric(value)) %>% 
  pivot_wider(names_from=type, values_from=value) %>%
  mutate(
    `Rumours Reported`=as.numeric(`Rumours Reported`),
    `Rumours Investigated in 24 Hours`=as.numeric(`Rumours Investigated in 24 Hours`),
    `Rumours that Became Suspects`=as.numeric(`Rumours that Became Suspects`),
    `Rumours Investigated in 24 Hours`=`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    `Rumours Reported`=`Rumours Reported`-`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    month=fct_relevel(month, month_order)) %>% 
  pivot_longer(c( `Rumours Reported`:`Rumours that Became Suspects`), names_to = "type") %>%
  mutate(type_color=case_when(
    type=="Rumours that Became Suspects" ~denim_light,
    type=="Rumours Investigated in 24 Hours" ~denim,
    type=="Rumours Reported"~old_rose_light),
    type_color=fct_relevel(type_color, type_color_order)) %>% 
  ggplot(aes(x=month, y=value, fill=type_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE)+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=8, family="Source Sans Pro SemiBold")+
  scale_y_continuous(breaks=seq(0, 800, 100))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=24, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


ggsave("suspects_TE.png",
       height = 5,
       width = 10)

df_suspects_district %>% 
  filter(region=="Rumbek North") %>% 
  mutate(value=as.numeric(value)) %>% 
  pivot_wider(names_from=type, values_from=value) %>%
  mutate(
    `Rumours Reported`=as.numeric(`Rumours Reported`),
    `Rumours Investigated in 24 Hours`=as.numeric(`Rumours Investigated in 24 Hours`),
    `Rumours that Became Suspects`=as.numeric(`Rumours that Became Suspects`),
    `Rumours Investigated in 24 Hours`=`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    `Rumours Reported`=`Rumours Reported`-`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    month=fct_relevel(month, month_order)) %>% 
  pivot_longer(c( `Rumours Reported`:`Rumours that Became Suspects`), names_to = "type") %>%
  mutate(type_color=case_when(
    type=="Rumours that Became Suspects" ~denim_light,
    type=="Rumours Investigated in 24 Hours" ~denim,
    type=="Rumours Reported"~old_rose_light),
    type_color=fct_relevel(type_color, type_color_order)) %>% 
  ggplot(aes(x=month, y=value, fill=type_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE)+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=8, family="Source Sans Pro SemiBold")+
  scale_y_continuous(breaks=seq(0, 800, 100))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=24, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


ggsave("suspects_RN.png",
       height = 5,
       width = 10)


##################### Updating these graphs 12/6

month_order_rev<- c("Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan")
type_color_order<- c(burnt_sienna, scooter, scooter_light)

df_suspects_district %>% 
  filter(region=="Uror") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type== "Rumours Reported" ~scooter_light,
    type=="Rumours Investigated in 24 Hours" ~scooter,
    type=="Rumours that Became Suspects" ~ burnt_sienna)) %>%
  mutate(month=fct_relevel(month, month_order_rev),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=month, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.8, position=position_dodge(.8), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.8, preserve="single"), na.rm=TRUE, color="black", hjust=-.2, size=9, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  scale_y_continuous(labels=comma, breaks=seq(0, 250, 50), limits = c(0,250))+
  coord_flip()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("suspects_UR_updated.png",
       height = 10,
       width = 17)

df_suspects_district %>% 
  filter(region=="Tonj East") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type== "Rumours Reported" ~scooter_light,
    type=="Rumours Investigated in 24 Hours" ~scooter,
    type=="Rumours that Became Suspects" ~ burnt_sienna)) %>%
  mutate(month=fct_relevel(month, month_order_rev),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=month, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.8, position=position_dodge(.8), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.8, preserve="single"), na.rm=TRUE, color="black", hjust=-.2, size=9, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  scale_y_continuous(labels=comma, breaks=seq(0, 550, 100), limits = c(0,550))+
  coord_flip()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("suspects_TE_updated.png",
       height = 10,
       width = 17)

df_suspects_district %>% 
  filter(region=="Rumbek North") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type== "Rumours Reported" ~scooter_light,
    type=="Rumours Investigated in 24 Hours" ~scooter,
    type=="Rumours that Became Suspects" ~ burnt_sienna)) %>%
  mutate(month=fct_relevel(month, month_order_rev),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=month, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.8, position=position_dodge(.8), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.8, preserve="single"), na.rm=TRUE, color="black", hjust=-.2, size=9, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  scale_y_continuous(labels=comma, breaks=seq(0, 550, 100), limits = c(0,550))+
  coord_flip()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("suspects_RN_updated.png",
       height = 10,
       width = 17)




df_suspects_r2<-
tibble::tribble(
  ~month,                              ~type,         ~region, ~value,
   "Jan",                 "Rumours Reported",         "Akobo",    71L,
   "Feb",                 "Rumours Reported",         "Akobo",    55L,
   "Mar",                 "Rumours Reported",         "Akobo",    54L,
   "Apr",                 "Rumours Reported",         "Akobo",    53L,
   "May",                 "Rumours Reported",         "Akobo",    48L,
   "Jun",                 "Rumours Reported",         "Akobo",    26L,
   "Jul",                 "Rumours Reported",         "Akobo",    40L,
   "Aug",                 "Rumours Reported",         "Akobo",    22L,
   "Sep",                 "Rumours Reported",         "Akobo",    28L,
   "Oct",                 "Rumours Reported",         "Akobo",    18L,
   "Jan", "Rumours Investigated in 24 Hours",         "Akobo",    71L,
   "Feb", "Rumours Investigated in 24 Hours",         "Akobo",    55L,
   "Mar", "Rumours Investigated in 24 Hours",         "Akobo",    54L,
   "Apr", "Rumours Investigated in 24 Hours",         "Akobo",    53L,
   "May", "Rumours Investigated in 24 Hours",         "Akobo",    48L,
   "Jun", "Rumours Investigated in 24 Hours",         "Akobo",    26L,
   "Jul", "Rumours Investigated in 24 Hours",         "Akobo",    40L,
   "Aug", "Rumours Investigated in 24 Hours",         "Akobo",    22L,
   "Sep", "Rumours Investigated in 24 Hours",         "Akobo",    28L,
   "Oct", "Rumours Investigated in 24 Hours",         "Akobo",    18L,
   "Jan",     "Rumours that Became Suspects",         "Akobo",    46L,
   "Feb",     "Rumours that Became Suspects",         "Akobo",    39L,
   "Mar",     "Rumours that Became Suspects",         "Akobo",    40L,
   "Apr",     "Rumours that Became Suspects",         "Akobo",    33L,
   "May",     "Rumours that Became Suspects",         "Akobo",    33L,
   "Jun",     "Rumours that Became Suspects",         "Akobo",    15L,
   "Jul",     "Rumours that Became Suspects",         "Akobo",    27L,
   "Aug",     "Rumours that Became Suspects",         "Akobo",    13L,
   "Sep",     "Rumours that Became Suspects",         "Akobo",    17L,
   "Oct",     "Rumours that Became Suspects",         "Akobo",    11L,
   "Jan",                 "Rumours Reported",        "Nyirol",   259L,
   "Feb",                 "Rumours Reported",        "Nyirol",   232L,
   "Mar",                 "Rumours Reported",        "Nyirol",   249L,
   "Apr",                 "Rumours Reported",        "Nyirol",   254L,
   "May",                 "Rumours Reported",        "Nyirol",   261L,
   "Jun",                 "Rumours Reported",        "Nyirol",   225L,
   "Jul",                 "Rumours Reported",        "Nyirol",   245L,
   "Aug",                 "Rumours Reported",        "Nyirol",   224L,
   "Sep",                 "Rumours Reported",        "Nyirol",   235L,
   "Oct",                 "Rumours Reported",        "Nyirol",   226L,
   "Jan", "Rumours Investigated in 24 Hours",        "Nyirol",   259L,
   "Feb", "Rumours Investigated in 24 Hours",        "Nyirol",   231L,
   "Mar", "Rumours Investigated in 24 Hours",        "Nyirol",   249L,
   "Apr", "Rumours Investigated in 24 Hours",        "Nyirol",   254L,
   "May", "Rumours Investigated in 24 Hours",        "Nyirol",   261L,
   "Jun", "Rumours Investigated in 24 Hours",        "Nyirol",   225L,
   "Jul", "Rumours Investigated in 24 Hours",        "Nyirol",   243L,
   "Aug", "Rumours Investigated in 24 Hours",        "Nyirol",   224L,
   "Sep", "Rumours Investigated in 24 Hours",        "Nyirol",   235L,
   "Oct", "Rumours Investigated in 24 Hours",        "Nyirol",   226L,
   "Jan",     "Rumours that Became Suspects",        "Nyirol",   131L,
   "Feb",     "Rumours that Became Suspects",        "Nyirol",   114L,
   "Mar",     "Rumours that Became Suspects",        "Nyirol",   121L,
   "Apr",     "Rumours that Became Suspects",        "Nyirol",   104L,
   "May",     "Rumours that Became Suspects",        "Nyirol",   111L,
   "Jun",     "Rumours that Became Suspects",        "Nyirol",    94L,
   "Jul",     "Rumours that Became Suspects",        "Nyirol",   117L,
   "Aug",     "Rumours that Became Suspects",        "Nyirol",    97L,
   "Sep",     "Rumours that Became Suspects",        "Nyirol",    96L,
   "Oct",     "Rumours that Became Suspects",        "Nyirol",    96L,
   "Jan",                 "Rumours Reported",    "Tonj North",   338L,
   "Feb",                 "Rumours Reported",    "Tonj North",   302L,
   "Mar",                 "Rumours Reported",    "Tonj North",   247L,
   "Apr",                 "Rumours Reported",    "Tonj North",   436L,
   "May",                 "Rumours Reported",    "Tonj North",   371L,
   "Jun",                 "Rumours Reported",    "Tonj North",   542L,
   "Jul",                 "Rumours Reported",    "Tonj North",   502L,
   "Aug",                 "Rumours Reported",    "Tonj North",   525L,
   "Sep",                 "Rumours Reported",    "Tonj North",   435L,
   "Oct",                 "Rumours Reported",    "Tonj North",   371L,
   "Jan", "Rumours Investigated in 24 Hours",    "Tonj North",   335L,
   "Feb", "Rumours Investigated in 24 Hours",    "Tonj North",   301L,
   "Mar", "Rumours Investigated in 24 Hours",    "Tonj North",   247L,
   "Apr", "Rumours Investigated in 24 Hours",    "Tonj North",   436L,
   "May", "Rumours Investigated in 24 Hours",    "Tonj North",   370L,
   "Jun", "Rumours Investigated in 24 Hours",    "Tonj North",   539L,
   "Jul", "Rumours Investigated in 24 Hours",    "Tonj North",   500L,
   "Aug", "Rumours Investigated in 24 Hours",    "Tonj North",   525L,
   "Sep", "Rumours Investigated in 24 Hours",    "Tonj North",   435L,
   "Oct", "Rumours Investigated in 24 Hours",    "Tonj North",   371L,
   "Jan",     "Rumours that Became Suspects",    "Tonj North",    26L,
   "Feb",     "Rumours that Became Suspects",    "Tonj North",    19L,
   "Mar",     "Rumours that Became Suspects",    "Tonj North",    39L,
   "Apr",     "Rumours that Became Suspects",    "Tonj North",    32L,
   "May",     "Rumours that Became Suspects",    "Tonj North",    35L,
   "Jun",     "Rumours that Became Suspects",    "Tonj North",    44L,
   "Jul",     "Rumours that Became Suspects",    "Tonj North",    27L,
   "Aug",     "Rumours that Became Suspects",    "Tonj North",    56L,
   "Sep",     "Rumours that Became Suspects",    "Tonj North",    38L,
   "Oct",     "Rumours that Became Suspects",    "Tonj North",    13L,
   "Jan",                 "Rumours Reported",    "Tonj South",    50L,
   "Feb",                 "Rumours Reported",    "Tonj South",    50L,
   "Mar",                 "Rumours Reported",    "Tonj South",    58L,
   "Apr",                 "Rumours Reported",    "Tonj South",    57L,
   "May",                 "Rumours Reported",    "Tonj South",    66L,
   "Jun",                 "Rumours Reported",    "Tonj South",    61L,
   "Jul",                 "Rumours Reported",    "Tonj South",    10L,
   "Aug",                 "Rumours Reported",    "Tonj South",    14L,
   "Sep",                 "Rumours Reported",    "Tonj South",    32L,
   "Oct",                 "Rumours Reported",    "Tonj South",    25L,
   "Jan", "Rumours Investigated in 24 Hours",    "Tonj South",    50L,
   "Feb", "Rumours Investigated in 24 Hours",    "Tonj South",    50L,
   "Mar", "Rumours Investigated in 24 Hours",    "Tonj South",    58L,
   "Apr", "Rumours Investigated in 24 Hours",    "Tonj South",    57L,
   "May", "Rumours Investigated in 24 Hours",    "Tonj South",    66L,
   "Jun", "Rumours Investigated in 24 Hours",    "Tonj South",    61L,
   "Jul", "Rumours Investigated in 24 Hours",    "Tonj South",    10L,
   "Aug", "Rumours Investigated in 24 Hours",    "Tonj South",    14L,
   "Sep", "Rumours Investigated in 24 Hours",    "Tonj South",    32L,
   "Oct", "Rumours Investigated in 24 Hours",    "Tonj South",    25L,
   "Jan",     "Rumours that Became Suspects",    "Tonj South",     2L,
   "Feb",     "Rumours that Became Suspects",    "Tonj South",     5L,
   "Mar",     "Rumours that Became Suspects",    "Tonj South",    26L,
   "Apr",     "Rumours that Became Suspects",    "Tonj South",    31L,
   "May",     "Rumours that Became Suspects",    "Tonj South",    35L,
   "Jun",     "Rumours that Became Suspects",    "Tonj South",    46L,
   "Jul",     "Rumours that Became Suspects",    "Tonj South",    10L,
   "Aug",     "Rumours that Became Suspects",    "Tonj South",    14L,
   "Sep",     "Rumours that Became Suspects",    "Tonj South",    31L,
   "Oct",     "Rumours that Became Suspects",    "Tonj South",    25L,
   "Jan",                 "Rumours Reported", "Rumbek Center",   131L,
   "Feb",                 "Rumours Reported", "Rumbek Center",   119L,
   "Mar",                 "Rumours Reported", "Rumbek Center",   192L,
   "Apr",                 "Rumours Reported", "Rumbek Center",   132L,
   "May",                 "Rumours Reported", "Rumbek Center",   165L,
   "Jun",                 "Rumours Reported", "Rumbek Center",   183L,
   "Jul",                 "Rumours Reported", "Rumbek Center",   256L,
   "Aug",                 "Rumours Reported", "Rumbek Center",   278L,
   "Sep",                 "Rumours Reported", "Rumbek Center",   262L,
   "Oct",                 "Rumours Reported", "Rumbek Center",   346L,
   "Jan", "Rumours Investigated in 24 Hours", "Rumbek Center",   131L,
   "Feb", "Rumours Investigated in 24 Hours", "Rumbek Center",   117L,
   "Mar", "Rumours Investigated in 24 Hours", "Rumbek Center",   192L,
   "Apr", "Rumours Investigated in 24 Hours", "Rumbek Center",   129L,
   "May", "Rumours Investigated in 24 Hours", "Rumbek Center",   160L,
   "Jun", "Rumours Investigated in 24 Hours", "Rumbek Center",   183L,
   "Jul", "Rumours Investigated in 24 Hours", "Rumbek Center",   255L,
   "Aug", "Rumours Investigated in 24 Hours", "Rumbek Center",   278L,
   "Sep", "Rumours Investigated in 24 Hours", "Rumbek Center",   262L,
   "Oct", "Rumours Investigated in 24 Hours", "Rumbek Center",   342L,
   "Jan",     "Rumours that Became Suspects", "Rumbek Center",    33L,
   "Feb",     "Rumours that Became Suspects", "Rumbek Center",    30L,
   "Mar",     "Rumours that Became Suspects", "Rumbek Center",    41L,
   "Apr",     "Rumours that Became Suspects", "Rumbek Center",    38L,
   "May",     "Rumours that Became Suspects", "Rumbek Center",    51L,
   "Jun",     "Rumours that Became Suspects", "Rumbek Center",    52L,
   "Jul",     "Rumours that Became Suspects", "Rumbek Center",    68L,
   "Aug",     "Rumours that Became Suspects", "Rumbek Center",    76L,
   "Sep",     "Rumours that Became Suspects", "Rumbek Center",    63L,
   "Oct",     "Rumours that Became Suspects", "Rumbek Center",    87L,
   "Jan",                 "Rumours Reported",          "Wulu",    67L,
   "Feb",                 "Rumours Reported",          "Wulu",    63L,
   "Mar",                 "Rumours Reported",          "Wulu",    62L,
   "Apr",                 "Rumours Reported",          "Wulu",    64L,
   "May",                 "Rumours Reported",          "Wulu",    50L,
   "Jun",                 "Rumours Reported",          "Wulu",    54L,
   "Jul",                 "Rumours Reported",          "Wulu",    54L,
   "Aug",                 "Rumours Reported",          "Wulu",    36L,
   "Sep",                 "Rumours Reported",          "Wulu",    72L,
   "Oct",                 "Rumours Reported",          "Wulu",    61L,
   "Jan", "Rumours Investigated in 24 Hours",          "Wulu",    67L,
   "Feb", "Rumours Investigated in 24 Hours",          "Wulu",    63L,
   "Mar", "Rumours Investigated in 24 Hours",          "Wulu",    62L,
   "Apr", "Rumours Investigated in 24 Hours",          "Wulu",    62L,
   "May", "Rumours Investigated in 24 Hours",          "Wulu",    50L,
   "Jun", "Rumours Investigated in 24 Hours",          "Wulu",    54L,
   "Jul", "Rumours Investigated in 24 Hours",          "Wulu",    54L,
   "Aug", "Rumours Investigated in 24 Hours",          "Wulu",    36L,
   "Sep", "Rumours Investigated in 24 Hours",          "Wulu",    72L,
   "Oct", "Rumours Investigated in 24 Hours",          "Wulu",    61L,
   "Jan",     "Rumours that Became Suspects",          "Wulu",    18L,
   "Feb",     "Rumours that Became Suspects",          "Wulu",    12L,
   "Mar",     "Rumours that Became Suspects",          "Wulu",    19L,
   "Apr",     "Rumours that Became Suspects",          "Wulu",    24L,
   "May",     "Rumours that Became Suspects",          "Wulu",    11L,
   "Jun",     "Rumours that Became Suspects",          "Wulu",    10L,
   "Jul",     "Rumours that Became Suspects",          "Wulu",    10L,
   "Aug",     "Rumours that Became Suspects",          "Wulu",     7L,
   "Sep",     "Rumours that Became Suspects",          "Wulu",    20L,
   "Oct",     "Rumours that Became Suspects",          "Wulu",    19L
  )

type_order<-c(`Rumours that Became Suspects`, `Rumours Investigated in 24 Hours`, `Rumours that Became Suspects` )
month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
type_color_order<- c(old_rose_light, denim_light, denim)

df_suspects_r2 %>% 
  filter(region %in% c("Tonj North", "Tonj South")) %>% 
  mutate(value=as.numeric(value)) %>% 
  pivot_wider(names_from=type, values_from=value) %>%
  mutate(
    `Rumours Reported`=as.numeric(`Rumours Reported`),
    `Rumours Investigated in 24 Hours`=as.numeric(`Rumours Investigated in 24 Hours`),
    `Rumours that Became Suspects`=as.numeric(`Rumours that Became Suspects`),
    `Rumours Investigated in 24 Hours`=`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    `Rumours Reported`=`Rumours Reported`-`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    month=fct_relevel(month, month_order)) %>% 
  pivot_longer(c( `Rumours Reported`:`Rumours that Became Suspects`), names_to = "type") %>%
  mutate(type_color=case_when(
    type=="Rumours that Became Suspects" ~denim,
    type=="Rumours Investigated in 24 Hours" ~denim_light,
    type=="Rumours Reported"~old_rose_light),
    type_color=fct_relevel(type_color, type_color_order)) %>% 
  ggplot(aes(x=month, y=value, fill=type_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE)+
  facet_wrap(~region, ncol=1, scale="free_y")+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=8, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 400, 50))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=20, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=20, family = "Source Sans Pro" ),
        strip.text = element_text(size = 30, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


ggsave("suspects_r2_TE.png",
       height = 5,
       width = 10)

df_suspects_r2 %>% 
  filter(region %in% c("Akobo", "Nyirol")) %>% 
  mutate(value=as.numeric(value)) %>% 
  pivot_wider(names_from=type, values_from=value) %>%
  mutate(
    `Rumours Reported`=as.numeric(`Rumours Reported`),
    `Rumours Investigated in 24 Hours`=as.numeric(`Rumours Investigated in 24 Hours`),
    `Rumours that Became Suspects`=as.numeric(`Rumours that Became Suspects`),
    `Rumours Investigated in 24 Hours`=`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    `Rumours Reported`=`Rumours Reported`-`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    month=fct_relevel(month, month_order)) %>% 
  pivot_longer(c( `Rumours Reported`:`Rumours that Became Suspects`), names_to = "type") %>%
  mutate(type_color=case_when(
    type=="Rumours that Became Suspects" ~denim,
    type=="Rumours Investigated in 24 Hours" ~denim_light,
    type=="Rumours Reported"~old_rose_light),
    type_color=fct_relevel(type_color, type_color_order)) %>% 
  ggplot(aes(x=month, y=value, fill=type_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE)+
  facet_wrap(~region, ncol=1)+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=8, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 600, 100))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=20, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=20, family = "Source Sans Pro" ),
        strip.text = element_text(size = 30, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


ggsave("suspects_r2_UR.png",
       height = 5,
       width = 10)

df_suspects_r2 %>% 
  filter(region %in% c("Rumbek Center", "Wulu")) %>% 
  mutate(value=as.numeric(value)) %>% 
  pivot_wider(names_from=type, values_from=value) %>%
  mutate(
    `Rumours Reported`=as.numeric(`Rumours Reported`),
    `Rumours Investigated in 24 Hours`=as.numeric(`Rumours Investigated in 24 Hours`),
    `Rumours that Became Suspects`=as.numeric(`Rumours that Became Suspects`),
    `Rumours Investigated in 24 Hours`=`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    `Rumours Reported`=`Rumours Reported`-`Rumours Investigated in 24 Hours`-`Rumours that Became Suspects`,
    month=fct_relevel(month, month_order)) %>% 
  pivot_longer(c( `Rumours Reported`:`Rumours that Became Suspects`), names_to = "type") %>%
  mutate(type_color=case_when(
    type=="Rumours that Became Suspects" ~denim,
    type=="Rumours Investigated in 24 Hours" ~denim_light,
    type=="Rumours Reported"~old_rose_light),
    type_color=fct_relevel(type_color, type_color_order)) %>% 
  ggplot(aes(x=month, y=value, fill=type_color), leg)+
  geom_bar(position="stack", stat="identity", show.legend=FALSE)+
  facet_wrap(~region, ncol=1)+
  #geom_col(alpha=.9, show.legend = FALSE)+
  #geom_text(aes(label=(value)), na.rm=TRUE, color=trolley_grey, vjust=0, size=8, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 600, 100))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=20, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=20, family = "Source Sans Pro" ),
        strip.text = element_text(size = 30, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


ggsave("suspects_r2_RN.png",
       height = 5,
       width = 10)



###########Updating for risk level 2 districts HERE


df_suspects_r2 %>% 
  filter(region %in% c("Akobo", "Nyirol")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type== "Rumours Reported" ~scooter_light,
    type=="Rumours Investigated in 24 Hours" ~scooter,
    type=="Rumours that Became Suspects" ~ burnt_sienna)) %>%
  mutate(month=fct_relevel(month, month_order_rev),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=month, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.8, position=position_dodge(.8), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.8, preserve="single"), na.rm=TRUE, color="black", hjust=-.2, size=8, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  facet_wrap(~region, ncol=2)+
  scale_y_continuous(labels=comma, breaks=seq(0, 275, 100), limits = c(0,275))+
  coord_flip()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("suspects_r2_UR_updated.png",
       height = 10,
       width = 17)

df_suspects_r2 %>% 
  filter(region %in% c("Tonj North", "Tonj South")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type== "Rumours Reported" ~scooter_light,
    type=="Rumours Investigated in 24 Hours" ~scooter,
    type=="Rumours that Became Suspects" ~ burnt_sienna)) %>%
  mutate(month=fct_relevel(month, month_order_rev),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=month, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.8, position=position_dodge(.8), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.8, preserve="single"), na.rm=TRUE, color="black", hjust=-.2, size=8, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  facet_wrap(~region, ncol=2)+
  scale_y_continuous(labels=comma, breaks=seq(0, 600, 200), limits = c(0,600))+
  coord_flip()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("suspects_r2_TE_updated.png",
       height = 10,
       width = 17)

df_suspects_r2 %>% 
  filter(region %in% c("Rumbek Center", "Wulu")) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type== "Rumours Reported" ~scooter_light,
    type=="Rumours Investigated in 24 Hours" ~scooter,
    type=="Rumours that Became Suspects" ~ burnt_sienna)) %>%
  mutate(month=fct_relevel(month, month_order_rev),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=month, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.8, position=position_dodge(.8), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.8, preserve="single"), na.rm=TRUE, color="black", hjust=-.2, size=8, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  facet_wrap(~region, ncol=2)+
  scale_y_continuous(labels=comma, breaks=seq(0, 350, 100), limits = c(0,350))+
  coord_flip()+
  si_style_xgrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("suspects_r2_RN_updated.png",
       height = 10,
       width = 17)



rl3_rumours<-
tibble::tribble(
              ~type, ~month, ~value,
          "rumours",  "Jan",  1891L,
          "rumours",  "Feb",  2003L,
          "rumours",  "Mar",  1671L,
          "rumours",  "Apr",  1817L,
          "rumours",  "May",  1856L,
          "rumours",  "Jun",  2157L,
          "rumours",  "Jul",  2590L,
          "rumours",  "Aug",  2574L,
          "rumours",  "Sep",  2798L,
          "rumours",  "Oct",  2612L,
     "investigated",  "Jan",  1890L,
     "investigated",  "Feb",  1986L,
     "investigated",  "Mar",  1612L,
     "investigated",  "Apr",  1803L,
     "investigated",  "May",  1844L,
     "investigated",  "Jun",  2147L,
     "investigated",  "Jul",  2568L,
     "investigated",  "Aug",  2563L,
     "investigated",  "Sep",  2797L,
     "investigated",  "Oct",  2611L,
         "suspects",  "Jan",   765L,
         "suspects",  "Feb",   789L,
         "suspects",  "Mar",   644L,
         "suspects",  "Apr",   783L,
         "suspects",  "May",   932L,
         "suspects",  "Jun",  1236L,
         "suspects",  "Jul",  1609L,
         "suspects",  "Aug",  1537L,
         "suspects",  "Sep",  1717L,
         "suspects",  "Oct",  1496L
     )

month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct") 
type_color_order<- c(scooter_light, scooter, burnt_sienna)

rl3_rumours %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type=="rumours" ~scooter_light,
    type=="investigated" ~scooter,
    type=="suspects" ~ burnt_sienna)) %>%
  mutate(month=fct_relevel(month, month_order),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=month, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.95, position=position_dodge(.95), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.95, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  scale_y_continuous(labels=comma, breaks=seq(0, 3000, 1000), limits = c(0,3000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("r3_rumours_month.png",
       height = 14,
       width = 24)

rl3_rumours_region<-
tibble::tribble(
                 ~region,          ~type, ~value,
  "Other (<500 rumours)",      "rumours",  1130L,
          "Gogrial West",      "rumours",  1082L,
                "Ikotos",      "rumours",  1090L,
             "Jur River",      "rumours", 11598L,
            "Lopa/Lafon",      "rumours",  1478L,
                 "Torit",      "rumours",  4486L,
                   "Wau",      "rumours",   521L,
                  "Wulu",      "rumours",   583L,
  "Other (<500 rumours)", "investigated",  1094L,
          "Gogrial West", "investigated",  1072L,
                "Ikotos", "investigated",  1087L,
             "Jur River", "investigated", 11598L,
            "Lopa/Lafon", "investigated",  1459L,
                 "Torit", "investigated",  4412L,
                   "Wau", "investigated",   521L,
                  "Wulu", "investigated",   581L,
  "Other (<500 rumours)",     "reported",   240L,
          "Gogrial West",     "reported",   621L,
                "Ikotos",     "reported",    85L,
             "Jur River",     "reported", 10006L,
            "Lopa/Lafon",     "reported",    68L,
                 "Torit",     "reported",   158L,
                   "Wau",     "reported",   233L,
                  "Wulu",     "reported",   150L
  )



district_order<- c("Jur River", "Torit", "Lopa/Lafon", "Other (<500 rumours)", "Ikotos", "Gogrial West", "Wulu", "Wau") 
district_order<- c("Other (<500 rumours)", "Wau", "Wulu", "Gogrial West", "Ikotos", "Lopa/Lafon", "Torit", "Jur River") 
type_color_order<- c(burnt_sienna, scooter, scooter_light)

rl3_rumours_region %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type=="rumours" ~scooter_light,
    type=="investigated" ~scooter,
    type=="reported" ~ burnt_sienna)) %>%
  mutate(region=fct_relevel(region, district_order),
         type_color=fct_relevel(type_color, type_color_order))%>%
  ggplot(aes(x=region, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.8, position=position_dodge(.8), alpha=.95)+
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", family="Source Sans Pro SemiBold")+
  geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.8, preserve="single"), na.rm=TRUE, color="black", hjust=-.2, size=10, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  #scale_y_continuous(breaks=seq(0, 3000, 1500))+
  scale_y_continuous(labels=comma, breaks=seq(0, 13000, 4000), limits = c(0,13000))+
  coord_flip()+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("r3_rumours_month_district.png",
       height = 10,
       width = 17)
