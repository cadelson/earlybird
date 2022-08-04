#Notes: Updated February 2022 with Dewormr data for cumulative 2021

rl1<-c("Uror", "Rumbek North", "Tonj East", "Awerial")
rl2<-c("Nyirol", "Tonj North", "Tonj South", "Cueibet", "Rumbek Centre", "Mayendit",
       "Panyijiar", "Yirol East", "Yirol West", "Terekeka")

df_cr<-df %>% 
  filter(indicator=="cr_reached",
         month!="Cumulative") %>% 
  mutate(rl=case_when(
    county %in% rl1 ~ "Risk Level 1 (n=4)",
    county %in% rl2 ~ "Risk Level 2 (n=8)",
    TRUE ~ "Risk Level 3 (n=25)")) %>% 
  group_by(rl, month) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  mutate(month=match(month, month.name),
         month=month.abb[month],
         month_short = substr(month,1,1),
         value_lab=paste((round(value/1000)), "k", sep=""))
  
  

# df_cr<-
# tibble::tribble(
#   ~rl, ~month,      ~value,
#    1L,  "Jan", 3713.193548,
#    1L,  "Feb", 4265.785714,
#    1L,  "Mar", 2935.096774,
#    1L,  "Apr", 3646.733333,
#    1L,  "May",        3494,
#    1L,  "Jun", 3946.866667,
#    1L,  "Jul", 3035.483871,
#    1L,  "Aug", 2987.129032,
#    1L,  "Sep", 2695.833333,
#    1L,  "Oct", 3154.709677,
#    2L,  "Jan", 2702.709677,
#    2L,  "Feb", 2749.642857,
#    2L,  "Mar",        2765,
#    2L,  "Apr", 2736.033333,
#    2L,  "May", 1989.806452,
#    2L,  "Jun",      2179.4,
#    2L,  "Jul", 2595.677419,
#    2L,  "Aug", 2032.903226,
#    2L,  "Sep", 1995.333333,
#    2L,  "Oct", 1854.483871,
#    3L,  "Jan", 4408.193548,
#    3L,  "Feb", 4711.928571,
#    3L,  "Mar", 3936.387097,
#    3L,  "Apr", 4091.633333,
#    3L,  "May", 4528.387097,
#    3L,  "Jun", 4279.566667,
#    3L,  "Jul", 4888.774194,
#    3L,  "Aug", 4503.387097,
#    3L,  "Sep",      5014.8,
#    3L,  "Oct", 4576.322581
#   )




month_order<- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December") 
month_order_ab<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
rl_order<- c(3, 2, 1)
rl_color_order<- c(genoa, golden_sand, old_rose)

# df_cr %>% 
#   mutate(rl = as.character(rl)) %>% 
#   mutate(rl_color=case_when(
#     rl=="1" ~old_rose,
#     rl=="2" ~golden_sand,
#     rl=="3" ~ genoa)) %>%
#   mutate(month=fct_relevel(month, month_order),
#          rl_color=fct_relevel(rl_color, rl_color_order))%>%
#   ggplot(aes(x=month, y=value, fill=rl_color))+
#   # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
#   geom_col(width=.7, position=position_dodge(.7), alpha=.8)+
#   # geom_text_repel(aes(label=comma(round(Cases), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", nudge_y=8000, nudge_x=.1, family="Source Sans Pro SemiBold",
#   #            data = . %>%
#   #              filter(row_number() %% 5 == 3))+
#   #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
#   geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
#   scale_y_continuous(breaks=seq(0, 10000, 1500))+
#   si_style_ygrid()+
#   labs(x = NULL, y = NULL)+
#   theme(axis.text.x  = element_text(vjust=0.5, size=32, family = "Source Sans Pro"),
#         axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
#         strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
#         axis.line.x = element_blank(),
#         axis.ticks.x = element_blank())+
#   scale_fill_identity()
# 
# ggsave("cr_by_month.png",
#        height = 14,
#        width = 24)

early_year <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")
late_year<- c("Sep", "Oct", "Nov", "Dec")

#Updated with Dewormr - Feb 2022
# df_cr %>% 
#   mutate(rl = as.character(rl)) %>% 
#   mutate(rl_color=case_when(
#     rl=="1" & month %in% c(early_year) ~old_rose_light,
#     rl=="1" & month %in% c(late_year) ~old_rose,
#     rl=="2" ~golden_sand,
#     rl=="3" & month %in% c(early_year) ~ genoa_light,
#     rl=="3" & month %in% c(late_year) ~ genoa,
#     TRUE~NA)) %>%
#   mutate(month=fct_relevel(month, month_order_ab),
#          rl_color=fct_relevel(rl_color, rl_color_order))%>%
#   ggplot(aes(x=month, y=value, fill=rl_color))+
#   geom_col(width=.7, position=position_dodge(.7), alpha=.8)+
#   facet_wrap(~rl)+
#   geom_text(aes(label=value_lab), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=5, family="Source Sans Pro SemiBold")+
#   #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
#   scale_y_continuous(breaks=seq(0, 200000, 50000), limits=c(0, 175000), labels=label_number(suffix="k", scale = 1e-3))+
#   si_style_ygrid()+
#   labs(x = NULL, y = NULL)+
#   theme(axis.text.x  = element_text(vjust=3, size=32, family = "Source Sans Pro"),
#         axis.text.y  = element_text(vjust=0.5, size=26, family = "Source Sans Pro" ),
#         strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
#         axis.line.x = element_blank(),
#         axis.ticks.x = element_blank())+
#   #theme(axis.text.y = element_text(margin = margin(r = 0)))
#   scale_fill_identity()

df_cr %>% 
  mutate(rl = as.character(rl)) %>% 
  mutate(rl_color=case_when(
    rl=="Risk Level 1 (n=4)" & month %in% c(early_year) ~old_rose_light,
    rl=="Risk Level 1 (n=4)" & month %in% c(late_year) ~old_rose,
    rl=="Risk Level 2 (n=8)" ~golden_sand_light,
    rl=="Risk Level 3 (n=25)" & month %in% c(early_year) ~ genoa_light,
    rl=="Risk Level 3 (n=25)" & month %in% c(late_year) ~ genoa)) %>%
  mutate(month=fct_relevel(month, month_order_ab),
         rl_color=fct_relevel(rl_color, rl_color_order),
         cases=case_when(month %in% c("Aug", "Oct") ~1,
                         month=="Jul" ~ 2,
                         TRUE ~ 0),
         cases_color=case_when(is.na(cases) | cases==0 ~ "#D9CDC3",
                               cases==1 ~ "#FDAC7A",
                               cases==2 ~ "#DA3C6A",
                               TRUE ~ "#A90773"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A")))%>%
  ggplot(aes(x=month, y=value, fill=rl_color))+
  geom_col(width=.7, position=position_dodge(.7))+
  #geom_col(width=.7, position=position_dodge(.7), alpha=.8)+
  facet_wrap(~rl)+
  geom_text(aes(label=value_lab), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=2.5, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  scale_y_continuous(breaks=seq(0, 175000, 50000), limits=c(0, 175000), labels=label_number(suffix="k", scale = 1e-3))+
  scale_x_discrete(labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))+
  geom_rug(aes(color = factor(cases_color)), size=2, sides="b", na.rm = TRUE) +
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, family = "Source Sans Pro" ),
        strip.text = element_text(size=16, hjust=.02, vjust=-.8, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  #theme(axis.text.y = element_text(margin = margin(r = 0)))
  scale_fill_identity()+
  scale_color_identity()

si_save("Images/2022_gr/cr_month")  

ggsave("cr_by_month_fy21.png",
       height = 9,
       width = 17)

df_cr_waf<-df_cr %>% 
  group_by(rl) %>% 
  summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  group_by(rl) %>% #do calculations by siteID
  ungroup() %>% 
  mutate(value = round(value / sum(value) * 100)) %>% 
  pivot_wider(names_from=rl, values_from=value)

waffle(df_cr_waf, rows=10, size=1.25, flip=TRUE, reverse=TRUE,
       colors= c("#D06471", "#F5C966", "#53968C"), legend_pos = "none")

ggsave("cr_gr_waffle.png",
       height = 7,
       width = 7)

df_cr_region<-
tibble::tribble(
         ~region, ~month, ~value,
  "Rumbek North",  "Jan", 41321L,
  "Rumbek North",  "Feb", 27405L,
  "Rumbek North",  "Mar", 28068L,
  "Rumbek North",  "Apr", 24486L,
  "Rumbek North",  "May", 35137L,
  "Rumbek North",  "Jun", 40340L,
  "Rumbek North",  "Jul", 29610L,
  "Rumbek North",  "Aug", 29460L,
  "Rumbek North",  "Sep", 23094L,
  "Rumbek North",  "Oct", 23628L,
     "Tonj East",  "Jan", 52708L,
     "Tonj East",  "Feb", 57927L,
     "Tonj East",  "Mar", 46931L,
     "Tonj East",  "Apr", 74093L,
     "Tonj East",  "May", 52165L,
     "Tonj East",  "Jun", 62055L,
     "Tonj East",  "Jul", 53880L,
     "Tonj East",  "Aug", 48473L,
     "Tonj East",  "Sep", 45840L,
     "Tonj East",  "Oct", 42927L,
          "Uror",  "Jan", 18014L,
          "Uror",  "Feb", 34110L,
          "Uror",  "Mar", 15989L,
          "Uror",  "Apr", 10823L,
          "Uror",  "May", 20416L,
          "Uror",  "Jun", 16011L,
          "Uror",  "Jul", 10610L,
          "Uror",  "Aug", 14668L,
          "Uror",  "Sep", 11941L,
          "Uror",  "Oct", 31241L
  )



df_cr_region %>% 
  filter(region=="Tonj East") %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=14, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 60000, 20000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=36, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=30, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_TE.png",
       height = 12,
       width = 20)

df_cr_region %>% 
  filter(region=="Uror")%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=14, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 30000, 5000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=36, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=30, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_UR.png",
       height = 12,
       width = 20)

df_cr_region %>% 
  filter(region=="Rumbek North")%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=14, family="Source Sans Pro SemiBold")+
  scale_y_continuous(label=comma, breaks=seq(0, 50000, 10000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=36, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=30, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_RN.png",
       height = 12,
       width = 20)


df_cr_r2<-
tibble::tribble(
             ~region, ~month, ~value,
             "Akobo",  "Jan",  8697L,
             "Akobo",  "Feb",  8975L,
             "Akobo",  "Mar", 11620L,
             "Akobo",  "Apr",  7289L,
             "Akobo",  "May",  1572L,
             "Akobo",  "Jun",  1923L,
             "Akobo",  "Jul",  4308L,
             "Akobo",  "Aug",  1269L,
             "Akobo",  "Sep",  1488L,
             "Akobo",  "Oct",  2527L,
            "Nyirol",  "Jan", 18152L,
            "Nyirol",  "Feb", 16123L,
            "Nyirol",  "Mar", 22080L,
            "Nyirol",  "Apr", 15702L,
            "Nyirol",  "May", 15936L,
            "Nyirol",  "Jun", 16093L,
            "Nyirol",  "Jul", 15176L,
            "Nyirol",  "Aug", 12358L,
            "Nyirol",  "Sep", 13559L,
            "Nyirol",  "Oct", 15620L,
     "Rumbek Center",  "Jan", 13393L,
     "Rumbek Center",  "Feb",  9537L,
     "Rumbek Center",  "Mar", 19938L,
     "Rumbek Center",  "Apr", 16501L,
     "Rumbek Center",  "May", 16998L,
     "Rumbek Center",  "Jun", 17031L,
     "Rumbek Center",  "Jul", 17222L,
     "Rumbek Center",  "Aug", 19885L,
     "Rumbek Center",  "Sep", 18853L,
     "Rumbek Center",  "Oct", 18182L,
        "Tonj North",  "Jan", 34377L,
        "Tonj North",  "Feb", 29498L,
        "Tonj North",  "Mar", 22943L,
        "Tonj North",  "Apr", 34533L,
        "Tonj North",  "May", 18356L,
        "Tonj North",  "Jun", 23213L,
        "Tonj North",  "Jul", 37516L,
        "Tonj North",  "Aug", 29508L,
        "Tonj North",  "Sep", 20301L,
        "Tonj North",  "Oct", 15288L,
        "Tonj South",  "Jan",  9165L,
        "Tonj South",  "Feb", 12857L,
        "Tonj South",  "Mar",  9134L,
        "Tonj South",  "Apr",  8056L,
        "Tonj South",  "May",  8822L,
        "Tonj South",  "Jun",  7122L,
        "Tonj South",  "Jul",  6244L,
        "Tonj South",  "Aug",  5143L,
        "Tonj South",  "Sep",  5659L,
        "Tonj South",  "Oct",  5872L,
              "Wulu",  "Jan",  5302L,
              "Wulu",  "Feb",  5464L,
              "Wulu",  "Mar",  4766L,
              "Wulu",  "Apr",  5544L,
              "Wulu",  "May",  6066L,
              "Wulu",  "Jun",  4763L,
              "Wulu",  "Jul",  5124L,
              "Wulu",  "Aug",  4949L,
              "Wulu",  "Sep",  5242L,
              "Wulu",  "Oct",  4277L
     )

df_cr_r2 %>% 
  filter(region %in% c("Akobo", "Nyirol"))%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  facet_wrap(~region, ncol=1)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 25000, 5000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l2_UR.png",
       height = 16,
       width = 20)

df_cr_r2 %>% 
  filter(region %in% c("Tonj North", "Tonj South"))%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  facet_wrap(~region, ncol=1)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 30000, 10000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l2_TE.png",
       height = 16,
       width = 20)

df_cr_r2 %>% 
  filter(region %in% c("Rumbek Center", "Wulu"))%>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  facet_wrap(~region, ncol=1)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 40000, 5000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l2_RN.png",
       height = 18,
       width = 20)


df_cr_r3<-
  tibble::tribble(
    ~month,  ~value,
     "Jan", 136654L,
     "Feb", 131934L,
     "Mar", 122028L,
     "Apr", 122749L,
     "May", 140380L,
     "Jun", 128387L,
     "Jul", 151552L,
     "Aug", 139605L,
     "Sep", 150444L,
     "Oct", 141866L
    )

month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")

df_cr_r3 %>% 
  mutate(value=as.numeric(value),
         month=fct_relevel(month, month_order)) %>% 
  ggplot(aes(x=month, y=value))+
  geom_col(aes(fill=old_rose_light, width=.85), show.legend = FALSE)+
  geom_text(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, color=trolley_grey, vjust=-.12, size=11, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels=comma, breaks=seq(0, 200000, 50000), limits=c(0, 175000))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=39, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=33, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_color_identity()+
  scale_fill_identity()

ggsave("cr_l3.png",
       height = 10,
       width = 19)

df_cr_aware_rl3<-
tibble::tribble(
                 ~region,     ~type, ~value,
            "Duk (n=35)",  "humans",      0,
     "Jur River (n=120)",  "humans",   0.98,
  "Kapoeta North (n=59)",  "humans",   0.85,
     "Lopa/Lafon (n=50)",  "humans",   0.92,
  "Rumbek Center (n=20)",  "humans",   0.75,
           "Wulu (n=15)",  "humans",      1,
            "Duk (n=35)", "animals",      0,
     "Jur River (n=120)", "animals",   0.92,
  "Kapoeta North (n=59)", "animals",   0.34,
     "Lopa/Lafon (n=50)", "animals",   0.82,
  "Rumbek Center (n=20)", "animals",   0.55,
           "Wulu (n=15)", "animals",   0.67
  )


type_color_order<- c("#287c6f", "#2057a7")

df_cr_aware_rl3 %>% 
  mutate(rl = as.character(region),
         value=as.numeric(value)) %>% 
  mutate(type_color=case_when(
    type=="animals" ~denim,
    type=="humans" ~genoa)) %>% 
  #mutate(type_color=fct_relevel(type, type_color_order))%>%
  ggplot(aes(x=region, y=value, fill=type_color))+
  # geom_col(position=position_dodge2(width=1, preserve="single"), alpha=.9)+
  geom_col(width=.7, position=position_dodge(.7), alpha=.9)+
  # geom_text_repel(aes(label=comma(round(Cases), accuracy=1)), na.rm=TRUE, segment.color = 'transparent', color="grey30", nudge_y=8000, nudge_x=.1, family="Source Sans Pro SemiBold",
  #            data = . %>%
  #              filter(row_number() %% 5 == 3))+
  geom_text(aes(label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, color="black", hjust=-.05, size=8, family="Source Sans Pro SemiBold")+
  #geom_text(aes(label=comma(round(value), accuracy=1)), position=position_dodge2(width=1, preserve="single"), na.rm=TRUE, color=trolley_grey, vjust=-.5, size=7, family="Source Sans Pro SemiBold")+
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_ygrid()+
  coord_flip()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust=0.5, size=24, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=24, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

ggsave("cr_aware.png",
       height = 12,
       width = 10)
