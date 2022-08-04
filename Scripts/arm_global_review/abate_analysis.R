# PURPOSE: Generate visualization for abate eligible and treated
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Dec 1, 2021
# NOTES: February 2022 update with dewormr dataset for Global Review

data_out <- "~/Github/pinchworm/Dataout"
month_order<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
status_order<-c("Targeted", "Eligible and Treated")
status_color_order<-c("#bfddff", "#2057a7")


df_abate<-df %>% 
  filter(indicator %in% c("abate_targeted", "abate_eligible")) %>% 
  mutate(indicator=case_when(
    indicator=="abate_targeted" ~ "Targeted",
    indicator=="abate_eligible" ~ "Eligible and Treated"),
    month=match(month, month.name),
    month=month.abb[month]) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  group_by(month) %>% 
  summarise(across(c("Eligible and Treated", Targeted), sum, na.rm=TRUE)) %>% 
  mutate(Targeted=as.numeric(Targeted),
         "Eligible and Treated"=as.numeric(`Eligible and Treated`),
         percent_treated=`Eligible and Treated`/Targeted,
         Targeted_Bar=Targeted-`Eligible and Treated`) %>% 
  pivot_longer(cols= c(`Eligible and Treated`, Targeted_Bar), names_to="indicator", values_to="value") %>%
  mutate(value = as.numeric(value)) %>%
  mutate(month=fct_relevel(month, month_order),
         status=fct_relevel(indicator, status_order),
         status_color=case_when(
           status=="Targeted_Bar" ~trolley_grey_light,
           status=="Eligible and Treated" ~denim),
         status_color=fct_relevel(status_color, c(trolley_grey_light, denim)),
         percent_treated=case_when(status=="Targeted_Bar" ~ NA_real_,
                                   TRUE~percent_treated),
         Targeted=case_when(status=="Targeted_Bar" ~ Targeted,
                            TRUE~NA_real_),
         eligible_treated=case_when(status=="Eligible and Treated" ~ value,
                                    TRUE~NA_real_),
         cases=case_when(month %in% c("Aug", "Oct") ~1,
                         month=="Jul" ~ 2,
                         TRUE ~ 0),
         cases_color=case_when(is.na(cases) | cases==0 ~ "#D9CDC3",
                               cases==1 ~ "#FDAC7A",
                               cases==2 ~ "#DA3C6A",
                               TRUE ~ "#A90773"),
         cases_color=factor(cases_color, c("#D9CDC3", "#FDAC7A", "#DA3C6A"))) %>% 
  write_xlsx(file.path(data_out, "abate_for_global_review.xlsx"))
  # group_by(month, indicator, status_color, percent) %>%
  # summarise(across(c(value), sum, na.rm=TRUE)) %>% 
  # View()

# df_abate<-
# tibble::tribble(
#                  ~status, ~month, ~value,
#               "Targeted",  "Jan",     0L,
#   "Eligible and Treated",  "Jan",    30L,
#               "Targeted",  "Feb",     0L,
#   "Eligible and Treated",  "Feb",    42L,
#               "Targeted",  "Mar",   177L,
#   "Eligible and Treated",  "Mar",    52L,
#               "Targeted",  "Apr",   170L,
#   "Eligible and Treated",  "Apr",    64L,
#               "Targeted",  "May",   138L,
#   "Eligible and Treated",  "May",   156L,
#               "Targeted",  "Jun",   162L,
#   "Eligible and Treated",  "Jun",   190L,
#               "Targeted",  "Jul",   245L,
#   "Eligible and Treated",  "Jul",   187L,
#               "Targeted",  "Aug",   273L,
#   "Eligible and Treated",  "Aug",   272L,
#               "Targeted",  "Sep",   400L,
#   "Eligible and Treated",  "Sep",   356L,
#               "Targeted",  "Oct",   494L,
#   "Eligible and Treated",  "Oct",   418L
#   )


df_abate %>% 
  #ggplot(aes(x=month, y=value, fill=scooter_med))+
  ggplot(aes(x=month, y=value, fill=status_color), leg)+
  geom_bar(position="stack", alpha=0, stat="identity", show.legend=FALSE)+
  geom_rect(aes(xmin = 5.5, xmax = 11.5, ymin = -Inf, ymax = Inf), alpha = 0.03, fill=denim_light, inherit.aes = TRUE)+
  geom_hline(aes(yintercept=250), colour=grey20k, size=.5)+
  geom_hline(aes(yintercept=500), colour=grey20k, size=.5)+
  geom_hline(aes(yintercept=750), colour=grey20k, size=.5)+
  geom_hline(aes(yintercept=1000), colour=grey20k, size=.5)+
  geom_bar(position="stack", alpha=.9, stat="identity", show.legend=FALSE)+
  geom_rug(aes(color = factor(cases_color)), size=2, sides="b", na.rm = TRUE) +
  #geom_col(alpha=.9, show.legend = FALSE)+
  geom_text(aes(x = month, y = Targeted, label = comma(Targeted, 1)), family="Source Sans Pro SemiBold", 
            color=grey80k, vjust = -.5) + 
  # geom_text(aes(label=Targeted), family="Source Sans Pro",
  #           color=grey80k, vjust=-3, na.rm=TRUE)+
  # geom_text(aes(label=eligible_treated), family="Source Sans Pro SemiBold",
  #           color=denim, vjust=-0.5, na.rm=TRUE)+
  geom_text(aes(x=month, y=0, label=percent(percent_treated, 1)), family="Source Sans Pro SemiBold",
            color="white", vjust=-0.2, na.rm=TRUE)+
  scale_y_continuous(labels=comma, breaks=seq(0, 1000, 250), limits=c(0,1200))+
  si_style_ygrid()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()+
  scale_color_identity()
  
  

si_save("Images/2022_gr/abate")  

ggsave("abate_eligible_fy21.png",
       height = 5,
       width = 10)
