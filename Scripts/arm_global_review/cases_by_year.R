# PURPOSE: Generate visualization for number of cases by year for 2021 ARM
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: Nov 25, 2021
# NOTES: November 2022 - updated for 2022 ARM

df_year<-
  tibble::tribble(
    ~Year,  ~Cases,
    1993L,    2984,
    1994L,   53271,
    1995L,   64608,
    1996L,  118587,
    1997L,   43596,
    1998L,   47977,
    1999L,   66097,
    2000L,   54890,
    2001L,   49471,
    2002L,   41493,
    2003L,   20299,
    2004L,    7266,
    2005L,    5585,
    2006L,   20581,
    2007L,    5815,
    2008L,    3618,
    2009L,    2733,
    2010L,    1698,
    2011L,    1028,
    2012L,     520,
    2013L,     113,
    2014L,      70,
    2015L,       5,
    2016L,       6,
    2017L,       0,
    2018L,      10,
    2019L,       4,
    2020L,       1,
    2021L,       4,
    2022L,       6
    )

df_year %>% 
  summarise(across(c(Cases), sum, na.rm = TRUE))


# df_year %>% 
#   ggplot(aes(x=Year, y=Cases))+
#   geom_col(stat="sum", fill=scooter, alpha=.8, width=.85)+
  
df_year %>% 
  ggplot(aes(x=Year, y=Cases))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=1995, to=2021, by = 5), size=1, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=100000, by = 25000), size=.5, color="grey90")+
  geom_line(size=1.2, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.2, 
             colour = denim,
             stroke=1.2) +
  si_style_ygrid()+
  scale_y_continuous(labels = label_number(suffix="K", scale=1e-3))+
  scale_x_continuous(breaks=seq(1995, 2021, 5))+
  labs(x = NULL, y = NULL, color = NULL)+
  theme(axis.text.y  = element_text( size= 15, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 15, family = "Source Sans Pro", margin = margin(t = -.18, unit = "in")),
        axis.ticks.x = element_blank())

si_save("Images/2022_arm/gw_yearly_cases.png")

df_year %>% 
  filter(Year>2014) %>% 
  ggplot(aes(x=Year, y=Cases))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2015, to=2021, by = 1), size=.5, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=10, by = 5), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white",
             shape = 21,
             size = 2,
             colour = denim,
             stroke=2) +
  si_style_ygrid()+
  scale_y_continuous(labels = number_format(accuracy=1), breaks=seq(from=0, to=10, by=5), limits=c(0, 10))+
  scale_x_continuous(breaks=seq(2015, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL, subtitle="2015-2022", family="Source Sans Pro")+
  scale_size_area()+
  theme(axis.text = element_text(size = 24),
        plot.subtitle = element_text(size=28, hjust=.15, vjust=3))

si_save("Images/2022_arm/cases_yearly_recent.png")

#########################

#For Specific Counties

############Tonj East
df_patient_data %>% 
  filter(county == "Tonj East",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2021, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=450, by = 100), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_te.png")


#Smaller graph
df_patient_data %>% 
  filter(county == "Tonj East",
         first_worm == 1, 
         year > 2011) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2012:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2012, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=3, by = 1), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2012, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 20, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 18, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))
si_save("Images/2022_arm/county_presentations/yearly_cases_te_small.png")




############Uror
df_patient_data %>% 
  filter(county == "Uror",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=200, by = 50), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_ur.png")


#Smaller graph
df_patient_data %>% 
  filter(county == "Uror",
         first_worm == 1, 
         year > 2011) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2012:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2012, to=2021, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=3, by = 1), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2012, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 20, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 18, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))
si_save("Images/2022_arm/county_presentations/yearly_cases_ur_small.png")


############Rumbek North
df_patient_data %>% 
  filter(county == "Rumbek North",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  View()
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=20, by = 5), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_rn.png")

############ Awerial
df_patient_data %>% 
  filter(county == "Awerial",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=400, by = 100), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_aw.png")


#Smaller graph
df_patient_data %>% 
  filter(county == "Awerial",
         first_worm == 1, 
         year > 2011) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2012:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2012, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to= 15, by = 5), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2012, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 20, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 18, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))
si_save("Images/2022_arm/county_presentations/yearly_cases_aw_small.png")


############ Lopa/Lafon
df_patient_data %>% 
  filter(county == "Lopa/Lafon",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=3, by = 1), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_ll.png")


#Smaller graph
df_patient_data %>% 
  filter(county == "Awerial",
         first_worm == 1, 
         year > 2011) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2012:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2012, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to= 15, by = 5), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2012, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 20, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 18, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))
si_save("Images/2022_arm/county_presentations/yearly_cases_aw_small.png")



############ Nyirol
df_patient_data %>% 
  filter(county == "Nyirol",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from = 0, to = 100, by = 25), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_ny.png")


#Smaller graph
df_patient_data %>% 
  filter(county == "Nyirol",
         first_worm == 1, 
         year > 2011) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2012:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2012, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to= 3, by = 1), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2012, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 30, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 36, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))
si_save("Images/2022_arm/county_presentations/yearly_cases_ny_small.png", width =14)


############ Akobo and Nyirol
df_patient_data %>% 
  filter(county %in% c("Akobo","Nyirol"),
         first_worm == 1) %>%
  group_by(year, county) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(county, year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  facet_wrap(~county, ncol =1) +
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from = 0, to = 100, by = 25), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 15, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 12, family = "Source Sans Pro" ),
        strip.text = element_text(size = 20, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_ak_ny.png")

############ Tonj North
df_patient_data %>% 
  filter(county == "Tonj North",
         first_worm == 1) %>%
  #group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from = 0, to = 800, by = 200), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_tn.png")

#Smaller graph
df_patient_data %>% 
  filter(county == "Tonj North",
         first_worm == 1, 
         year > 2011) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2012:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2012, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to= 8, by = 2), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2012, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 20, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 18, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))
si_save("Images/2022_arm/county_presentations/yearly_cases_tn_small.png")

############ Rumbek Centre
df_patient_data %>% 
  filter(county == "Rumbek Centre",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from = 0, to = 6, by = 2), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_rc.png")

############ Terekeka
df_patient_data %>% 
  filter(county == "Terekeka",
         first_worm == 1) %>%
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year),
         n = as.numeric(n)) %>%
  complete(year = 2007:2022, fill = list(n=0)) %>%
  ggplot(aes(x=year, y=n))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2007, to=2022, by = 1), size=.75, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from = 0, to = 300, by = 100), size=.5, color="grey90")+
  geom_line(size=1, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 1.5, 
             colour = denim,
             stroke=1.5) +
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2007, 2022, 1))+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size = 13, family = "Source Sans Pro", vjust = 3),
        axis.text.y  = element_text(size = 10, family = "Source Sans Pro" ),
        strip.text = element_text(hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0, vjust=0),
        plot.subtitle = element_text(hjust=0, vjust=0))


si_save("Images/2022_arm/county_presentations/yearly_cases_ter.png")




df_year_region %>% 
  filter(location=="Uror") %>% 
  ggplot(aes(x=year, y=value))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2006, to=2021, by = 1), size=2, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=180, by = 50), size=.5, color="grey90")+
  geom_line(size=2.5, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 3, 
             colour = denim,
             stroke=3) +
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, size=12, segment.color = 'transparent', color="grey30", nudge_y=15, family="Source Sans Pro SemiBold",)+
  #geom_text(aes(label=value), na.rm=TRUE, color=grey80k, vjust=-1.5, size=16, family="Source Sans Pro SemiBold")+
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2006, 2021, 1))+
  #geom_text(aes(label=(Cases), color=grey70k, vjust=-1),na.rm=TRUE)+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text( size=32, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=42, hjust=0, vjust=0),
        plot.subtitle = element_text(size=38, hjust=0, vjust=0))
theme(axis.text = element_text(size = 12))

ggsave("gw_yearly_cases_UR.png",
       height = 14,
       width = 24)

df_year_region %>% 
  filter(location=="Rumbek North") %>% 
  ggplot(aes(x=year, y=value))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2006, to=2021, by = 1), size=2, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=20, by = 5), size=.5, color="grey90")+
  geom_line(size=2.5, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 3, 
             colour = denim,
             stroke=3) +
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, size=12, segment.color = 'transparent', color="grey30", nudge_y=1.5, family="Source Sans Pro SemiBold",)+
  #geom_text(aes(label=value), na.rm=TRUE, color=grey80k, vjust=-1.5, size=16, family="Source Sans Pro SemiBold")+
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2006, 2021, 1))+
  #geom_text(aes(label=(Cases), color=grey70k, vjust=-1),na.rm=TRUE)+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text( size=32, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=42, hjust=0, vjust=0),
        plot.subtitle = element_text(size=38, hjust=0, vjust=0))
theme(axis.text = element_text(size = 12))

ggsave("gw_yearly_cases_RN.png",
       height = 14,
       width = 24)

df_year_region %>% 
  filter(location=="Awerial") %>% 
  ggplot(aes(x=year, y=value))+
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2006, to=2021, by = 1), size=2, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=450, by = 50), size=.5, color="grey90")+
  geom_line(size=2.5, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 3, 
             colour = denim,
             stroke=3) +
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, size=12, segment.color = 'transparent', color="grey30", nudge_y=35, family="Source Sans Pro SemiBold",)+
  #geom_text(aes(label=value), na.rm=TRUE, color=grey80k, vjust=-1.5, size=16, family="Source Sans Pro SemiBold")+
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2006, 2021, 1))+
  #geom_text(aes(label=(Cases), color=grey70k, vjust=-1),na.rm=TRUE)+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text( size=32, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=42, hjust=0, vjust=0),
        plot.subtitle = element_text(size=38, hjust=0, vjust=0))
theme(axis.text = element_text(size = 12))

ggsave("gw_yearly_cases_AW.png",
       height = 14,
       width = 24)


##### Facets for level two districts

df_lvl2_cases<-
  tibble::tribble(
            ~region, ~year, ~value,
       "Tonj North", 2006L,   126L,
       "Tonj North", 2007L,   307L,
       "Tonj North", 2008L,   577L,
       "Tonj North", 2009L,   704L,
       "Tonj North", 2010L,   314L,
       "Tonj North", 2011L,    49L,
       "Tonj North", 2012L,     6L,
       "Tonj North", 2013L,     0L,
       "Tonj North", 2014L,     0L,
       "Tonj North", 2015L,     0L,
       "Tonj North", 2016L,     0L,
       "Tonj North", 2017L,     0L,
       "Tonj North", 2018L,     1L,
       "Tonj North", 2019L,     0L,
       "Tonj North", 2020L,     0L,
       "Tonj North", 2021L,     0L,
       "Tonj South", 2006L,    22L,
       "Tonj South", 2007L,    62L,
       "Tonj South", 2008L,    61L,
       "Tonj South", 2009L,    68L,
       "Tonj South", 2010L,    77L,
       "Tonj South", 2011L,    11L,
       "Tonj South", 2012L,     1L,
       "Tonj South", 2013L,     0L,
       "Tonj South", 2014L,     0L,
       "Tonj South", 2015L,     0L,
       "Tonj South", 2016L,     0L,
       "Tonj South", 2017L,     0L,
       "Tonj South", 2018L,     0L,
       "Tonj South", 2019L,     0L,
       "Tonj South", 2020L,     1L,
       "Tonj South", 2021L,     0L,
            "Akobo", 2006L,    44L,
            "Akobo", 2007L,    68L,
            "Akobo", 2008L,     7L,
            "Akobo", 2009L,     0L,
            "Akobo", 2010L,     0L,
            "Akobo", 2011L,     0L,
            "Akobo", 2012L,     0L,
            "Akobo", 2013L,     0L,
            "Akobo", 2014L,     0L,
            "Akobo", 2015L,     0L,
            "Akobo", 2016L,     0L,
            "Akobo", 2017L,     0L,
            "Akobo", 2018L,     0L,
            "Akobo", 2019L,     0L,
            "Akobo", 2020L,     0L,
            "Akobo", 2021L,     0L,
           "Nyirol", 2006L,   153L,
           "Nyirol", 2007L,    88L,
           "Nyirol", 2008L,    25L,
           "Nyirol", 2009L,     2L,
           "Nyirol", 2010L,     0L,
           "Nyirol", 2011L,     0L,
           "Nyirol", 2012L,     0L,
           "Nyirol", 2013L,     1L,
           "Nyirol", 2014L,     0L,
           "Nyirol", 2015L,     0L,
           "Nyirol", 2016L,     0L,
           "Nyirol", 2017L,     0L,
           "Nyirol", 2018L,     1L,
           "Nyirol", 2019L,     0L,
           "Nyirol", 2020L,     0L,
           "Nyirol", 2021L,     0L,
    "Rumbek Center", 2006L,    59L,
    "Rumbek Center", 2007L,     3L,
    "Rumbek Center", 2008L,     0L,
    "Rumbek Center", 2009L,     1L,
    "Rumbek Center", 2010L,     0L,
    "Rumbek Center", 2011L,     0L,
    "Rumbek Center", 2012L,     0L,
    "Rumbek Center", 2013L,     0L,
    "Rumbek Center", 2014L,     0L,
    "Rumbek Center", 2015L,     0L,
    "Rumbek Center", 2016L,     0L,
    "Rumbek Center", 2017L,     0L,
    "Rumbek Center", 2018L,     4L,
    "Rumbek Center", 2019L,     0L,
    "Rumbek Center", 2020L,     0L,
    "Rumbek Center", 2021L,     0L,
             "Wulu", 2006L,    20L,
             "Wulu", 2007L,     0L,
             "Wulu", 2008L,     0L,
             "Wulu", 2009L,     0L,
             "Wulu", 2010L,     1L,
             "Wulu", 2011L,     0L,
             "Wulu", 2012L,     0L,
             "Wulu", 2013L,     0L,
             "Wulu", 2014L,     1L,
             "Wulu", 2015L,     0L,
             "Wulu", 2016L,     0L,
             "Wulu", 2017L,     0L,
             "Wulu", 2018L,     0L,
             "Wulu", 2019L,     0L,
             "Wulu", 2020L,     0L,
             "Wulu", 2021L,     0L
    )

df_lvl2_cases %>% 
  filter(region %in% c("Tonj North", "Tonj South")) %>% 
  ggplot(aes(x=year, y=value))+
  facet_wrap(~region, scales="free_y", ncol=1) +
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2006, to=2021, by = 1), size=2, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=450, by = 100), size=.5, color="grey90")+
  geom_line(size=2.5, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 3, 
             colour = denim,
             stroke=3) +
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, size=12, segment.color = 'transparent', color="grey30", nudge_y=50, family="Source Sans Pro SemiBold",)+
  #geom_text(aes(label=value), na.rm=TRUE, color=grey80k, vjust=-1.5, size=16, family="Source Sans Pro SemiBold")+
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2006, 2021, 1))+
  #geom_text(aes(label=(Cases), color=grey70k, vjust=-1),na.rm=TRUE)+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text( size=32, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=42, hjust=0, vjust=0),
        plot.subtitle = element_text(size=38, hjust=0, vjust=0))
theme(axis.text = element_text(size = 12))

ggsave("yearly_cases_lvl2_TE.png",
       height = 14,
       width = 24)

df_lvl2_cases %>% 
  filter(region %in% c("Akobo", "Nyirol")) %>% 
  ggplot(aes(x=year, y=value))+
  facet_wrap(~region, scales="free_y", ncol=1) +
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2006, to=2021, by = 1), size=2, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=200, by = 50), size=.5, color="grey90")+
  geom_line(size=2.5, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 3, 
             colour = denim,
             stroke=3) +
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, size=12, segment.color = 'transparent', color="grey30", nudge_y=50, family="Source Sans Pro SemiBold",)+
  #geom_text(aes(label=value), na.rm=TRUE, color=grey80k, vjust=-1.5, size=16, family="Source Sans Pro SemiBold")+
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2006, 2021, 1))+
  #geom_text(aes(label=(Cases), color=grey70k, vjust=-1),na.rm=TRUE)+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text( size=32, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=42, hjust=0, vjust=0),
        plot.subtitle = element_text(size=38, hjust=0, vjust=0))
theme(axis.text = element_text(size = 12))

ggsave("yearly_cases_lvl2_UR.png",
       height = 14,
       width = 24)

df_lvl2_cases %>% 
  filter(region %in% c("Rumbek Center", "Wulu")) %>% 
  ggplot(aes(x=year, y=value))+
  facet_wrap(~region, scales="free_y", ncol=1) +
  geom_area(alpha=.5, fill=denim_light)+
  geom_vline(xintercept = seq(from=2006, to=2021, by = 1), size=2, color="white", linetype="dotted")+
  geom_hline(yintercept = seq(from=0, to=75, by = 25), size=.5, color="grey90")+
  geom_line(size=2.5, colour=denim)+
  geom_point(fill = "white", 
             shape = 21, 
             size = 3, 
             colour = denim,
             stroke=3) +
  #geom_text_repel(aes(label=comma(round(value), accuracy=1)), na.rm=TRUE, size=12, segment.color = 'transparent', color="grey30", nudge_y=50, family="Source Sans Pro SemiBold",)+
  #geom_text(aes(label=value), na.rm=TRUE, color=grey80k, vjust=-1.5, size=16, family="Source Sans Pro SemiBold")+
  si_style_ygrid()+
  scale_x_continuous(breaks=seq(2006, 2021, 1))+
  #geom_text(aes(label=(Cases), color=grey70k, vjust=-1),na.rm=TRUE)+
  labs(x = NULL, y = NULL, color = NULL)+
  scale_size_area()+
  theme(axis.text.x  = element_text(size=32, family = "Source Sans Pro"),
        axis.text.y  = element_text( size=32, family = "Source Sans Pro" ),
        strip.text = element_text(size = 38, hjust=.02, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=42, hjust=0, vjust=0),
        plot.subtitle = element_text(size=38, hjust=0, vjust=0))
theme(axis.text = element_text(size = 12))

ggsave("yearly_cases_lvl2_RN.png",
       height = 14,
       width = 24)

