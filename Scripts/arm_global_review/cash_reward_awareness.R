# PURPOSE: Cash reward awareness graph for ARM
# AUTHOR: Cody Adelson | Data Manager
# LICENSE: MIT
# DATE: December 1, 2022

df_cra <- 
tibble::tribble(
          ~county,    ~risk_level,     ~type, ~people_surveyed, ~value,
      "Tonj East", "Risk Level 1",  "humans",              44L,   0.86,
           "Uror", "Risk Level 1",  "humans",              12L,   0.92,
  "Rumbek Centre", "Risk Level 2",  "humans",             100L,   0.71,
           "Leer", "Risk Level 3",  "humans",              31L,   0.03,
          "Torit", "Risk Level 3",  "humans",              77L,   0.92,
      "Tonj East", "Risk Level 1", "animals",              44L,   0.73,
           "Uror", "Risk Level 1", "animals",              12L,   0.42,
  "Rumbek Centre", "Risk Level 2", "animals",             100L,    0.2,
           "Leer", "Risk Level 3", "animals",              31L,      0,
          "Torit", "Risk Level 3", "animals",              77L,   0.77
  )


type_color_order<- c("#8980cb", "#c43d4d")

df_cra %>% 
  mutate(type_color = case_when(
    type == "animals" ~ moody_blue,
    type == "humans" ~ old_rose)) %>% 
  ggplot(aes(x=county, y=value, fill=type_color))+
  geom_col(width=.7, position=position_dodge(.7))+
  geom_text(aes(label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, hjust = -.1, family="Source Sans Pro SemiBold") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_xgrid()+
  coord_flip()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust= 2, size=12, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=12, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()


si_save("Images/2022_arm/cra_county", width = 5)  

#Uror
df_cra$alphayr<-as.factor(ifelse(df_cra$county %notin% c("Uror"), 0.6, 1))

df_cra %>% 
  mutate(type_color = case_when(
    type == "animals" ~ moody_blue,
    type == "humans" ~ old_rose)) %>% 
  ggplot(aes(x=county, y=value, fill=type_color))+
  geom_col(aes(alpha=factor(alphayr)), width=.7, position=position_dodge(.7))+
  geom_text(aes(alpha=factor(alphayr), label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, hjust = -.1, family="Source Sans Pro SemiBold") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_xgrid()+
  coord_flip()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust= 2, size=12, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=12, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cra_uror")  

# Tonj East
df_cra$alphayr<-as.factor(ifelse(df_cra$county %notin% c("Tonj East"), 0.6, 1))

df_cra %>% 
  mutate(type_color = case_when(
    type == "animals" ~ moody_blue,
    type == "humans" ~ old_rose)) %>% 
  ggplot(aes(x=county, y=value, fill=type_color))+
  geom_col(aes(alpha=factor(alphayr)), width=.7, position=position_dodge(.7))+
  geom_text(aes(alpha=factor(alphayr), label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, hjust = -.1, family="Source Sans Pro SemiBold") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_xgrid()+
  coord_flip()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust= 2, size=12, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=12, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cra_te")  

# Rumbek Centre
df_cra$alphayr<-as.factor(ifelse(df_cra$county %notin% c("Rumbek Centre"), 0.6, 1))

df_cra %>% 
  mutate(type_color = case_when(
    type == "animals" ~ moody_blue,
    type == "humans" ~ old_rose)) %>% 
  ggplot(aes(x=county, y=value, fill=type_color))+
  geom_col(aes(alpha=factor(alphayr)), width=.7, position=position_dodge(.7))+
  geom_text(aes(alpha=factor(alphayr), label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, hjust = -.1, family="Source Sans Pro SemiBold") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_xgrid()+
  coord_flip()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust= 2, size=12, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=12, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

si_save("Images/2022_arm/county_presentations/cra_rc") 

# Level three
df_cra$alphayr<-as.factor(ifelse(df_cra$county %notin% c("Leer", "Torit"), 0.6, 1))

df_cra %>% 
  mutate(type_color = case_when(
    type == "animals" ~ moody_blue,
    type == "humans" ~ old_rose)) %>% 
  ggplot(aes(x=county, y=value, fill=type_color))+
  geom_col(aes(alpha=factor(alphayr)), width=.7, position=position_dodge(.7))+
  geom_text(aes(alpha=factor(alphayr), label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, hjust = -.1, family="Source Sans Pro SemiBold") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_xgrid()+
  coord_flip()+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust= 2, size=12, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=12, family = "Source Sans Pro"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

si_save("Images/2022_arm/risk_level_3/cra_lvl_3", width = 5) 



df_cra_rl <-
  tibble::tribble(
       ~risk_level,     ~type, ~value,
    "Risk Level 1",  "humans",   0.84,
    "Risk Level 1", "animals",   0.66,
    "Risk Level 2",  "humans",   0.71,
    "Risk Level 2", "animals",    0.2,
    "Risk Level 3",  "humans",   0.67,
    "Risk Level 3", "animals",   0.55
    )



# County 

df_cra_rl %>% 
  mutate(type_color = case_when(
    type == "animals" ~ moody_blue,
    type == "humans" ~ old_rose)) %>% 
  ggplot(aes(x=risk_level, y=value, fill=type_color))+
  geom_col(width=.7, position=position_dodge(.7))+
  geom_text(aes(label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, hjust = -.1, family="Source Sans Pro SemiBold") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_xgrid()+
  scale_x_discrete(limits=rev) +
  coord_flip()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x  = element_text(vjust= 2, size=12, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=12, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

si_save("Images/2022_arm/cra_rl", width = 5)  


# Risk Level 3 

df_cra_rl$alphayr<-as.factor(ifelse(df_cra_rl$risk_level %notin% c("Risk Level 3"), 0.6, 1))

df_cra_rl %>% 
  mutate(type_color = case_when(
    type == "animals" ~ moody_blue,
    type == "humans" ~ old_rose)) %>% 
  ggplot(aes(x=risk_level, y=value, fill=type_color))+
  geom_col(aes(alpha=factor(alphayr)), width=.7, position=position_dodge(.7))+
  geom_text(aes(alpha=factor(alphayr), label=scales::percent(value, 1)), position=position_dodge2(width=.7, preserve="single"), na.rm=TRUE, hjust = -.1, family="Source Sans Pro SemiBold") +
  scale_y_continuous(labels = scales::percent, breaks=seq(0, 1, .25), limits=c(0, 1.1))+
  si_style_xgrid()+
  scale_x_discrete(limits=rev) +
  coord_flip()+
  labs(x = NULL, y = NULL)+
  scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none')+
  theme(axis.text.x  = element_text(vjust= 2, size=12, family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust=0.5, size=12, family = "Source Sans Pro" ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_identity()

si_save("Images/2022_arm/risk_level_3/cra_rl_level_3", width = 5)  
