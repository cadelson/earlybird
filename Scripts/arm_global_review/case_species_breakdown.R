# PURPOSE: Generate visualization differentiating species with cases/infections/unemerged worms
# AUTHOR: Cody Adelson | Data Consultant
# LICENSE: MIT
# DATE: Nov 6, 2024
# NOTES: 

library(tidyverse)
library(glitr)
library(scales)
library(extrafont)
library(AzureStor)
library(systemfonts)

month_order<- c("J-F", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "N-D") 

# Grab and load recent cases from Azure
bl_endp_key <- storage_endpoint(Sys.getenv("CC_GW_AZURE_BLOB_URL"),
                                key = Sys.getenv("CC_GW_AZURE_KEY"))

cont <- storage_container(bl_endp_key, "gwep")

files_list <- list_storage_files(cont, "DEVELOPMENT/rds files")

rds_file_path <- "DEVELOPMENT/rds files/global_patient_animal_dataset.rds"

temp_rds <- tempfile(fileext = ".rds")
storage_download(cont, src = rds_file_path, dest = temp_rds, overwrite = TRUE)

df_cases_global <- readRDS(temp_rds)

df_cases_species <- df_cases_global %>% 
  rename(month = month_emerge,
         year = year_event) %>% 
  filter(country == "South Sudan",
         #snu2_detect %in% c("Lafon", "Lopa/Lafon"),
         year == 2024,
         first_worm %in% c(NA, "1")) %>%
  #filter(year == "2024", month == 8, host == "Human") %>% View()
  group_by(month, host, year) %>% 
  distinct(id_event) %>% 
  tally() %>% 
  ungroup() %>% 
  complete(month = 1:12, host, fill = list(n=0)) %>%
  rename(cases = n) %>% 
  group_by(host) %>% 
  mutate(host_label = paste0(host, " (n=", sum(cases), ")"),
         total_sort = case_when(
           host == "Human" ~ 7,
           host == "Cat" ~ 6,
           host == "Dog" ~ 5,
           host == "African Wild Cat" ~ 4,
           host == "Serval Cat" ~ 3,
           host == "Civet" ~ 2,
           host == "Genet" ~ 1)) %>% 
  ungroup() %>% 
  mutate(
    cases_label = case_when(
      cases > 0 ~ cases,
      TRUE ~ NA_real_),
    host_color = case_when(
      host == "Human" ~ old_rose,
      host %in% c("Cat", "Dog") ~ moody_blue,
      TRUE ~ scooter),
    month=month.abb[month],
    month = str_replace_all(month, "Jan|Feb", "J-F"),
    month = str_replace_all(month, "Nov|Dec", "N-D"),
    month=fct_relevel(month, month_order),
    host_label = factor(host_label, levels = unique(host_label[order(-total_sort)])))
    #host = factor(host_label, levels = c("Human", "Serval Cat", "Wild Cat", "Civet")))


df_cases_species %>% 
  ggplot(aes(x = month, y = cases)) +
  geom_col(aes(fill = host_color)) +
  geom_hline(yintercept = c(1, 2, 3, 4), linewidth = .3, colour="white")+
  # geom_text(aes(label = cases_label),
  #           family = "Source Sans Pro", size = 1.5, vjust = -.25) +
  #facet_wrap(~host, ncol = 1) +
  facet_grid(host_label ~ ., 
             switch = "y", 
             scales="free_y"
             )+
  si_style_ygrid() +
  scale_y_continuous(breaks=seq(0, 5, 2), limits = c(0, 5)) +
  labs(x = NULL, y = NULL,
       )+
  theme(axis.text.x  = element_text(vjust = 0.5, size = 32, 
                                    family = "Source Sans Pro"),
        axis.text.y  = element_text(vjust = 0.5, size = 28, 
                                    family = "Source Sans Pro"),
        strip.text = element_text(size = 40, 
                                  family = "Source Sans Pro",
                                  #face = "bold"
                                  ),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside"
        ) + 
  geom_rect(data = data.frame(Year = "Serval Cat (n=5)"), xmin = 2.5, xmax = 8.5, ymin = -Inf, ymax = Inf, alpha = 0.2, fill=old_rose_light, inherit.aes = FALSE)+
  scale_fill_identity()

ggsave("Images/2024_arm/gw_by_species.png",
       height = 14,
       width = 22)
