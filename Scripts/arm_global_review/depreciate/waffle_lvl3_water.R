df_lvl3_water<-
tibble::tribble(
  ~n.bh, ~nw,   ~w,
   330L, 26L, 130L
  )



muat
waffle(df_lvl3_water, rows=27, size=1.25, flip=TRUE,
       colors= c(old_rose, burnt_sienna, denim), legend_pos = "none")

ggsave("Images/2022_arm/risk_level_3/water_rl3.png",
       height = 5,
       width = 10)

si_save("Images/2022_arm/county_presentations/average_animals_screened_fo_ee.png")