df_lvl3_water<-
tibble::tribble(
  ~no.bh, ~nw,   ~w,
    192L, 17L, 131L
  )



muat
waffle(df_lvl3_water, rows=20, size=1.25, flip=TRUE,
       colors= c(old_rose, burnt_sienna, denim), legend_pos = "none")

ggsave("water_rl3.png",
       height = 5,
       width = 10)
