library(tidyverse)
library(sf)


state_01 <- st_read("data-parts/01.shp")


state_01_clean <- state_01 %>%
  st_set_geometry(NULL) %>%
  group_by(blockfp = blck_fp, tractfp = trct_fp, cntyfp = cnty_fp, cnty_name = NAMELSA, statefp = STATEFP) %>%
  summarise(avg_d_mbps = weighted.mean(avg_d_k, tests) / 1000,
            avg_u_mbps = weighted.mean(avg__kb, tests) / 1000,
            tests = sum(tests),
            devices = sum(devices)) %>%
  ungroup() %>%
  mutate(avg_d_mbps_cat = cut(avg_d_mbps, c(0, 10, 25, 100, max(max(avg_d_mbps), 101)), ordered_result = TRUE))


blocks <- blocks(state = "01")

blocks_data <- blocks %>%
  select(blockfp = GEOID10) %>%
  full_join(state_01_clean)



blocks_data %>%
  filter(cnty_name == "Autauga County") %>%
  ggplot() +
  geom_sf(data = blocks %>% filter(COUNTYFP10 == "001"), fill = "gray98", color = "gray50", lwd = 0.1) +
  geom_sf(aes(fill = avg_d_mbps_cat), color = "white", lwd = 0.05) +
  # geom_sf(data = states_base, color = "gray40", fill = NA, lwd = 0.2) +
  # geom_text_repel(data = label_cities(), aes(label = city, x = X, y = Y), size = 4.2, color = "gray30", family = "IBM Plex Mono") +
  scale_fill_manual(
    values = rev(c("#93D0BC", "#dcf0ea", "#ffd0c0", "#ffa084")),
    guide = guide_legend(
      title = "Fixed Internet\nDownload Speed",
      keywidth = 5,
      keyheight = .5,
      title.position = "left",
      label.position = "top",
      title.hjust = .5,
      order = 1
    ),
    labels = c("0 to 10 Mbps", "10 to 25", "25 to 100", "100+"),
    drop = FALSE) +
  labs(
    title = "Autauga County\n\n",
    caption = "Data: Ookla, US Census Bureau || Q2 2020 || By Katie Jolly"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "sans"),
    legend.position = "top",
    legend.direction = "horizontal",
    plot.title = element_text(size = 24)
  )
