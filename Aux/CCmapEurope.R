world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ccplot.data_temp <- df.cov %>%
  dplyr::select(., c("year", "CC", "Country", "iso2c")) %>% distinct(.) %>% drop_na(CC)
ccplot.data <- world %>%
  left_join(
    x = .,
    y = ccplot.data_temp %>% rename("iso_a2_eh" = "iso2c"),
    by = "iso_a2_eh"  
  )

ggplot(data = ccplot.data %>% drop_na(CC)) +
  geom_sf(aes(fill=CC)) +
  facet_wrap(~year) + 
#  scale_fill_distiller(palette = "Greys", direction = 1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1 ) +
  coord_sf(xlim = c(-12, 32), ylim = c(35, 71), expand = FALSE) +
  theme_void(base_size = 20) + theme(legend.position = c(0.77, 0.2)) +
  guides(fill = guide_colorbar(direction = "horizontal", barwidth = unit(8, "cm"),
    barheight = unit(0.5, "cm"))
)
ggsave("Texte/map_bw.pdf", width = 16, height = 9)
knitr::plot_crop("Texte/map_bw.pdf")

