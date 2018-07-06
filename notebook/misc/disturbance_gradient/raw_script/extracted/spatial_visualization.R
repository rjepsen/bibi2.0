## ------------------------------------------------------------------------
suppressPackageStartupMessages(
library(rgdal)
)

## ------------------------------------------------------------------------
basin.poly <- readOGR(file.path("D:/ZSmith/Projects/Chessie_BIBI/bibi_baseline/bibi_baseline",
                                "data/gis/NHDPlus/shapefiles/basin.shp"),
                      verbose = FALSE)

basin.fort <- fortify(basin.poly)

## ------------------------------------------------------------------------
ref.class <- bio.fam.df %>% 
  cutpoints() %>% 
  filter(category %in% c("ref")) %>% 
  mutate(class = if_else(final_score >= cutpoint, "TP", "FN")) %>% 
  rename(latitude = latitude.x,
         longitude = longitude.x)
  

## ---- fig.width = 8, fig.height=12---------------------------------------
ref.plot <- ggplot() +
  geom_polygon(data = basin.fort,
               aes(long, lat, group = group),
               color = "black", fill = "#999999") +
  #annotation_map(fortify(clip2), fill = "#999999", colour = "black") +
  geom_point(data = ref.class,
             aes(longitude, latitude, color = class)) +
   scale_color_manual(values = c("TP" = "#56B4E9",
                               "FN" = "#E69F00"),
                     name = "Classification") +
  coord_equal() +
  theme_bw() +
  theme(#legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

fn.plot <- ggplot() +
  geom_polygon(data = basin.fort,
               aes(long, lat, group = group),
               color = "black", fill = "#999999") +
  #annotation_map(fortify(clip2), fill = "#999999", colour = "black") +
  geom_point(data = ref.class[ref.class$class == "FN", ],
             aes(longitude, latitude, color = class)) +
   scale_color_manual(values = c("TP" = "#56B4E9",
                               "FN" = "#E69F00"),
                     name = "Classification") +
  coord_equal() +
  theme_bw() +
  theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

cowplot::plot_grid(ref.plot, fn.plot, ncol = 1, labels = "auto")

