
# Create a regional overview map of the Fremont Complex

####################.
#    OUTLINE
# 1) R Preamble
# 2) Shapefiles
# 3) Project Area
# 4) Basemap
# 5) Main Map
# 6) Inset Map
# 7) Save
####################.



# R Preamble --------------------------------------------------------------

library(cowplot)
library(here)
library(sf)
library(tidyverse)
library(tigris)

here("R", "fun-get_basemap.R") %>% source()



# Shapefiles --------------------------------------------------------------

fremont <- here("gis", "fremont.shp") %>% read_sf()

state_names <- c(
  "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", "Nevada", "Utah"
)

states <- tigris::states() %>% 
  rename_with(tolower) %>% 
  filter(name %in% state_names) %>% 
  select(name, stusps) %>%
  st_transform(4326)

utah <- states %>% filter(name == "Utah")



# Project Area ------------------------------------------------------------

bb8 <- states %>% 
  filter(name == "Utah") %>% 
  st_buffer(50000) %>% 
  st_bbox()

dy <- bb8[["ymax"]] - bb8[["ymin"]]
dx <- bb8[["xmax"]] - bb8[["xmin"]]

ratio <- c(9000 * dx/dy, 9000)

cover <- st_sym_difference(utah, st_as_sfc(bb8))



# Basemap -----------------------------------------------------------------

basemap <- get_basemap(
  bb8,
  map = "physical",
  size = ratio
)



# Main Map ----------------------------------------------------------------

bob <-
  ggplot() +
  annotation_raster(
    basemap, 
    bb8[["xmin"]], bb8[["xmax"]], 
    bb8[["ymin"]], bb8[["ymax"]]
  ) +
  geom_sf(
    data = states, 
    fill = "transparent", 
    color = "black",
    size = 0.2
  ) +
  geom_sf(
    data = cover, 
    fill = "white", 
    color = "transparent", 
    alpha = 0.7
  ) +
  geom_sf(
    data = utah, 
    fill = "transparent", 
    color = "black",
    size = 0.2
  ) +
  geom_sf(
    data = st_as_sfc(bb8), 
    fill = "transparent", 
    color = "black"
  ) +
  geom_sf(
    data = fremont,
    fill = alpha("gray45", 0.2),
    color = "darkred",
    linetype = "dashed",
    size = 0.35
  ) +
  coord_sf(
    xlim = c(bb8[["xmin"]], bb8[["xmax"]]),
    ylim = c(bb8[["ymin"]], bb8[["ymax"]]),
    expand = FALSE
  ) +
  theme_void()



# Inset Map ---------------------------------------------------------------

state_labels <- 
  states %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename_with(tolower) %>% 
  mutate(
    state = states$stusps,
    color = if_else(state == "UT", "white", "black"),
    y     = if_else(state == "ID", y - 0.75, y)
  )

inset <-
  ggplot() +
  geom_sf(
    data = states,
    fill = "gray98",
    color = "gray45",
    size = 0.1
  ) +
  geom_sf(
    data = utah,
    fill = "gray10",
    color = "black"
  ) +
  geom_text(
    data = state_labels,
    aes(x, y, label = state, color = color),
    size = 2 # have to make this smaller because of cowplot/ggsave scaling
  ) +
  scale_color_manual(
    values = c("black","white"),
    guide = "none"
  ) +
  coord_sf(
    expand = FALSE
  ) +
  theme_void()

full_map <-
  ggdraw() +
  draw_plot(bob) +
  draw_plot(
    inset,
    x = 0.83,
    y = 0.98,
    hjust = 1,
    vjust = 1,
    height = 0.27,
    width = 0.27
  )

# full_map



# Save --------------------------------------------------------------------

ggsave(
  plot = full_map,
  filename = here("fremont_map.png"),
  width = 4,
  height = 4 * dy/dx,
  dpi = 300
)
