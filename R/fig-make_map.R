
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

# preamble ----------------------------------------------------------------

library(ggfx)
library(here)
library(sf)
library(tidyverse)
library(tigris)

sys.source(
  here("R", "fun-get_basemap.R"),
  envir = attach(NULL, name = "basemap")
)

# shapefiles --------------------------------------------------------------

fremont <- here("gis", "fremont.shp") |> read_sf()

state_names <- c(
  "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", "Nevada", "Utah"
)

states <- tigris::states() |> 
  rename_with(tolower) |> 
  filter(name %in% state_names) |> 
  select(name, stusps) |>
  st_transform(26912)

utah <- states |> filter(name == "Utah")

# project area ------------------------------------------------------------

bb8 <- states |> 
  filter(name == "Utah") |> 
  st_buffer(50000) |> 
  st_bbox()

dy <- bb8[["ymax"]] - bb8[["ymin"]]
dx <- bb8[["xmax"]] - bb8[["xmin"]]

ratio <- c(9000 * dx/dy, 9000)

cover <- st_sym_difference(utah, st_as_sfc(bb8))

# basemap -----------------------------------------------------------------

basemap <- get_basemap(
  bb8,
  map = "physical",
  size = ratio
)

# main map ----------------------------------------------------------------

bob <- ggplot() +
  annotation_raster(
    basemap,
    bb8[["xmin"]], bb8[["xmax"]],
    bb8[["ymin"]], bb8[["ymax"]]
  ) +
  geom_sf(
    data = states, 
    fill = "transparent", 
    color = "black",
    linewidth = 0.2
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
    linewidth = 0.3
  ) +
  geom_sf(
    data = st_as_sfc(bb8), 
    fill = "transparent", 
    color = "black",
    linewidth = 0.3
  ) +
  with_blur(
    geom_sf(
      data = fremont,
      fill = "transparent",
      color = alpha("white", 0.7),
      linewidth = 2
    ),
    sigma = 3
  ) +
  with_blur(
    geom_sf(
      data = fremont,
      fill = "transparent",
      color = alpha("#10A8A0", 0.7),
      linewidth = 1.5
    ),
    sigma = 3
  ) +
  geom_sf(
    data = fremont,
    fill = "transparent",
    color = "#19535F",
    linewidth = 0.5
  )
  
# inset map ---------------------------------------------------------------

wst_cntr <- states |> st_union() |> st_centroid()

flerp <- states

st_geometry(flerp) <- (st_geometry(states)-wst_cntr) * 0.09 + wst_cntr + c(37000, 290000)

st_crs(flerp) <- 26912

state_labels <- flerp |> 
  st_centroid() |> 
  st_coordinates() |> 
  as_tibble() |> 
  rename_with(tolower) |> 
  mutate(
    state = states$stusps,
    color = if_else(state == "UT", "white", "black"),
    y     = if_else(state == "ID", y - 6500, y)
  )

full_map <- bob +
  geom_sf(
    data = flerp,
    fill = "gray98",
    color = "gray45",
    linewidth = 0.1
  ) +
  geom_sf(
    data = flerp |> filter(name == "Utah"),
    fill = "gray10",
    color = "black"
  ) +
  geom_text(
    data = state_labels,
    aes(x, y, label = state), 
    color = state_labels$color,
    size = 12/.pt
  ) +
  coord_sf(
    crs = 26912,
    xlim = c(bb8[["xmin"]], bb8[["xmax"]]),
    ylim = c(bb8[["ymin"]], bb8[["ymax"]]),
    expand = FALSE
  ) +
  theme_void()

full_map

# Save --------------------------------------------------------------------

ggsave(
  plot = full_map,
  filename = here("fremont_map.jpg"),
  width = 5.75,
  height = 5.75 * dy/dx,
  dpi = 900
)
