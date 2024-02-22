bairros_sf_path <- "data/DIVISA_DE_BAIRROS.shp"
semaforos_path <- "data/traffic_lights.shp"
radares_path <- "data/speed_traps.shp"

divisa_bairros <- st_read(bairros_sf_path)
semaforos <- st_read(semaforos_path)
radares <- st_read(radares_path)

divisa_bairros <- st_transform(divisa_bairros, 4674)

qnt_semaforos <- 
  st_join(semaforos, divisa_bairros) |> 
  group_by(NOME) |> 
  summarise(n = n()) |> 
  st_drop_geometry()

qnt_radares <- st_join(radares, divisa_bairros) |> 
  group_by(NOME) |> 
  summarise(n = n()) |> 
  st_drop_geometry()

cwb_limits <- getbb("Curitiba")

cwb_highway <- opq(cwb_limits) |> 
  add_osm_feature(key = "highway") |> 
  osmdata_sf()

osm_axis_classes <- c(
  "trunk", "primary", "secondary", "tertiary", "unclassified", "residential",
  "motorway", "motorway_link", "trunk_link", "primary_link", "secondary_link",
  "tertiary_link"
)

cwb_axis <- cwb_highway$osm_lines |> 
  select(osm_id, name, highway) |> 
  filter(highway %in% osm_axis_classes) |> 
  st_transform(31982) |>
  mutate(dist = st_length(geometry))

cwb_axis <- st_transform(cwb_axis, 4674)

axis_dist_bairros <- st_join(cwb_axis, divisa_bairros) |> 
  group_by(NOME) |> 
  summarise(dist = sum(dist)) |> 
  st_drop_geometry() |> 
  drop_na()

divisa_bairros_var <- divisa_bairros |> 
  left_join(qnt_semaforos |> rename(n_semaforos = n), by = c("NOME")) |> 
  left_join(qnt_radares |> rename(n_radares = n), by = c("NOME")) |> 
  left_join(axis_dist_bairros, by = c("NOME")) |> 
  select(NOME, n_semaforos, n_radares, dist) |> 
  replace_na(list(n_semaforos = 0, n_radares = 0, dist = 0)) |> 
  mutate(
    ind_semaforos = units::drop_units(n_semaforos / dist * 1000),
    ind_radares = units::drop_units(n_radares / dist * 1000)
  )
