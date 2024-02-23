count_semaforos <- function(semaforos, bairros) {
  qnt_semaforos <- 
    st_join(semaforos, bairros) |> 
    group_by(NOME) |> 
    summarise(n = n()) |> 
    st_drop_geometry()
  return(qnt_semaforos)
}

count_radares <- function(radares, bairros) {
  qnt_radares <- 
    st_join(radares, bairros) |> 
    group_by(NOME) |> 
    summarise(n = n()) |> 
    st_drop_geometry()
  return(qnt_radares)
}

import_cwb_axis <- function() {
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
  return(cwb_axis)
}

calc_vars <- function(axis, div_bairros, vars, semaforos, radares) {
  axis_dist_bairros <- st_join(axis, div_bairros) |> 
    group_by(NOME) |> 
    summarise(dist = sum(dist)) |> 
    st_drop_geometry() |> 
    drop_na()
  divisa_bairros_var <- div_bairros |> 
    left_join(semaforos |> rename(n_semaforos = n), by = c("NOME")) |> 
    left_join(radares |> rename(n_radares = n), by = c("NOME")) |> 
    left_join(axis_dist_bairros, by = c("NOME")) |> 
    select(NOME, n_semaforos, n_radares, dist) |> 
    replace_na(list(n_semaforos = 0, n_radares = 0, dist = 0)) |> 
    mutate(
      ind_semaforos = units::drop_units(n_semaforos / dist * 1000),
      ind_radares = units::drop_units(n_radares / dist * 1000)
    )
  dados_bairros <- divisa_bairros_var |> 
    left_join(vars, by = c("NOME" = "bairro")) |> 
    select(bairro = NOME, i1 = ind_semaforos, i2 = ind_radares, v1:r3) |> 
    drop_na()
  return(dados_bairros)
}

