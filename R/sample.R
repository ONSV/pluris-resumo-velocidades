ndsbr_path <- "data/ndsbr_sample.parquet"

ndsbr <- open_dataset(ndsbr_path)

ndsbr_time <- ndsbr |> 
  collect() |> 
  mutate(
    id = paste0(driver, trip),
    s = if_else(
      id == lag(id),
      time_length(time) - time_length(lag(time)),
      1
    )
  ) |> 
  filter(s > 0) |> 
  replace_na(list(s = 1))

tempo_bairros <- ndsbr_time |> 
  group_by(neighbhd) |> 
  summarise(s = sum(s)) |>
  rename(bairro = neighbhd) |> 
  filter(bairro != "NPI")

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

divisa_bairros_count <- divisa_bairros |> 
  left_join(qnt_semaforos |> rename(n_semaforos = n), by = "NOME") |>
  left_join(qnt_radares |> rename(n_radares = n), by = "NOME") |>
  left_join(tempo_bairros, by = c("NOME" = "bairro")) |>
  select(nome_bairro = NOME, n_semaforos, n_radares, s) |> 
  replace_na(list(n_semaforos = 0, n_radares = 0, s = 0))

bairros_indicadores <- divisa_bairros_count |> 
  mutate(
    i1 = if_else(
      n_semaforos == 0 | s == 0,
      0,
      n_semaforos / s * 60
    ),
    i2 = if_else(
      n_radares == 0 | s == 0,
      0,
      n_radares / s * 60
    )
  )

ggplot() +
  geom_sf(data = bairros_indicadores, aes(fill = i2))
