library(tidyverse)
library(onsvplot)
library(Hmisc)
library(ggtext)
library(glue)
library(janitor)
library(readxl)
library(sf)
library(patchwork)
library(arrow)
library(osmdata)
library(gt)

source("R/01-sample.R")
source("R/02-correlacao.R")
source("R/03-wilcox.R")
source("R/04_table.R")

calc_sample <- function() {
  dados_bairros <- 
    readxl::read_xlsx("data/Dados Bairros media v3.xlsx") |> 
    janitor::clean_names()
  bairros_sf_path <- "data/DIVISA_DE_BAIRROS.shp"
  semaforos_path <- "data/traffic_lights.shp"
  radares_path <- "data/speed_traps.shp"
  divisa_bairros <- st_read(bairros_sf_path)
  semaforos <- st_read(semaforos_path)
  radares <- st_read(radares_path)
  divisa_bairros <- st_transform(divisa_bairros, 4674)
  qnt_semaforos <- count_semaforos(semaforos, divisa_bairros)
  qnt_radares <- count_radares(radares, divisa_bairros)
  cwb_axis <- import_cwb_axis()
  dados_bairros <- calc_vars(
    cwb_axis, divisa_bairros, dados_bairros, qnt_semaforos, qnt_radares
  )
  return(dados_bairros)
}

calc_corr <- function(dados_bairros) {
  cov_tibble <- calc_correlacao(dados_bairros)
  cov_tibble_fct <- transform_fct(cov_tibble)
  cor_matrix_plot <- plot_cor_matrix(cov_tibble_fct)
  ggsave(
    plot = cor_matrix_plot, 
    filename = "plot/cov_matrix.png",
    width = 5, 
    height = 5, 
    dpi = 300
  )
}

calc_wil <- function(dados_bairros) {
  tbl_medias <- group_medias(dados_bairros)
  tbl_wilcox <- calc_wilcox(tbl_medias)
  writexl::write_xlsx(tbl_wilcox, "data/tbl_wilcox.xlsx")
  box_wilcox_plot <- plot_box_wilcox(tbl_medias)
  ggsave(
    filename = "plot/plot_hist.png",
    plot = box_wilcox_plot,
    width = 14,
    height = 8,
    dpi = 300
  )
}

main <- function() {
  dados_bairros <- calc_sample()
  calc_correlacao(dados_bairros)
  calc_wil(dados_bairros)
  st_write(dados_bairros, "data/dados_bairros.geojson", append = FALSE)
  make_sample_table()
}

main()