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

source("R/01-correlacao.R")
source("R/02-wilcox.R")

calc_correlacao <- function() {
  dados_bairros <- 
    readxl::read_xlsx("data/Dados Bairros media v3.xlsx") |> 
    janitor::clean_names()
  cov_tibble <- calc_correlacao(dados_bairros)
  cov_tibble_fct <- transform_fct(cov_tibble)
  cor_matrix_plot <- plot_cor_matrix(cov_tibble_fct)
  ggsave(
    plot = cor_matrix_plot, 
    filename ="plot/cov_matrix.png",
    width = 5, 
    height = 5, 
    dpi = 300
  )
}

calc_wilcox <- function() {
  tbl_medias <- group_medias(dados_bairros)
  tbl_wilcox <- calc_wilcox(tbl_medias)
  writexl::write_xlsx(tbl_wilcox, "data/tbl_wilcox.xlsx")
  box_wilcox_plot <- plot_box_wilcox(tbl_medias)
  ggsave(
    filename = "plot/plot_hist.png",
    plot = box_wilcox_plot,
    width = 12,
    height = 8,
    dpi = 300
  )
}

main <- function() {
  calc_correlacao()
  calc_wilcox()
}

main()