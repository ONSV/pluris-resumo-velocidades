library(tidyverse)
library(onsvplot)
library(Hmisc)
library(ggtext)
library(glue)

source("R/01-correlacao.R")

dados_bairros <- 
  readxl::read_xlsx("data/Dados Bairros media v3.xlsx") |> 
  janitor::clean_names()
