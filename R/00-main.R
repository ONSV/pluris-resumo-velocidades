library(tidyverse)
library(onsvplot)
library(Hmisc)
library(ggtext)
library(glue)
library(janitor)

dados_bairros <- 
  readxl::read_xlsx("data/Dados Bairros media v3.xlsx") |> 
  janitor::clean_names()

source("R/01-correlacao.R")
source("R/02-wilcox.R")
