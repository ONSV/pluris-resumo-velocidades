library(tidyverse)
library(onsvplot)

dados_bairros <- 
  readxl::read_xlsx("data/Dados Bairros media v3.xlsx") |> 
  janitor::clean_names()

cov_matrix <- dados_bairros |> 
  select(-bairro) |> 
  cor(method = "spearman")

cov_tibble <- cov_matrix |>
  as_tibble(rownames = "var_a") |> 
  pivot_longer(-var_a, names_to = "var_b", values_to = "cor") |> 
  mutate(
    var_a = factor(var_a, level = unique(cov_tibble$var_a)),
    var_b = factor(var_b, level = rev(unique(cov_tibble$var_b))),
    lvl_a = as.numeric(var_a),
    lvl_b = as.numeric(var_b |> fct_rev()),
    cor = if_else(lvl_a < lvl_b, cor, NA_real_)
  )

cov_plot <- cov_tibble |>
  ggplot(aes(x = var_a, y = var_b, fill = cor)) +
  geom_tile(
    color = ifelse(
      is.na(cov_tibble$cor),
      NA,
      "grey10"
    )
  ) +
  geom_text(
    aes(label = round(cor, 2)), 
    color = ifelse(
      abs(cov_tibble$cor) > 0.6,
      "grey90",
      "grey10"
    )
  ) +
  scale_fill_gradient2(
    low = onsv_palette$red, 
    high = onsv_palette$blue,
    mid = "white",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = NA
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.87, 0.8),
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  coord_fixed() +
  scale_x_discrete(limits = unique(cov_tibble$var_a)[1:7]) +
  scale_y_discrete(limits = rev(unique(cov_tibble$var_b))[1:7]) +
  labs(
    x = element_blank(),
    y = element_blank(),
    fill = "Correlação de\nSpearman"
  )

ggsave(
  plot = cov_plot, 
  filename ="plot/cov_matrix.png",
  width = 5, 
  height = 5, 
  dpi = 300
)
