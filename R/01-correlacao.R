calc_correlacao <- function(tbl_bairros) {
  cov_matrix <- tbl_bairros |> 
    select(-bairro) |> 
    cor(method = "spearman")

  rcorr_matrix <- tbl_bairros |> 
    select(-bairro) |> 
    as.matrix() |> 
    rcorr(type = "spearman")
  
  pvalue_tibble <- rcorr_matrix$P |> 
    as_tibble(rownames = "var_a") |> 
    pivot_longer(-var_a, names_to = "var_b", values_to = "p_value")
  
  cov_tibble <- cov_matrix |>
    as_tibble(rownames = "var_a") |> 
    pivot_longer(-var_a, names_to = "var_b", values_to = "cor") |> 
    left_join(pvalue_tibble, by = c("var_a", "var_b"))
  
  return(cov_tibble)
}

transform_fct <- function(cov_tbl) {
  cov_tibble_fct <- cov_tbl |> 
    mutate(
      var_a = factor(var_a, level = unique(cov_tbl$var_a)),
      var_b = factor(var_b, level = rev(unique(cov_tbl$var_b))),
      lvl_a = as.numeric(var_a),
      lvl_b = as.numeric(var_b |> fct_rev()),
      cor = if_else(lvl_a < lvl_b, cor, NA_real_)
    )
  
  return(cov_tibble_fct)
}

plot_cor_matrix <- function(tbl_cor) {
  plot_subtitle <- glue(
    "Valores em ",
    "<span style = 'color:#D51F29'>**vermelho**</span>",
    " apresentam p-valor >= 0.05"
  )
  
  cov_plot <- tbl_cor |>
    ggplot(aes(x = var_a, y = var_b, fill = cor)) +
    geom_tile(
      color = ifelse(
        is.na(tbl_cor$cor),
        NA,
        "grey10"
      )
    ) +
    geom_text(
      aes(label = round(cor, 2)),
      color = ifelse(
        tbl_cor$p_value >= 0.05,
        onsv_palette$red,
        ifelse(
          abs(tbl_cor$cor) > 0.6,
          "grey90",
          "grey10"
        )
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
      plot.background = element_rect(fill = "white", color = "white"),
      plot.subtitle = element_markdown()
    ) +
    coord_fixed() +
    scale_x_discrete(limits = unique(tbl_cor$var_a)[1:7]) +
    scale_y_discrete(limits = rev(unique(tbl_cor$var_b))[1:7]) +
    labs(
      x = element_blank(),
      y = element_blank(),
      fill = "Correlação de\nSpearman",
      subtitle = plot_subtitle
    )
  return(cov_plot)
}