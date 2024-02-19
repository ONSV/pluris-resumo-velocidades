
tbl_medias <- dados_bairros |> 
  select(-bairro) |> 
  mutate(
    media_v1 = ifelse(v1 > mean(v1), "acima", "abaixo"),
    media_v2 = ifelse(v2 > mean(v2), "acima", "abaixo"),
    media_v3 = ifelse(v3 > mean(v3), "acima", "abaixo"),
    across(
      starts_with("media"),
      ~ ifelse(
        .x == "acima",
        paste0(
          .x,
          " (n = ",
          .data$.x[.data$.x == "acima"] |> length(),
          ")"
        ),
        paste0(
          .x,
          " (n = ",
          .data$.x[.data$.x == "abaixo"] |> length(),
          ")"
        )
      )
    )
  )

calc_wilcox <- function(tbl) {
  vars <- c("p1", "p2", "r1", "r2", "r3")
  divs <- c("media_v1", "media_v2", "media_v3")
  
  wilcox_input <- expand.grid(vars, divs) |> 
    as_tibble() |> 
    mutate(formula = paste0(Var1, " ~ ", Var2))
  
  map(
    wilcox_input$formula,
    ~ wilcox.test(as.formula(.x), data = tbl, alternative = "two.sided")
  ) |> 
    map(broom::tidy) |> 
    bind_rows() |> 
    bind_cols(formula = wilcox_input$formula)
}

tbl_wilcox <- calc_wilcox(tbl_medias)

writexl::write_xlsx(tbl_wilcox, "data/tbl_wilcox.xlsx")

plot_box_wilcox <- function(x, y, data = tbl_medias) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_boxplot(aes(fill = {{x}}), color = "grey20") +
    scale_y_continuous(limit = c(0, 1)) +
    scale_fill_manual(values = c(onsv_palette$blue, onsv_palette$red)) +
    theme_minimal() +
    theme(
      legend.position = "none"
    )
}

plot_media_v1 <- tbl_medias |> 
  select(r1, r2, r3, p1, p2, media_v1) |> 
  pivot_longer(
    cols = r1: p2,
    names_to = "var",
    values_to = "value"
  ) |> 
  ggplot() +
  geom_boxplot(
    aes(x = media_v1, y = value, fill = media_v1), color = "grey20"
  ) +
  scale_fill_manual(values = c(onsv_palette$blue, onsv_palette$red)) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = "white"),
  ) +
  labs(x = element_blank(), y = element_blank()) +
  facet_grid(~var) +
  ggtitle("Média v1")

plot_media_v2 <- tbl_medias |> 
  select(r1, r2, r3, p1, p2, media_v2) |> 
  pivot_longer(
    cols = r1: p2,
    names_to = "var",
    values_to = "value"
  ) |> 
  ggplot() +
  geom_boxplot(
    aes(x = media_v2, y = value, fill = media_v2), color = "grey20"
  ) +
  scale_fill_manual(values = c(onsv_palette$blue, onsv_palette$red)) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = "white"),
  ) +
  labs(x = element_blank(), y = element_blank()) +
  facet_grid(~var) +
  ggtitle("Média v2")

plot_media_v3 <- tbl_medias |> 
  select(r1, r2, r3, p1, p2, media_v3) |> 
  pivot_longer(
    cols = r1: p2,
    names_to = "var",
    values_to = "value"
  ) |> 
  ggplot() +
  geom_boxplot(
    aes(x = media_v3, y = value, fill = media_v3), color = "grey20"
  ) +
  scale_fill_manual(values = c(onsv_palette$blue, onsv_palette$red)) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = "white"),
  ) +
  labs(x = element_blank(), y = element_blank()) +
  facet_grid(~var) +
  ggtitle("Média v3")

plot_hist <- plot_media_v1 / plot_media_v2 / plot_media_v3

ggsave(
  filename = "plot/plot_hist.png",
  plot = plot_hist,
  width = 12,
  height = 8,
  dpi = 300
)