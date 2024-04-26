make_sample_table <- function() {
  sample <- sf::read_sf("data/dados_bairros.geojson")
  
  sample_data <- sample |> 
    select(bairro, i1, i2) |> 
    st_drop_geometry() |> 
    arrange(bairro, .locale = "pt") |> 
    distinct(bairro, .keep_all = TRUE) |> 
    mutate(bairro = str_to_title(bairro))
  
  gt_sample <- sample_data |> 
    gt() |> 
    cols_label(
      bairro = "Bairro",
      i1 = "Densidade de semáforos [nº/km]",
      i2 = "Densidade de radares [nº/km]"
    ) |> 
    fmt_number(
      columns = c(i1, i2),
      decimals = 2,
      dec_mark = ",",
      sep_mark = "."
    )
  
  gtsave(gt_sample, "table/sample_data.docx")
}