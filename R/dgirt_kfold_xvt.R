

dgirt_demsup_kfold <- purrr::map_df(7:8, function(x) {
    load(here::here("data", "kfold", "dgirt", paste0("fold_", x, ".rda")))
    df <- tibble::as_tibble(out_test) %>%
        pivot_longer(cols = everything(),
                     names_to = "var_name",
                     values_to = "value") %>% 
        group_by(var_name) %>%
        mutate(count = row_number()) %>%
        pivot_wider(id_cols = "var_name",
                    names_from = "count",
                    values_from = "value") %>% 
        mutate(fold = x)
    return(df)
})