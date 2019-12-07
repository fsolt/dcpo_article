

ttt <- tibble::as_tibble(out_test) %>%
    pivot_longer(cols = everything(),
                 names_to = "var_name",
                 values_to = "value") %>% 
    group_by(var_name) %>%
    mutate(count = row_number()) %>%
    pivot_wider(id_cols = "var_name",
                names_from = "count",
                values_from = "value")
