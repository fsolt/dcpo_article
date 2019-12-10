library(tidyverse)


demsup <- read_csv("https://github.com/fsolt/DCPOtools/raw/master/inst/extdata/all_data_demsupport.csv")
dcpo_data <- demsup %>% 
    DCPOtools::with_min_yrs(3) %>% 
    group_by(country, year, item) %>% 
    mutate(samp_prop = n/sum(n)) %>% 
    ungroup()

set.seed(324)

n_tkqr <- reshape2::acast(dcpo_data,
                          year ~ country ~ item ~ r,
                          fun.aggregate = sum,
                          value.var = "n",
                          drop = FALSE)

dgirt_demsup_kfold <- purrr::map_df(7:8, function(x) {
    load(here::here("data", "kfold", "dgirt", paste0("fold_", x, ".rda")))
    df <- tibble::as_tibble(out_test) %>%
        pivot_longer(cols = everything(),
                     names_to = "par",
                     values_to = "value") %>% 
        group_by(par) %>%
        mutate(count = row_number()) %>%
        pivot_wider(id_cols = "par",
                    names_from = "count",
                    values_from = "value") %>% 
        mutate(fold = x,
               country_code = str_replace(par, "PI\\[\\d+,(\\d+),\\d+,\\d+\\]", "\\1") %>%
                   as.numeric(),
               country = attr(n_tkqr, "dimnames")[2][[1]][country_code],
               year_code = str_replace(par, "PI\\[(\\d+),\\d+,\\d+,\\d+\\]", "\\1") %>%
                   as.numeric(),
               year = attr(n_tkqr, "dimnames")[1][[1]][year_code] %>% 
                   as.numeric(),
               item_code = str_replace(par, "PI\\[\\d+,\\d+,(\\d+),\\d+\\]", "\\1") %>%
                   as.numeric(),
               item = attr(n_tkqr, "dimnames")[3][[1]][item_code],
               r = str_replace(par, "PI\\[\\d+,\\d+,\\d+,(\\d+)\\]", "\\1") %>%
                   as.numeric()) %>% 
        left_join(dcpo_data %>% select(country, year, item, r, samp_prop), by = c("country", "year", "item", "r")) %>% 
        arrange(country_code, year, item_code, r)
    return(df)
})

test_data <- dcpo_xvt_output %>%
    dplyr::first() %>%
    dplyr::nth(-2) %>%
    dplyr::filter(test == 1)

y_r_test_all <- dcpo_xvt_output %>%
    dplyr::nth(2) %>%
    rstan::extract(pars = "y_r_test") %>%
    dplyr::first()

model_mae <- mean(abs(test_data$y_r/test_data$n_r - (colMeans(y_r_test_all)/test_data$n_r))) %>%
    round(3)

country_mean <- test_data %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(country_mean = mean(y_r/n_r)) %>%
    dplyr::ungroup()
country_mean_mae <- mean(abs((country_mean$y_r/country_mean$n_r - country_mean$country_mean))) %>%
    round(3)

improv_vs_cmmae <- round((country_mean_mae - model_mae)/country_mean_mae * 100, 1)

coverage <- (mean(test_data$y_r >= apply(y_r_test_all, 2, quantile, (1-ci/100)/2) &
                      test_data$y_r <= apply(y_r_test_all, 2, quantile, 1-(1-ci/100)/2)) * 100) %>%
    round(1)

xvt_results <- tibble::tibble(model = c(paste0("Fold ", dcpo_xvt_output$xvt_args$fold_number, " of ", dcpo_xvt_output$xvt_args$number_of_folds, " (", dcpo_xvt_output$xvt_args$fold_seed,")"), "country means"),
                              mae = c(model_mae, country_mean_mae),
                              improv_over_cmmae = c(improv_vs_cmmae, NA))
ci_name <- paste0("coverage", ci, "ci")
xvt_results[[ci_name]] <- c(coverage, NA)

return(xvt_results)