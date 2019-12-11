library(tidyverse)

demsup <- read_csv("https://github.com/fsolt/DCPOtools/raw/master/inst/extdata/all_data_demsupport.csv")
dcpo_data <- demsup %>% 
    DCPOtools::with_min_yrs(3) %>% 
    group_by(country, year, item) %>% 
    mutate(samp_prop = n/sum(n)) %>% 
    ungroup()

dgirt_demsup_kfold <- purrr::map_df(1:10, function(x) {
    set.seed(324)
    dat <- dcpo_data %>%
        group_by(country, year, item) %>%
        mutate(fold = runif(min = 1, max = 10, n = 1) %>%
                   round()) %>%
        ungroup() %>%
        mutate(test = as.numeric(fold == x),
               n = if_else(test==1, 0, n))
    
    n_tkqr <- reshape2::acast(dcpo_data,
                              year ~ country ~ item ~ r,
                              fun.aggregate = sum,
                              value.var = "n",
                              drop = FALSE)
    
    n_tkqr_train <- reshape2::acast(dat,
                                    year ~ country ~ item ~ r,
                                    fun.aggregate = sum,
                                    value.var = "n",
                                    drop = FALSE)
    
    load(here::here("data", "kfold", "dgirt", paste0("fold_", x, ".rda")))
    df <- tibble::as_tibble(out_test) %>%
        pivot_longer(cols = everything(),
                     names_to = "par",
                     values_to = "value") %>% 
        group_by(par) %>%
        summarize(mean_pred = mean(value), 
                  lb = quantile(value, probs = .1), 
                  ub = quantile(value, probs = .9)) %>%
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
        arrange(country_code, year, item_code, r) %>% 
        mutate(abs_error = abs(mean_pred - samp_prop),
               in_80ci = mean_pred >= lb & mean_pred <= ub)
    
    country_mean <- dat %>%
        dplyr::filter(test == 0) %>%
        dplyr::group_by(country, year, item) %>%
        dplyr::arrange(desc(r), .by_group = TRUE) %>%
        dplyr::mutate(y_r = round(cumsum(n)),
                      n_r = round(sum(n))) %>%
        dplyr::arrange(r, .by_group = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::filter(r > 1) %>%
        dplyr::group_by(country) %>%
        dplyr::summarize(country_mean = mean(y_r/n_r))

    cmmae_test <- dcpo_data %>%
        dplyr::group_by(country, year, item) %>%
        dplyr::arrange(desc(r), .by_group = TRUE) %>%
        dplyr::mutate(y_r = round(cumsum(n)),
                      n_r = round(sum(n))) %>%
        dplyr::arrange(r, .by_group = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::filter(r > 1) %>%
        left_join(country_mean, by = "country")
    
    country_mean_mae <- mean(abs((cmmae_test$y_r/cmmae_test$n_r - cmmae_test$country_mean))) %>%
        round(3)
    
    model_mae <- mean(df$abs_error) %>% 
        round(3)
    
    improv_vs_cmmae <- round((country_mean_mae - model_mae)/country_mean_mae * 100, 1)
    
    coverage <- (mean(df$in_80ci) * 100) %>%
        round(1)
    
    xvt_results <- tibble::tibble(model = paste0("Fold ", x),
                                  mae = model_mae,
                                  improv_over_cmmae = improv_vs_cmmae,
                                  coverage80ci = coverage)
})

xvt_dgirt <- dgirt_demsup_kfold %>% 
    summarize_if(is.numeric, mean) %>% 
    rename(mean_mae = mae, 
           mean_improv_over_cmmae = improv_over_cmmae)

save(xvt_dgirt, file = here::here("data", "kfold", "dgirt", "dgirt_demsup_kfold.rda"))

