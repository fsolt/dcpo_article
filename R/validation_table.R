# Validation table for dcpo_article.Rmd
library(huxtable)
library(tidyverse)

load(here::here("data", "church_21_2k_12-13-00-03.rda"))
ivt_dcpo <- DCPOtools::internal_validation_tests(demsup_data, out1)
load(here::here("data", "kfold", "dcpo_demsup_kfold.rda"))
xvt_dcpo <- DCPO::get_xvt_results(dcpo_demsup_kfold) %>% 
    filter(model == "k-fold mean") %>% 
    transmute(mean_mae = mae,
              mean_improv_over_cmmae = improv_over_cmmae,
              coverage80ci = coverage80ci)

validation_dcpo <- bind_cols(ivt_dcpo, xvt_dcpo) %>% 
    mutate(model = "DCPO")


# Claassen
load(here::here("data", "claassen_m5_2k_12-12-16-03.rda"))
ivt_claassen <- DCPOtools::internal_validation_tests(demsup_claassen, claassen_m5, "claassen")
load(here::here("data", "kfold", "claassen_demsup_kfold.rda"))
xvt_claassen <- get_claassen_xvt_results(claassen_demsup_kfold) %>% 
    filter(model == "k-fold mean") %>% 
    transmute(mean_mae = mae,
              mean_improv_over_cmmae = improv_over_cmmae,
              coverage80ci = coverage80ci)

validation_claassen <- bind_cols(ivt_claassen, xvt_claassen) %>% 
    mutate(model = "\\vtop{\\hbox{\\strut Claassen (2019)}\\hbox{\\strut Model 5}}")


# DGIRT
load(here::here("data","dgirt", "demsup_dgirt_50_12-15-03-44.rda"))
demsup <- read_csv("https://github.com/fsolt/DCPOtools/raw/master/inst/extdata/all_data_demsupport.csv")
demsup_data <- DCPOtools::format_dgirt(demsup %>% DCPOtools::with_min_yrs(3))
ivt_dgirt <- DCPOtools::internal_validation_tests(demsup_data, out1, "dgirt")

load(here::here("data", "kfold", "dgirt", "dgirt_demsup_kfold.rda"))

validation_dgirt <- bind_cols(ivt_dgirt, xvt_dgirt) %>% 
    mutate(model = "\\vtop{\\hbox{\\strut Caughey, O'Grady,}\\hbox{\\strut and Warshaw (2019)}}")

# Make the table
validation_table <- bind_rows(validation_claassen, validation_dgirt, validation_dcpo) %>% 
    mutate(mean_mae = sprintf("%.3f", mean_mae),
           mean_improv_over_cmmae = sprintf("%2.1f", mean_improv_over_cmmae),
           coverage80ci = if_else(coverage80ci > 80, paste0("+", sprintf("%2.1f", coverage80ci - 80)), sprintf("%2.1f", coverage80ci - 80))) %>% 
    transmute(`\\vtop{\\hbox{\\strut }\\hbox{\\strut }\\hbox{\\strut }\\hbox{\\strut Model}}` = model,
              `\\vtop{\\hbox{\\strut Mean}\\hbox{\\strut Absolute}\\hbox{\\strut Error}\\hbox{\\strut (MAE)}}` = mae,
              `\\vtop{\\hbox{\\strut }\\hbox{\\strut Country-}\\hbox{\\strut Means}\\hbox{\\strut MAE}}` = sprintf("%.3f", cmmae),
              `\\vtop{\\hbox{\\strut \\% Im-}\\hbox{\\strut prove-}\\hbox{\\strut ment in}\\hbox{\\strut MAE}}` = improv_over_cmmae,
              `\\vtop{\\hbox{\\strut }\\hbox{\\strut $k$-fold}\\hbox{\\strut Mean}\\hbox{\\strut MAE}}` = mean_mae,
              `\\vtop{\\hbox{\\strut $k$-fold}\\hbox{\\strut Mean \\%}\\hbox{\\strut Improve-}\\hbox{\\strut ment}}` = mean_improv_over_cmmae,
              `\\vtop{\\hbox{\\strut $k$-fold 80\\%}\\hbox{\\strut Credible}\\hbox{\\strut Interval}\\hbox{\\strut Coverage}}` = coverage80ci) %>% 
    as_hux() %>% 
    add_colnames() %>% 
    set_width(.8) %>% 
    rbind(c(NA_character_, "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), .) %>% 
    rbind(c(NA_character_, "Internal Validation Test", NA_character_, NA_real_, "External Validation Test", NA_real_, NA_real_), .) %>% 
    merge_cells(1, 2:4) %>% 
    merge_cells(1, 5:7) %>% 
    set_bold(c(1:3, 6), everywhere, TRUE) %>%
    set_top_border(1, everywhere, 1) %>%
    set_bottom_border(c(1, 3), everywhere, 1) %>%
    set_bottom_border(final(1), everywhere, 1) %>% 
    set_caption("Internal and External Validation Tests") %>%
    set_position("left") %>%
    set_escape_contents(3, everywhere, FALSE) %>%
    set_escape_contents(4:5, 1, FALSE) %>%
    set_background_color(evens, everywhere, "grey95") %>%
    set_background_color(2, everywhere, "white") %>%
    set_align("center") %>% 
    set_align(everywhere, 1, "left") %>% 
    add_rows(c("The internal validation test uses the same data for model fitting and validation; the external validation test employs k-fold validation with 10 folds, randomly dividing the data into tenths and then sequentially treating each tenth as a test set while fitting the model on a training set consisting of the remaining 90 percent of the data.  Percent improvement in MAE (column 3) compares the model's MAE (column 1) and the corresponding country-means MAE (column 2).", rep(NA_real_, 6))) %>%
    set_bold(final(1), everywhere, FALSE) %>%
    merge_cells(final(1), everywhere) %>%
    set_wrap(final(1), everywhere, TRUE) %>%
    set_background_color(final(1), everywhere, "white") %>%
    set_font_size(final(1), everywhere, 9) %>%
    set_label("tab:validation_table")

save(validation_table, file = here::here("data", "validation_table.rda"))
