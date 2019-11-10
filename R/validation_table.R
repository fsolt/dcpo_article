# Validation table for dcpo_article.Rmd
library(huxtable)
library(tidyverse)

load(here::here("data", "church_21_2k_10-27-17-20.rda"))
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
load(here::here("data", "claassen_m5_1k_07-15-10-25.rda"))
ivt_claassen <- DCPOtools::internal_validation_tests(demsup_claassen, claassen_m5, "claassen")
load(here::here("data", "kfold", "claassen_demsup_kfold.rda"))
xvt_claassen <- get_claassen_xvt_results(claassen_demsup_kfold) %>% 
    filter(model == "k-fold mean") %>% 
    transmute(mean_mae = mae,
              mean_improv_over_cmmae = improv_over_cmmae,
              coverage80ci = coverage80ci)

validation_claassen <- bind_cols(ivt_claassen, xvt_claassen) %>% 
    mutate(model = "\\vtop{\\hbox{\\strut Claassen (2019)}\\hbox{\\strut }}")


# DGIRT
load(here::here("data","dgirt", "demsup_dgirt_50_11-10-09-47.rda"))
demsup <- read_csv("https://github.com/fsolt/DCPOtools/raw/master/inst/extdata/all_data_demsupport.csv")
demsup_data <- DCPOtools::format_dgirt(demsup %>% DCPOtools::with_min_yrs(3))
ivt_dgirt <- DCPOtools::internal_validation_tests(demsup_data, out1, "dgirt")

validation_dgirt <- bind_cols(ivt_dgirt) %>% 
    mutate(model = "\\vtop{\\hbox{\\strut Caughey, O'Grady,}\\hbox{\\strut  and Warshaw (2019)}}")

# Make the table
validation_table <- bind_rows(validation_claassen, validation_dgirt, validation_dcpo) %>% 
    transmute(`\\vtop{\\hbox{\\strut }\\hbox{\\strut }\\hbox{\\strut }\\hbox{\\strut Model}}` = model,
              `\\vtop{\\hbox{\\strut Mean}\\hbox{\\strut Absolute}\\hbox{\\strut Error}\\hbox{\\strut (MAE)}}` = mae,
              `\\vtop{\\hbox{\\strut }\\hbox{\\strut Country-}\\hbox{\\strut Means}\\hbox{\\strut MAE}}` = sprintf("%.3f", cmmae),
              `\\vtop{\\hbox{\\strut \\% Im-}\\hbox{\\strut prove-}\\hbox{\\strut ment in}\\hbox{\\strut MAE}}` = improv_over_cmmae,
              `\\vtop{\\hbox{\\strut }\\hbox{\\strut $k$-fold}\\hbox{\\strut Mean}\\hbox{\\strut MAE}}` = mean_mae,
              `\\vtop{\\hbox{\\strut $k$-fold}\\hbox{\\strut Mean \\%}\\hbox{\\strut Improve-}\\hbox{\\strut ment}}` = mean_improv_over_cmmae,
              `\\vtop{\\hbox{\\strut $k$-fold 80\\%}\\hbox{\\strut Credible}\\hbox{\\strut Interval}\\hbox{\\strut Coverage}}` = coverage80ci) %>% 
    as_hux() %>% 
    add_colnames() %>% 
    rbind(c(NA_character_, "Internal Validation Tests", NA_character_, NA_real_, "External Validation Tests", NA_real_, NA_real_), .) %>% 
    merge_cells(1, 2:4) %>% 
    merge_cells(1, 5:7) %>% 
    set_bold(c(1:2,5), everywhere, TRUE) %>%
    set_top_border(1, everywhere, 1) %>%
    set_bottom_border(1:2, everywhere, 1) %>%
    set_bottom_border(final(1), everywhere, 1) %>% 
    set_caption('Internal and External Validation Tests') %>%
    set_position("left") %>%
    set_escape_contents(2, everywhere, FALSE) %>%
    set_escape_contents(3:4, 1, FALSE) %>%
    set_background_color(odds, everywhere, "grey95") %>%
    set_background_color(1, everywhere, "white") %>%
    set_align("center") %>% 
    set_align(everywhere, 1, "left") %>% 
    add_rows(c("The internal validation test uses the same data for model fitting and validation; the external validation test employs k-fold validation with 10 folds, randomly dividing the data into tenths and then sequentially treating each tenth as a test set while fitting the model on a training set consisting of the remaining 90 percent of the data.  Percent improvement in MAE compares the model's MAE (column 1) and the corresponding country-mean MAE (column 2).", rep(NA_real_, 6))) %>% 
    merge_cells(final(1), 1:7) %>% 
    set_wrap(final(1), everywhere, TRUE) %>% 
    set_background_color(final(1), everywhere, "white") %>% 
    set_font_size(final(1), everywhere, 8)

save(validation_table, file = here::here("data", "validation_table.rda"))
