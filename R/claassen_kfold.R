library(rstan)
library(tidyverse)
library(beepr)

source("R/claassen_xvt.R")

demsup <- read_csv(system.file("extdata", "all_data_demsupport.csv", package = "DCPOtools"))

demsup_data <- DCPOtools::format_claassen(demsup %>% 
                                              DCPOtools::with_min_yrs(3))


claassen_demsup_kfold <- purrr::map(1:10, function(x) {
    claassen_xvt(demsup_data,
             fold_number = x, # number_of_folds = 10 is the default
             iter = 500)
})

xvt_claassen_kfold <- get_claassen_xvt_results(claassen_demsup_kfold)

save(list = c("claassen_demsup_kfold", "xvt_claassen_kfold"), file = file.path("data", "kfold", "claassen_demsup_kfold.rda"))
