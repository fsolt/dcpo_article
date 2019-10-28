library(tidyverse)
library(DCPOtools)
library(DCPO)

demsup <- read_csv(system.file("extdata", "all_data_demsupport.csv", package = "DCPOtools"))

dcpo_demsup_kfold <- purrr::map(1:10, function(x) {
                           dcpo_xvt(demsup_data,
                                      fold_number = x, # number_of_folds = 10 is the default
                                      iter = 500)
                                      })

save(dcpo_demsup_kfold, file = file.path("data", "kfold", "dcpo_demsup_kfold.rda"))
