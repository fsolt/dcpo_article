remotes::install_github("fsolt/DCPOtools")

library(tidyverse)
library(DCPOtools)
library(rstan)

fold <- commandArgs(trailingOnly=TRUE) %>%
    as.numeric()

download.file("https://raw.githubusercontent.com/jamesdunham/dgo/modgirt_dev/inst/models/2019_01_10_modgirt.stan", "dgirt.stan")

demsup <- read_csv("https://github.com/fsolt/DCPOtools/raw/master/inst/extdata/all_data_demsupport.csv")

modgirt_in <- setClass("modgirt_in",
                       slots = list(
                           items = "character",
                           time = "character",
                           geo = "character",
                           demo = "character",
                           stan_data = "list")
)

format_dgirt_kfold <- function(dcpo_data, 
                               fold_number = 1, 
                               number_of_folds = 10, 
                               fold_seed = 324) {
    set.seed(fold_seed)
    
    n_tkqr <- reshape2::acast(dcpo_data,
                              year ~ country ~ item ~ r,
                              fun.aggregate = sum,
                              value.var = "n",
                              drop = FALSE)
    
    folds_tkqr <- runif(min = 1, max = number_of_folds, 
                       n = length(which(!n_tkqr==0))) %>% 
        round()
    
    n_tkqr_train <- n_tkqr
    n_tkqr_train[which(!n_tkqr==0)][folds_tkqr==fold_number] <- 0
    
    n_tkqr_test <- n_tkqr
    n_tkqr_test[which(!n_tkqr==0)][!folds_tkqr==fold_number] <- 0
    test_pars <- paste0("PI[", apply(which(!n_tkqr_test==0, arr.ind = T), 1, paste, collapse = ","), "]")
    
    unused_cp <- as.data.frame(apply(n_tkqr_train, c(3, 4), sum)) %>%
        tibble::rownames_to_column() %>%
        dplyr::mutate_all(~if_else(. > 0, 0, 1)) %>%
        dplyr::select(-`1`, -rowname) %>%
        as.matrix()
    
    dgirt_stan <- new('modgirt_in',
                      items = attr(n_tkqr, "dimnames")[[3]],
                      time = "year",
                      geo = "country",
                      demo = "country",
                      stan_data = list( G          = dplyr::n_distinct(dcpo_data$country),
                                        T          = max(dcpo_data$year) - min(dcpo_data$year) + 1,
                                        Q          = dplyr::n_distinct(dcpo_data$item),
                                        K          = max(dcpo_data$r),
                                        D          = 1,
                                        SSSS       = n_tkqr_train,
                                        beta_sign  = matrix(1, dplyr::n_distinct(dcpo_data$item), 1),
                                        unused_cut = unused_cp,
                                        N_nonzero  = sum(n_tkqr != 0),
                                        data       = dcpo_data)
    )
    
    dgirt <- list(train = dgirt_stan, test = n_tkqr_test)
    
    return(dgirt)
}

demsup_data <- format_dgirt_kfold(demsup %>% with_min_yrs(3), fold_number = fold)

demsup_data$train@stan_data$evolving_alpha = as.integer(FALSE)

out1 <- stan(file = 'dgirt.stan',
             data = demsup_data$train@stan_data,
             iter = 10,
             thin = 2,
             chains= 4,
             cores = 4,
             pars = test_pars,
             control = list(adapt_delta = 0.99, stepsize = 0.001, max_treedepth = 14))

save(out1, 
     file = str_c("data/fold_", 
                  fold,
                  ".rda"))
