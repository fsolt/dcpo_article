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
    
    dat <- dcpo_data %>%
        group_by(country, year, item) %>%
        mutate(fold = runif(min = 1, max = number_of_folds, n = 1) %>%
                   round()) %>%
        ungroup() %>%
        mutate(test = as.numeric(fold == fold_number),
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

    n_tkqr_test <- setdiff(which(n_tkqr_train==0), which(n_tkqr==0))
    test_pars <- paste0("PI[", apply(arrayInd(n_tkqr_test, dim(n_tkqr), dimnames(n_tkqr)), 1, paste, collapse = ","), "]")
    
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
                      stan_data = list( G          = dim(n_tkqr_train)[2],
                                        T          = dim(n_tkqr_train)[1],
                                        Q          = dim(n_tkqr_train)[3],
                                        K          = dim(n_tkqr_train)[4],
                                        D          = 1,
                                        SSSS       = n_tkqr_train,
                                        beta_sign  = matrix(1, dplyr::n_distinct(dcpo_data$item), 1),
                                        unused_cut = unused_cp,
                                        N_nonzero  = sum(n_tkqr_train != 0),
                                        data       = dcpo_data)
    )
    
    dgirt <- list(train = dgirt_stan, test = test_pars)
    
    return(dgirt)
}

demsup_data <- format_dgirt_kfold(demsup %>% with_min_yrs(3), fold_number = fold)

demsup_data$train@stan_data$evolving_alpha = as.integer(FALSE)

out1 <- stan(file = 'dgirt.stan',
             data = demsup_data$train@stan_data,
             iter = 300,
             thin = 2,
             chains= 4,
             cores = 4,
             pars = "PI",
             control = list(adapt_delta = 0.99, stepsize = 0.001, max_treedepth = 14))

out_test <- rstan::extract(out1, pars = demsup_data$test)

save(out_test, 
     file = str_c("data/fold_", 
                  fold,
                  ".rda"))
