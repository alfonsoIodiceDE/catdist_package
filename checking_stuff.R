library(tidyverse)
library(tidymodels)
library(fastDummies)
library(aricode)
library(corrplot)
library(ggh4x)
library(magick)
library(pdftools)
library(clustrd)
library(cluster)
library(Matrix)
library(purrr)
library(philentropy)
library(data.table)
library(tidytext)
source("R/cat_custom_delta.R")
source("R/z_preproc.R")
source("R/select_data.R")
source("R/catdist.R")
source("R/cat_delta.R")
source("R/predict_pam.R")
source("R/prepare_data.R")
source("R/cat_nearest_neighbors.R")
set.seed(123)


selected_distances = c("tot_var_dist", "gifi_chi2",
                       "supervised","supervised_full", "matching","eskin",
                       "goodall_3","goodall_4","iof","of","lin",
                       "var_entropy","var_mutability","kullback-leibler")

dataset_names = c("vote","australian","wbcd", "tictac","balance","tae",
                  "lympho","soybeanlarge","cars")

benchmark_data = tibble(datasets=dataset_names) %>%
  mutate(prepped_data = map(.x=datasets,~select_data(.x)),
         prepped_data = map(.x=prepped_data, ~cbind(.x$df,response=.x$y)),
         distance_method = rerun(selected_distances,.n=n())
  )

benchmark_data_resamples_pam_preproc = benchmark_data %>% 
  mutate(
    cross_validation = map(.x=prepped_data,~vfold_cv(.x, v= 5, strata=response)),
    fold_id = map(.x=cross_validation,~.x$id),
    fold_splits = map(.x=cross_validation,~.x$splits),
    n_clusters  = map_dbl(.x=prepped_data,~length(levels(.x$response)))
  )%>% unnest(cols = distance_method) %>% unnest(cols = c(fold_id,fold_splits)) %>% #%>% unnest(cols = k_par)
  select(-cross_validation,-prepped_data) %>%
  mutate(train_fold = map(.x=fold_splits,~analysis(.x)),
         test_fold=map(.x=fold_splits,.f=~assessment(.x))
         # ,
         # Z_train=map(.x=train_fold,~dummy_cols(.x, remove_selected_columns = TRUE) %>% data.matrix()),
         # Z_test=map(.x=test_fold,~dummy_cols(.x, remove_selected_columns = TRUE) %>% data.matrix())
  ) %>% 
  select(-fold_splits)


benchmark_data_resamples_pam = benchmark_data_resamples_pam_preproc %>%
  mutate(
    distance_res = pmap(.l=list(..1 = train_fold,..2 = distance_method,..3 = 1),
                        .f=~catdist(x = ..1 %>% select(-response),
                                    y = ..1 %>% pull(response),
                                    method = ..2,out_delta=..3)),
    # distance_mat = map(.x=distance_res,.f=function(x=.x){x$distance_mat[is.na(x$distance_mat)]=0;
    distance_mat = map(.x=distance_res,.f=function(x=.x){x$distance_mat;
      return(x$distance_mat)}),
    NA_distance_mat=map_lgl(.x=distance_mat,~anyNA(.x)),
    delta = map(.x=distance_res,~.x$delta),
    NA_delta = map_lgl(.x=delta,~anyNA(.x))
  )


benchmark_data_resamples_pam %>% filter(NA_distance_mat) %>% slice(1) 
benchmark_data_resamples_pam  %>% slice(2) %>% pull(delta)
benchmark_data_resamples_pam %>%  filter(NA_distance_mat) %>% slice(2) %>% pull(distance_mat)

which(benchmark_data_resamples_pam %>%  filter(NA_distance_mat) %>% slice(2) %>% pull(Z_train) %>% .[[1]] %>% colSums() ==0)



