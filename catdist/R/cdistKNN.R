cdistKNN <- function(train_df,assess_df = NULL, k = 2, method = "tot_var_dist"){
  .x = NULL
  response = NULL
  train_df = train_df %>%  purrr::map_df(~fct_drop(.))
  train_resp = train_df %>% pull(response) %>% as.character
  train_df = train_df %>% select(-response)
  truth = assess_df %>% pull(response) %>% as.character
  assess_df = assess_df %>% select(-response)

  prep_data = list()
  prep_data$df=train_df
  prep_data$Q<-as.numeric(lapply(train_df,nlevels))
  prep_data$y=train_resp

  delta = cat_delta(x = train_df, y = train_resp, method_cat = method)[[method]]

  Z_tr = dummy_cols(train_df, remove_selected_columns = TRUE) %>%
    as_tibble()

 # n_tr = nrow(Z_tr)

  Z_ts = dummy_cols(rbind(assess_df), remove_selected_columns = TRUE) %>%
    as_tibble() %>% select(names(Z_tr))

  D_tr_ts = (Z_tr %>% data.matrix()) %*% delta %*% t(Z_ts%>% data.matrix())
  D_tr_ts = as.data.table(D_tr_ts)
  preds = map_chr(.x = D_tr_ts, .f= function(x = .x){
    nbs = order(x)[1:k];
    nbs_y = train_resp[nbs]
    return(nbs_y %>% table %>% which.max %>% names)
  })
  return(tibble(truth=factor(truth), .pred = factor(preds)))
}


