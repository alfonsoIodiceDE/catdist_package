catdistNN <-function(train_df,assess_df = NULL, y = NULL, k, method = method){
  # source("R/cat_delta.r")
  # source("R/CalculateDistances2.R")
  # library(fastDummies)
  colnames(train_df)[y] = "response"
  colnames(assess_df)[y] = "response"
  train_df = train_df %>%  map_df(~fct_drop(.))
  train_resp = train_df %>% pull(response) %>% as.character
  train_df = train_df %>% select(-response)
  truth = assess_df %>% pull(response) %>% as.character
  assess_df = assess_df %>% select(-response)
  
  prep_data = list()
  prep_data$df=train_df
  prep_data$Q<-as.numeric(lapply(train_df,nlevels))
  prep_data$y=train_resp

  # out_cdist = fct_delta(df=train_df, y=train_resp, method = method)
  # print(method)
  # delta = out_cdist$deltas[[method]] %>% as.matrix
 
  delta = cat_delta(x = train_df, y = train_resp, method = method)[[2]]
 
  Z_tr = dummy_cols(train_df, remove_selected_columns = TRUE) %>%
    as_tibble()
  # out_delta = cat_delta(x=train_df,y=NULL,method = method)
  # delta = out_delta$full_delta #%>% as.matrix()
  # Z_tr = out_delta$Z
  # print(names(Z_tr))
  n_tr = nrow(Z_tr)

  Z_ts = dummy_cols(rbind(assess_df), remove_selected_columns = TRUE) %>%
    as_tibble() %>% select(names(Z_tr))
#  print(dim(Z_tr))
 # print(dim(delta))
  
  D_tr_ts = (Z_tr %>% data.matrix()) %*% delta %*% t(Z_ts%>% data.matrix())
  D_tr_ts = as.data.table(D_tr_ts)
  preds = map_chr(.x = D_tr_ts, .f= function(x = .x){
    nbs = order(x)[1:k];
    nbs_y = train_resp[nbs]
    return(nbs_y %>% table %>% which.max %>% names)
  })
  return(tibble(truth=factor(truth), .pred = factor(preds)))
}
