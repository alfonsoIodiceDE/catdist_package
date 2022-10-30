predict_pam = function(medoids, newdata, delta){
  
  # newdata = newdata %>%  map_df(~fct_drop(.))
  # newdata = newdata %>%  map_df(~fct_drop(.))
  
  
  
  
  Z_new = dummy_cols(newdata, remove_selected_columns = TRUE) %>%
    data.matrix()
  
  
  
  
  Z_med = dummy_cols(medoids, remove_selected_columns = TRUE) %>%
    data.matrix()
  
  # print(dim(Z_new))
  # print(dim(Z_med))
  # print(dim(delta))
  delta[is.na(delta)]=0
  Z_m_d = Z_med %*% delta[1:ncol(Z_med),1:ncol(Z_med)]
  Z_m_d[is.na(Z_m_d)]=0
  pred_mat = Z_m_d %*% t(Z_new) 
  
  pred = pred_mat %>% as_tibble() %>% map_dbl(~which.min(.)) %>% as.character %>% factor
  
  
  return(pred)
  
}
