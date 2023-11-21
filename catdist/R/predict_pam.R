predict_pam = function(medoids, newdata, delta, delta_names){

<<<<<<< HEAD
=======
  # newdata = newdata %>%  map_df(~fct_drop(.))
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
  all_of <- NULL
  Z_new = dummy_cols(newdata, remove_selected_columns = TRUE) %>%
    select(all_of(delta_names)) %>% data.matrix()

  Z_med = dummy_cols(medoids, remove_selected_columns = TRUE) %>%
    select(all_of(delta_names)) %>% data.matrix()
<<<<<<< HEAD

  Z_m_d = Z_med %*% delta
=======

  # print(dim(Z_new))
  # print(dim(Z_med))
  # if(dim(delta)[2]!=dim(Z_med)[2]){
  #   print(dim(delta))
  #   }


  Z_m_d = Z_med %*% delta
  # Z_m_d = Z_med %*% delta[1:ncol(Z_med),1:ncol(Z_med)]
  # print(is.na(Z_m_d))
  # Z_m_d[is.na(Z_m_d)]=0
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
  pred_mat = Z_m_d %*% t(Z_new)

  pred = pred_mat %>% as_tibble() %>% map_dbl(~which.min(.)) %>% as.character %>% factor


  return(pred)

}
