predict_pam = function(medoids, newdata, delta, delta_names){

  all_of <- NULL
  Z_new = dummy_cols(newdata, remove_selected_columns = TRUE) %>%
    select(all_of(delta_names)) %>% data.matrix()

  Z_med = dummy_cols(medoids, remove_selected_columns = TRUE) %>%
    select(all_of(delta_names)) %>% data.matrix()

  Z_m_d = Z_med %*% delta
  pred_mat = Z_m_d %*% t(Z_new)

  pred = pred_mat %>% as_tibble() %>% map_dbl(~which.min(.)) %>% as.character %>% factor


  return(pred)

}
