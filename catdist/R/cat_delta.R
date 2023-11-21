<<<<<<< HEAD
cat_delta <- function(x, y = NULL, method = NULL, method_cat="tot_var_dist", mkw_p = 1){
=======
cat_delta <- function(x, y = NULL, method = NULL, mkw_p = 1){
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
  id <- NULL
  a <- NULL
  b <- NULL
  blocks <- NULL
  .x <- NULL
<<<<<<< HEAD

  # if(method=="association_based"){
  #   ab_method=method
  method=method_cat
  # }else{
  #   ab_method=method
  # }
=======
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
  catdiss = method
  # print(map(x,class))
  # print(map_dbl(x,nlevels))
<<<<<<< HEAD
  x = purrr::map_df(x,fct_drop)
  # print(map_dbl(x,nlevels))


  if(is.null(dim(x))){
=======


    if(is.null(dim(x))){
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
    Q=nlevels(x)
  }else{
    Q=map_dbl(x,nlevels)
<<<<<<< HEAD
  }
=======
    }
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b


  n=nrow(x)
  nvar=length(Q)

  z_prep = z_preproc(x=x,y=y,Q=Q)
  #print("preprocessed")
  Z_names= colnames(z_prep$Z)

  Z = z_prep$Z %>% data.matrix()

<<<<<<< HEAD
  ZZod = z_prep$ZZod
  zm = z_prep$zm
=======

    ZZod = z_prep$ZZod
    zm = z_prep$zm

>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b

  Z_list = z_prep$Z_list
  # level_pos = z_prep$level_pos
  level_pos = data.table(start=c(1,cumsum(Q)[-length(Q)]+1),stop=cumsum(Q))
  if(!is.null(y)){
    Z_y = z_prep$Z_y
  }else{Z_y=NULL}



  Qs=ncol(Z)


  if(method %in% c("tot_var_dist", "gifi_chi2",
                   "supervised","supervised_full","matching","eskin",
                   "goodall_3","goodall_4","iof","of","lin","var_entropy","var_mutability")){
    # print("in custom")
    full_delta = cat_custom_delta(ZZod=ZZod,Z=Z,Z_y=Z_y,Z_list=Z_list,
                                  zm=zm,Q=Q,nvar=nvar,method=method,Qs=Qs)
<<<<<<< HEAD
    ### Checks for NA / NaN
    # is.nan.data.frame <- function(x)
    #    do.call(cbind, lapply(x, is.nan))

    #  if (sum(is.na(as.matrix(full_delta))) >0)
    #    print(sum(is.na(as.matrix(full_delta))))
    #  full_delta[is.nan.data.frame(full_delta)] <- 0
    # full_delta[is.na(full_delta)]=0
=======
  ### Checks for NA / NaN
   # is.nan.data.frame <- function(x)
  #    do.call(cbind, lapply(x, is.nan))

  #  if (sum(is.na(as.matrix(full_delta))) >0)
  #    print(sum(is.na(as.matrix(full_delta))))
  #  full_delta[is.nan.data.frame(full_delta)] <- 0
   # full_delta[is.na(full_delta)]=0
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
  }else{


    crs = crossing(a=1:(nvar),b=1:nvar) %>% filter(a!=b)
    blocks_id_a = level_pos[crs$a,]
    blocks_id_b = level_pos[crs$b,] %>% rename(`end_start`=start,`end_stop`=stop)
    block_ids=cbind(blocks_id_a,blocks_id_b)

    pull_block <-function(start=1,stop=1,end_start=1,end_stop=1,squared=TRUE){
      if(squared==T){
        return(ZZod[start:stop,end_start:end_stop])
      }else{
        return(Z[,start:stop])
      }
    }


    # distance_blocks = tibble(row_ind = crs$a,col_ind=crs$b,
    #                          blocks = pmap(block_ids, ~pull_block(start=..1,stop=..2,
    #                                                               squared=FALSE)
    #                          )
    # )
    # }else{
    distance_blocks = tibble(row_ind = crs$a,col_ind=crs$b,
                             blocks = pmap(block_ids, ~pull_block(start=..1,stop=..2,
                                                                  end_start=..3,end_stop=..4)
                             )
    )
    # }
    #print("distance_blocks$blocks")
    #print(distance_blocks$blocks)



    distance_blocks = distance_blocks %>%
      mutate(block_dist=map(
        .x=blocks,.f = function(x=.x){
          x[is.na(x)]=0
          phil_dist = philentropy::distance(x = x,method=catdiss,
                                            mute.message = TRUE,p=mkw_p);

          if(is_scalar_vector(phil_dist)){
            # print(is_scalar_vector(phil_dist))
            phil_dist=matrix(phil_dist,2,2);
            diag(phil_dist)=0}

          return(phil_dist)
        }
      )
      )
    # print("distance_blocks$block_dist")
<<<<<<< HEAD
    # print(distance_blocks$block_dist)
=======
     # print(distance_blocks$block_dist)
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
    #

    ####################################################################
    ####################################################################
    ### THE WEIGHTS SHOULD GO HERE #####################################
    ####################################################################
    ####################################################################

    delta_blocks = tibble(id=as.list(1:nvar)) %>%
      mutate(diag_delta = map(.x=id,
                              .f=~Reduce("+", distance_blocks %>%
                                           filter(row_ind==.x) %>%
                                           pull(block_dist))
      )
      )
    ####################################################################
    ####################################################################

    #print("delta_blocks$diag_delta")
    #print(delta_blocks$diag_delta)

    full_delta = bdiag(delta_blocks$diag_delta) %>% as.matrix
    full_delta = full_delta/(nvar-1)
  }

  # if(ab_method=="association_based"){
  #   method=ab_method
  # }
  out=list()
  out$delta_names = Z_names
  # full_delta[is.na(full_delta)]=0
  out[[method_cat]] = full_delta %>% as.matrix
  # out$delta_blocks = delta_blocks
  out$Z = Z
<<<<<<< HEAD

=======
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
  return(out)
}

