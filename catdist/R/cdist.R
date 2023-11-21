cdist<-function(x,y=NULL,method="tot_var_dist", weights=1){
  #source("R/cat_delta.R")
  #source("R/cat_custom_delta.R")
  full_delta <- NULL
  level_stop <- NULL
  level_start <- NULL
  response <- NULL
  delta_tmp <- NULL
  all_of <- NULL
  map2 <- NULL
  .x <- NULL
  a <- NULL
  b <- NULL
  blocks <- NULL
  id <- NULL
  if(length(method)==1){

    # print(method)

    out_delta = cat_delta(x=x,y=y,method_cat=method)
    delta = out_delta[[method]] %>% data.matrix
    delta_names = out_delta$delta_names
    ########################################################################
    ########################################################################
    ########################################################################
    # delta[is.na(delta)]=0
    ########################################################################
    ########################################################################
    ########################################################################

    Z = out_delta$Z %>% data.matrix

    if(is.null(dim(x))){
      Q=nlevels(x)
    }else{
      Q=map_dbl(x,nlevels)
    }

    if(is.null(dim(weights))){
      if(length(weights) == 1){
        distance_mat = Z  %*% delta %*%  t(Z)
      }else{
        weightsexp =NULL
        for (i in 1:ncol(x)) {
          weightsexp = c(weightsexp,rep(weights[i],Q[i]))
        }
        W=diag(weightsexp,nrow=length(weightsexp),ncol=length(weightsexp))
        distance_mat = Z %*% W %*% delta %*% W %*% t(Z)
      }
    }else{ # Weights is a matrix
      weights = diag(weights)
      weightsexp =NULL
      for (i in 1:ncol(x)) {
        weightsexp = c(weightsexp,rep(weights[i],Q[i]))
      }
      W=diag(weightsexp)
      distance_mat = Z %*% W %*% delta %*% W %*% t(Z)
    }
  }else{ #differnt method for each variable

    #### THIS STUFF MUST GO
    # library(ca)
    # library(tidyverse)
    # library(fastDummies)
    # library(data.table)
    # library(Matrix)
    # data(wg93)
    # x = wg93
    #
    # method_vec = c("tot_var_dist", "gifi_chi2",
    #                "matching",
    #                "goodall_3","eskin","goodall_4","tot_var_dist")
    # ####
    method_vec = method

    if(is.null(dim(x))){
      Q=nlevels(x)
    }else{
      Q=map_dbl(x,nlevels)
    }
    nvar=length(Q)
    level_pos = data.table(start=c(1,cumsum(Q)[-length(Q)]+1),stop=cumsum(Q))

    delta_structure = tibble(method = method_vec) %>% mutate(
      x=map(.x=method_vec,~as_tibble(x)),
      delta_tmp=map2(.x = x,.y = method,.f=~cat_delta(x=.x,method=.y)),
      full_delta = map(.x=delta_tmp,.f=~.x[[1]]),
      level_start = level_pos$start,
      level_stop = level_pos$stop,
      delta_block=pmap(.l=list(..1 = full_delta,..2=level_start,..3=level_stop),.f=~..1[..2:..3,..2:..3])
    )

    Z = delta_structure$delta_tmp[[1]]$Z %>% as.matrix()

    delta = bdiag(delta_structure$delta_block) %>% as.matrix
    if(is.null(dim(weights))){
      if(length(weights) == 1){
        distance_mat = Z  %*% delta %*%  t(Z)
      }else{

        weightsexp =NULL
        for (i in 1:ncol(x)) {
          weightsexp = c(weightsexp,rep(weights[i],Q[i]))
        }
        W=diag(weightsexp,nrow=length(weightsexp),ncol=length(weightsexp))

        distance_mat = Z %*% W %*% delta %*% W %*% t(Z)
      }
    }else{ # weights is a matrix
      weights = diag(weights)
      weightsexp =NULL
      for (i in 1:ncol(x)) {
        weightsexp = c(weightsexp,rep(weights[i],Q[i]))
      }
      W=diag(weightsexp)
      distance_mat = Z %*% W %*% delta %*% W %*% t(Z)
    }

  }

  # print(dim(distance_mat))
  # if(is.null(out_delta)){
  #    return(distance_mat)
  #  }else{
  out_catdist = list()
  out_catdist$distance_mat = distance_mat
  out_catdist$delta = delta
  out_catdist$delta_names = delta_names
  return(out_catdist)
  # }
}
