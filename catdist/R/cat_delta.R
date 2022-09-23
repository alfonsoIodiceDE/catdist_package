cat_delta <- function(x, y = NULL, method = NULL, mkw_p = 1){
  
  # library("tidyverse")
  # library("Matrix")
  # library("purrr")
  # library("fastDummies")
  # library("data.table")
  # source("R/z_preproc.R")
  # source("R/cat_custom_delta.R")
  catdiss = method
  
  # print("catdiss")
   
  
  if(is.null(dim(x))){
    Q=nlevels(x)
    }else{
    Q=map_dbl(x,nlevels)
    }
  
  
  n=nrow(x)
  nvar=length(Q)
  
  z_prep = z_preproc(x=x,y=y,Q=Q)
  #print("preprocessed")
  
  Z = z_prep$Z %>% data.matrix()
  
  
    ZZod = z_prep$ZZod
    zm = z_prep$zm
  
  
  Z_list = z_prep$Z_list
  # level_pos = z_prep$level_pos
  level_pos = data.table(start=c(1,cumsum(Q)[-length(Q)]+1),stop=cumsum(Q))
  if(!is.null(y)){
    Z_y = z_prep$Z_y
  }else{Z_y=NULL}
  
  
  
  Qs=ncol(Z)
  
  
  if(method %in% c("tot_var_dist", "gifi_chi2",
                   "supervised","matching","eskin",
                   "goodall_3","goodall_4","iof","of","lin","var_entropy","var_mutability")){
    # print("in custom")
    full_delta = cat_custom_delta(ZZod=ZZod,Z=Z,Z_y=Z_y,Z_list=Z_list,
                                  zm=zm,Q=Q,nvar=nvar,method=method,Qs=Qs)

    
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
          phil_dist = philentropy::distance(x = x,method=catdiss,
                                            mute.message = TRUE,p=mkw_p);
          if(is_scalar_vector(phil_dist)){
            
            phil_dist=matrix(phil_dist,2,2);
            diag(phil_dist)=0}
          return(phil_dist)
        }
      )
      )
    
    #print("distance_blocks$block_dist")
    #print(distance_blocks$block_dist)
    
    
    delta_blocks = tibble(id=as.list(1:nvar)) %>%
      mutate(diag_delta = map(.x=id,
                              .f=~Reduce("+", distance_blocks %>%
                                           filter(row_ind==.x) %>%
                                           pull(block_dist))
      )
      )
    
    #print("delta_blocks$diag_delta")
    #print(delta_blocks$diag_delta)
    
    full_delta = bdiag(delta_blocks$diag_delta) %>% as.matrix
    full_delta = full_delta/(nvar-1)
  }
  out=list()
  # out$full_delta = full_delta
  out[[method]] = full_delta %>% as.matrix
  # out$delta_blocks = delta_blocks
  out$Z = Z
  
  return(out)
}
