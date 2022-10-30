select_data<-function(DF){
  
 # source("R/prepare_data.R")
  
  if (DF == "mushroom"){
    df<-read.csv("data/mushrooms.csv")
    df<-df[,-17]   # Remove constant variable
    ynum<-1       # poison  
    nclas<-2
  }
  
  if (DF == "zoo"){
    df<-read.csv("data/zoo.csv")
    ynum<-1       # class
    nclas<-7
  }
  
  
  if (DF == "vote"){
    df<-read.csv("data/voteall.csv", header=FALSE)
    ynum<-17
    nclas<-2}
  
  if (DF == "soybean"){
    df<-read.csv("data/soybean-small.csv", header =FALSE)
    ynum<-36
    nclas<-4}
  
  if (DF == "australian"){
    df<-read.csv("data/australian.csv", sep=" ",header =FALSE)
    remx<-c(2,3,7,10,13,14)
    df<-df[,-remx]
    ynum<-9
    nclas<-2}
  
  if (DF == "wbcd"){
    df<-read.csv("data/wbcd.csv", header = FALSE)
    df<-df[,-1]
    ynum<-10
    nclas<-2}
  
  if (DF == "cars"){
    df<-read.csv("data/car.data", header = FALSE)
    ynum<-1
    nclas<-4}
  
  if (DF =="audio"){
    df<-read.csv("data/audiology.standardized.data", header = FALSE)
    df<-df[,-70]
    ynum<-70
    nclas<-24
  }  
  
  if (DF == "tae"){
    df<-read.csv("data/tae.data", header = FALSE)
    ynum<-6
    nclas<-3
  }
  
  if (DF == "balance"){
    df<-read.csv("data/balance-scale.data", header = FALSE)
    ynum<-1
    nclas<-3
  }
  
  if (DF == "breastcancer"){
    df<-read.csv("data/breast-cancer.data", header = FALSE)
    ynum<-1
    nclas<-2
  }
  
 # if (DF == "dermatology"){
#    df<-read.csv("data/dermatology.data", header = FALSE)
#    ynum<-1
#    nclas<-6
#  }  
  
  if (DF == "hayesroth"){
    df<-read.csv("data/hayes-roth.data", header = FALSE)
    ynum<-6
    nclas<-3
  }
  
  if (DF == "lympho"){
    df<-read.csv("data/lymphography.data", header = FALSE)
    ynum<-1
    nclas<-4
  }
  
  if (DF == "nursery"){
    df<-read.csv("data/nursery.data", header = FALSE)
    ynum<-9
    nclas<-5
  }
  
  if (DF == "postoperative"){
    df<-read.csv("data/post-operative.data", header = FALSE)
    ynum<-9
    nclas<-3
  } 
  
  if (DF == "soybeanlarge"){
    df<-read.csv("data/soybean-large.data", header = FALSE)
    ynum<-1
    nclas<-19
  }
  
  if (DF == "tictac"){
    df<-read.csv("data/tic-tac-toe.data", header = FALSE)
    ynum<-10
    nclas<-2
  }
  
  # Prepare data (make factors, remove y, make dummies
  return(prepare_data(df,yvar=ynum,nclas=nclas))
}

