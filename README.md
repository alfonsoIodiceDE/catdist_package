catdist
====

A general framework for implementing distances for categorical variables (van de Velden, Iodice D'Enza, Markos and Cavicchia, 2023).

## Examples

### Calculate Total Variation Distance (vote)

```R
data(vote)
## calculate Total variation distance excluding the class variable, 
<<<<<<< HEAD
outvoteTV <- cdist(vote[,-ncol(vote)], method = "tot_var_dist")
=======
outvoteTV <- cdist(vote[,-17], method = "tot_var_dist")
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b

## use the class variable as response variable  
## (supervised setting using the Total variation distance);
## the input matrix is the cross-tabulation of the y variable with all other variables
outvotesupTV <- cdist(vote[,-17], y = vote[,17], method = "supervised")
outvotesupTV$distance_mat[1:5,1:5]
```

### Run KNN (australian)

```R
data(australian)

## keep the factors only
df <- australian[,c(1,4,5,6,8,9,11,12,15)]

## split to train/test
set.seed(1)
## use 80\% of dataset as training set and 20\% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]

## Apply distance-based KNN
## using the Variable Mutability dissimilarity
## the class variable (y) is the ninth variable
<<<<<<< HEAD
outaus <- cdistKNN(train, test, k = 2, method = "var_mutability")
=======
outaus <- cdistKNN(train, test, y = 9, k = 2, method = "var_mutability")
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b

## confusion matrix
table(outaus$truth, outaus$.pred)
# aricode::ARI(outaus$truth,outaus$.pred)
}
```

### Run PAM (wbcd)
```R
data(wbcd)

df <- wbcd

## split to train/test
set.seed(1)
## use 80\% of dataset as training set and 20\% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]

## Apply cdist() on the training data set
## to obtain the distance matrix
## using the supervised approach
outwbcd <- cdist(train[, -10],y = train[, 10],"supervised")


## Apply PAM on the resulting distance matrix
outpam <- pam(outwbcd$distance_mat,k = 2,diss=TRUE)

## Predict classes of new observations
newclass <- predict_pam(train[outpam$medoids,-10], test[,-10], delta = outwbcd$delta, delta_names = outwbcd$delta_names)
table(newclass,test[,10])
```
