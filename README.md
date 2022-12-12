catdist
====

A general framework for implementing distances for categorical variables (van de Velden, Iodice D'Enza, Markos and Cavicchia, 2023).

## Examples

```R

data(vote)
## calculate Total variation distance excluding the class variable, 
outvoteTV <- cdist(vote[,-17], method = "tot_var_dist")

## use the class variable as response variable  
## (supervised setting using the Total variation distance);
## the input matrix is the cross-tabulation of the y variable with all other variables
outvotesupTV <- cdist(vote[,-17], y = vote[,17], method = "supervised")

####### ####### ####### ####### #####
####### Run KNN on australian ####### 
####### ####### ####### ####### #####

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
outaus <- cdistKNN(train, test, y = 9, k = 2, method = "var_mutability")

## print confusion matrix
print(table(outaus$truth, outaus$.pred))
# aricode::ARI(outaus$truth,outaus$.pred)
}


```
