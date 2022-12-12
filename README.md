catdist
====

A general framework for implementing distances for categorical variables (van de Velden, Iodice D'Enza, Markos and Cavicchia, 2023)

## Examples

```R
data(vote)
## calculate Total variation distance excluding the class variable, 
outvoteTV <- cdist(vote[,-17], method = "tot_var_dist")

## calculate Hellinger distance excluding the class variable;
## this distance is calculated via the philentropy package, 
## use philentropy::getDistMethods() to see all options
outvoteHE <- cdist(vote[,-17], method = "hellinger")

## doubles the weight of the first variable; 
## the remaining 15 variables have weight equal to 1
outvoteWHE <- cdist(vote[,-17], method="hellinger", weights = c(rep(2,1),rep(1,15)))

## use the class variable as response variable  
## (supervised setting using the Total variation distance);
## the input matrix is the cross-tabulation of the y variable with all other variables
outvotesupTV <- cdist(vote[,-17], y = vote[,17], method = "supervised")

## use the class variable as response variable  (supervised setting)
outvotesupTV <- cdist(vote[,-17], y = vote[,17], method = "supervised_full")
```
