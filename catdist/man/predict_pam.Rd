\name{predict_pam}
\alias{predict_pam}
\title{Prediction function for Partitioning Around Medoids (PAM)}
\usage{
predict_pam(medoids, newdata, delta, delta_names)
}

\description{
This function finds predicted classes of new observatios based on the medoids already obtained from PAM on the original observations, as described in van de Velden et al. (2023).
}

\arguments{
\item{medoids}{medoids obtained via PAM on a set of observations}
\item{newdata}{new observations}
\item{delta}{a block diagonal matrix with separate category dissimilarity matrices as diagonal blocks}
\item{delta_names}{a vector with variable category names}
}

<<<<<<< HEAD
=======
\details{}
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
\value{
\item{pred}{a vector with the predicted classes}
}
\examples{
data(australian)

## keep the factors only
df <- australian[,c(1,4,5,6,8,9,11,12,15)]

## split to train/test
set.seed(1)
## use 80\% of dataset as training set and 20\% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]

## Apply cdist() on the training data set
## to obtain the distance matrix
## using the supervised approach
outaus <- cdist(train[, -9],y = train[, 9],"supervised")

## Apply PAM on the resulting distance matrix
outpam <- cluster::pam(outaus$distance_mat,k = 2,diss=TRUE)

## Predict classes of new observations
<<<<<<< HEAD
newclass <- predict_pam(train[outpam$medoids,-9], test[,-9],
delta = outaus$delta, delta_names = outaus$delta_names)
=======
newclass <- predict_pam(train[outpam$medoids,-9], test[,-9], delta = outaus$delta, delta_names = outaus$delta_names)
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b

## print confusion matrix
print(table(newclass, test[,9]))
# aricode::ARI(newclass, test[,9])
}
