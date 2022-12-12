\name{catdistKNN}
\alias{catdistKNN}
\title{K-nearest neighbors of categorical data}
\usage{
catdistKNN(train_df, assess_df, y = 1, k = 2, method = "tot_var_dist", weights = 1)
}

\description{
This function applies the KNN classifier on a test data set based on the calculation of the distance between each test observation and the training observations, as described in van de Velden et al. (2023).
}
\arguments{
\item{train_df}{training observations}
\item{assess_df}{test observations}
\item{y}{the position of the response variable in the data set (default = \code{1)}}
\item{k}{the number of classes (default = \code{2)}}
\item{method}{Specifies the dissimilarity of choice (default = \code{"tot_var_dist"}). 

Options are: 

\code{"tot_var_dist"} for Total variation distance

\code{"matching"} for simple matching

\code{"eskin"} for Eskin 

\code{"goodall_3"} for Goodall 3 

\code{"goodall_4"} for Goodall 4 

\code{"iof"} for Inverse occurency frequency 

\code{"of"} for Occurence frequency 

\code{"lin"} for Lin \code{"gifi_chi2"} for chi-squared distance 

\code{"var_entropy"} for Variable Entropy

\code{"var_mutability"} for Variable Mutability

\code{"supervised"} for supervised Total variation distance; the input is the contingency table of the two-way cross-tabulations between the y variable and all other categorical variables

\code{"supervised_full"} for full supervised Total variation distance; the input is the generalized contingency table of all two-way cross-tabulations between the categorical variables

\code{"kullback-leibler"} for Kullback-Leibler divergence  

and all the method names returned by the function \code{getDistMethods()} of the package \code{philentropy}.}
\item{weights}{a vector with variable weights; its length should equal the total number of variables}
}
\details{}
\value{
\item{truth}{the vector with the true classes}
\item{.pred}{a vector with the predicted classes}
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

## Apply distance-based KNN 
## using the Variable Mutability dissimilarity
## the class variable (y) is the ninth variable
outaus <- catdistKNN(train, test, y = 9, k = 2, method = "var_mutability")

## print confusion matrix
print(table(outaus$truth, outaus$.pred))
# aricode::ARI(outaus$truth,outaus$.pred)
}
