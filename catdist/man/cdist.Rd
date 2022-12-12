\name{cdist}
\alias{cdist}
\title{Computing distances for categorical variables}
\usage{
cdist(x, y = NULL, method = "tot_var_dist", weights = 1)
}
\description{
This function computes the distance matrix between observations described by categorical variables based on the framework proposed in van de Velden, Iodice D'Enza, Markos, & Cavicchia (2023).
}
\arguments{
\item{x}{a data set with categorical variables (as factors)}
\item{y}{a vector of the response variable (default = \code{NULL)}}
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

\value{
\item{distance matrix}{a matrix with distances between observations}
\item{delta}{a block diagonal matrix with separate category dissimilarity matrices as diagonal blocks}
\item{delta_names}{a vector with variable category names}
}
\examples{
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

}
