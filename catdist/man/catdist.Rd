\name{catdist}
\alias{catdist}
\title{Computing distances for categorical variables}
\usage{
catdist(x, y = NULL, method = "tot_var_dist", weights = 1)
}
\description{
This function computes the distance matrix between observations described by categorical variables based on the framework proposed in van de Velden, Iodice D'Enza, Markos, & Cavicchia (2023).
}
\arguments{
\item{x}{a data set with categorical variables (as factors)}
\item{y}{a vector of the response variable (default = \code{NULL)}}
\item{method}{Specifies the dissimilarity of choice. Options are \code{"tot_var_dist"} for Total variation distance, \code{"matching"} for simple matching, \code{"eskin"} for Eskin, \code{"goodall_3"} for Goodall 3, \code{"goodall_4"} for Goodall 4, \code{"iof"} for Inverse occurency frequency, \code{"of"} for Occurence frequency, \code{"lin"} for Lin, \code{"gifi_chi2"} for chi-squared distance, \code{"var_entropy"} for Variable Entropy, \code{"var_mutability"} for Variable Mutability, 
         \code{"supervised"} for supervised total variation distance,\code{"supervised_full"} for full supervised total variation distance, \code{"kullback-leibler"} for Kullback-Leibler divergence (default = \code{"tot_var_dist"}) and all the method names returned by the function \code{getDistMethods()} of the package \code{philentropy}.}
\item{weights}{a vector with variable category weights; its length should equal the total number of categories}
}
\details{}
\value{
\item{distance matrix}{a matrix with distances between observations}
\item{delta}{a block diagonal matrix with separate category dissimilarity matrices as diagonal blocks}
\item{delta_names}{a vector with variable category names}
}
\examples{
data(vote)
## calculate Total variation distance excluding the class variable, 
outvoteTV <- catdist(vote[,-17], method = "tot_var_dist")

## calculate Hellinger distance excluding the class variable; this distance is calculated via the philentropy package, use philentropy::getDistMethods() to see all options
outvoteHE <- catdist(vote[,-17], method = "hellinger")

## use the class variable as response variable  (supervised setting)
outvotesupTV <- catdist(vote[,-17], y = vote[,17], method = "tot_var_dist")

## double the weight of the categories of the first variable (supervised setting)
outvotesupWTV <- catdist(vote[,-17], y = vote[,17], method = "tot_var_dist", weights = c(rep(2,2),rep(1,46)))
}
