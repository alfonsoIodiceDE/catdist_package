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
         \code{"supervised"} for supervised total variation distance,\code{"supervised_full"} for full supervised total variation distance, \code{"kullback-leibler"} for Kullback-Leibler divergence (default = \code{"tot_var_dist"})}
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
# exclude the class variable
outvote <- catdist(vote[,-17], method = "tot_var_dist")

# excluding the class variable and use it as response variable in a supervised setting
outvotesup <- catdist(vote[,-17], y = vote[,17], method = "tot_var_dist")

# use specific category weights; double the weight of the categories of the first variable
outvotesup <- catdist(vote[,-17], y = vote[,17], method = "tot_var_dist", weights = c(rep(2,2),rep(1,46)))
}
