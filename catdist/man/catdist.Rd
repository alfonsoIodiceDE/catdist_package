\name{catdist}
\alias{catdist}
\title{Computing distances for categorical variables}
\usage{
catdist(x, y = NULL, method = "tot_var_dist")
}
\description{
This function implements ...
}
\arguments{
\item{x}{Dataset with categorical variables (factors)}
\item{y}{Response variable (default = \code{NULL)}}
\item{method}{Specifies the dissimilarity of choice. Options are \code{"tot_var_dist"} for Total variation distance, \code{"matching"} for simple matching, \code{"eskin"} for Eskin, \code{"goodall_3"} for Goodall 3, \code{"goodall_4"} for Goodall 4, \code{"iof"} for Inverse occurency frequency, \code{"of"} for Occurence frequency, \code{"lin"} for Lin, \code{"gifi_chi2"} for chi-squared distance, \code{"var_entropy"} for Variable Entropy, \code{"var_mutability"} for Variable Mutability, 
         \code{"supervised"} for supervised total variation distance,\code{"supervised_full"} for full supervised total variation distance, \code{"kullback-leibler"} for Kullback-Leibler divergence (default = \code{"tot_var_dist"})}
\item{weigths}{}
\item{out_delta}{(default = \code{NULL)}}
}
\details{}
\value{
\item{distance matrix}{a distance matrix}
\item{delta}{a matrix}
\item{delta_names}{a vector}
}
\examples{

}
