\name{catdist}
\alias{catdist}
\title{Computing distances for categorical variables}
\usage{
catdist(x,y=NULL,method="tot_var_dist")
}
\description{
This function implements ...
}
\arguments{
\item{x}{Dataset with categorical variables (factors)}
\item{y}{Response variable (default = \code{NULL)}}
\item{method}{Specifies the dissimilarity of choice. Options are "tot_var_dist" for Total variation distance, "matching" for simple matching, "eskin" for Eskin, "goodall_3" for Goodall 3, "goodall_4" for Goodall 4, "iof" for Inverse occurency frequency, "of" for Occurence frequency, "lin" for Lin, "gifi_chi2" for chi-squared distance, "var_entropy" for Variable Entropy, "var_mutability" for Variable Mutability, 
         "supervised" for supervised total variation distance,"supervised_full" for full supervised total variation distance, "kullback-leibler" for Kullback-Leibler divergence (default = \code{"tot_var_dist"})}
\item{weigths}{}
\item{out_delta}{(default = \code{NULL)}}
}
\details{}
\value{
\item{distance matrix}{a distance matrix}
}
\examples{

}
