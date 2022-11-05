\name{cars}
\alias{cars}
\docType{data}
\title{Car Evaluation Data Set (UCI)}
\description{
This data set contains car information with 7 attributes, including one class attribute that tells whether the car is in acceptable conditions. 
}

\usage{data("cars")}
\format{
   A data frame with 1728 observations on 7 variables (including the class attribute, "acceptability"). 
  \describe{
    \item{\code{buying}}{Buying price of the car (\code{high}, \code{low}, \code{med} ,\code{vhigh})}
    \item{\code{maint}}{Price of the maintenance (\code{high}, \code{low}, \code{med}, \code{vhigh})}
    \item{\code{doors}}{Number of doors (\code{2}, \code{3}, \code{4}, \code{5more})}
    \item{\code{persons}}{Capacity in terms of persons to carry (\code{2}, \code{4}, \code{more})}
    \item{\code{lug_boot}}{Size of luggage boot (\code{big}, \code{med}, \code{small})}
    \item{\code{safety}}{Estimated safety of the car (\code{high}, \code{low}, \code{med})}
    \item{\code{acceptability}}{\code{unacc}, \code{acc}, \code{good}, \code{vgood}}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/car+evaluation}

\examples{
data(cars)}
\keyword{datasets}