\name{balance}
\alias{balance}
\docType{data}
\title{Balance Scale Data Set (UCI)}
\description{
This data set was generated to model psychological experimental results. Each example is classified as having the balance scale tip to the right, tip to the left, or be balanced. The attributes are the left weight, the left distance, the right weight, and the right distance.
}

\usage{data("balance")}
\format{
<<<<<<< HEAD
   A data frame with 625 rows and 5 variables (including the class variable, "response").
=======
   A data frame with 625 rows and 5 variables (including the class variable, "class").
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
  \describe{
    \item{\code{Left.Weight}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{Left.Distance}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{Right.Weight}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{Right.Distance}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
<<<<<<< HEAD
    \item{\code{response}}{one of \code{L}, \code{B} or \code{R}}
=======
    \item{\code{class}}{one of \code{L}, \code{B} or \code{R}}
>>>>>>> 72e0c5b6d717da613d07c4e0170eb887c0fe354b
}
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/
\

Balance+Scale}

\examples{
data(balance)}
\keyword{datasets}
