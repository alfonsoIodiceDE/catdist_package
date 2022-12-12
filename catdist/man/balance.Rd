\name{balance}
\alias{balance}
\docType{data}
\title{Balance Scale Data Set (UCI)}
\description{
This data set was generated to model psychological experimental results. Each example is classified as having the balance scale tip to the right, tip to the left, or be balanced. The attributes are the left weight, the left distance, the right weight, and the right distance. The correct way to find the class is the greater of (left-distance x left-weight) and (right-distance x right-weight). If they are equal, it is balanced.
}

\usage{data("balance")}
\format{
   A data frame with 625 rows and 5 variables (including the class variable, "class").
  \describe{
    \item{\code{Left.Weight}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{Left.Distance}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{Right.Weight}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{Right.Distance}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{class}}{one of \code{L}, \code{B} or \code{R}}
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
