\name{australian}
\alias{australian}
\docType{data}
\title{
Australian Credit Approval Data Set (UCI)}
\description{
This data set concerns credit card applications. It consists of 14 variables and 1 class label that quantifies the approval decision. Not much is known about the 14 features themselves for the sake of confidentiality.
}

\usage{data("australian")}
\format{
   A data frame with 690 observations and 15 variables (including the class variable, "class")
  \describe{
    \item{\code{A1}}{\code{0}, \code{1}}
    \item{\code{A2}}{continuous}
    \item{\code{A3}}{continuous}
    \item{\code{A4}}{\code{1}, \code{2}, \code{3}}
    \item{\code{A5}}{\code{1} to \code{14}}
    \item{\code{A6}}{\code{1} to \code{9}}
    \item{\code{A7}}{continuous}
    \item{\code{A8}}{\code{0}, \code{1}}
    \item{\code{A9}}{\code{0}, \code{1}}
    \item{\code{A10}}{continuous}
    \item{\code{A11}}{\code{0}, \code{1}}
    \item{\code{A12}}{\code{1}, \code{2}, \code{3}}
    \item{\code{A13}}{continuous}
    \item{\code{A14}}{continuous}
    \item{\code{class}}{\code{0}, \code{1}}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/statlog+(australian+credit+approval)}


\examples{
data(australian)}
\keyword{datasets}