\name{wbcd}
\alias{wbcd}
\docType{data}
\title{Breast Cancer Wisconsin (Diagnostic) Data Set (UCI)}
\description{
The data set provides data for 569 patients on 30 features of the cell nuclei obtained from a digitized image of a fine needle aspirate (FNA) of a breast mass. For each patient the cancer was diagnosed as malignant or benign.
}

\usage{data("wbcd")}
\format{
   A data frame with 569 observations on the following variables.
  \describe{
    \item{\code{ID}}{ID number}
    \item{\code{Radius_mean}}{a numeric vector}
    \item{\code{Texture_mean}}{a numeric vector}
    \item{\code{Perimeter_mean}}{a numeric vector}
    \item{\code{Area_mean}}{a numeric vector}
    \item{\code{Smoothness_mean}}{a numeric vector}
    \item{\code{Compactness_mean}}{a numeric vector}
    \item{\code{Concavity_mean}}{a numeric vector}
    \item{\code{Nconcave_mean}}{a numeric vector}
    \item{\code{Symmetry_mean}}{a numeric vector}
    \item{\code{Fractaldim_mean}}{a numeric vector}
    \item{\code{Radius_se}}{a numeric vector}
    \item{\code{class}}{cancer diagnosis: M = malignant, B = benign}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\examples{
data(wbcd)}
\keyword{datasets}