\name{wbcd}
\alias{wbcd}
\docType{data}
\title{Breast Cancer Wisconsin (Original) Data Set (UCI)}
\description{
The data set provides data for 699 cancer patients on 10 features of the cell nuclei obtained from a digitized image of a fine needle aspirate (FNA) of a breast mass. For each patient the cancer was diagnosed as malignant or benign. 16 patients have missing values in the bare_nuclei variable, indicated as \code{?}).
}

\usage{data("wbcd")}
\format{
   A data frame with 699 observations on the following variables (including the class variable, "class").
  \describe{
    \item{\code{clump_thickness}}{\code{1} to \code{10}}
    \item{\code{cell_size_uniformity}}{\code{1} to \code{10}}
    \item{\code{cell_shape_uniformity}}{\code{1} to \code{10}}
    \item{\code{marginal_adhesion}}{\code{1} to \code{10}}
    \item{\code{single_epithelial_cell_size}}{\code{1} to \code{10}}
    \item{\code{bare_nuclei}}{\code{1} to \code{10}, \code{?} denotes missing}
    \item{\code{bland_chromatin}}{\code{1} to \code{10}}
    \item{\code{normal_nucleoli}}{\code{1} to \code{10}}
    \item{\code{mitoses}}{\code{1} to \code{10}}
    \item{\code{response}}{cancer diagnosis: \code{2} for benign, \code{4} for malignant}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(original)}


\examples{
data(wbcd)}
\keyword{datasets}
