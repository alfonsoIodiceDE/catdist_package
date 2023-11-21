\name{lymphography}
\alias{lymphography}
\docType{data}
\title{The Lymphography Domain Data Set (UCI)}
\description{
The information in the data set is intended to detect a lymphoma and its current state.
}

\usage{data("lymphography")}
\format{
   A data frame with 148 observations on the following 19 variables (including the class attribute).
  \describe{
    \item{\code{lymphatics}}{a factor with levels \code{1=normal}, \code{2=arched}, \code{3=deformed}, \code{4=displaced}}
    \item{\code{blockofaffere}}{a factor with levels \code{1=no}, \code{2=yes}}
    \item{\code{bloflymphc}}{a factor with levels \code{1=no}, \code{2=yes}}
    \item{\code{bloflymphs}}{a factor with levels \code{1=no}, \code{2=yes}}
    \item{\code{bypass}}{a factor with levels \code{1=no}, \code{1=yes}}
    \item{\code{extravasates}}{a factor with levels \code{1=no}, \code{2=yes}}
    \item{\code{regenerationof}}{a factor with levels \code{1=no}, \code{2=yes}}
    \item{\code{earlyuptakein}}{a factor with levels \code{1=no}, \code{2=yes}}
    \item{\code{lymnodesdimin}}{a factor with levels \code{1}, \code{2}, \code{3}}
      \item{\code{lymnodesenlar}}{a factor with levels \code{1}, \code{2}, \code{3}, \code{4}}
     \item{\code{changesinlym}}{a factor with levels \code{1=bean}, \code{2=oval}, \code{3=round}}
     \item{\code{defectinnode}}{a factor with levels \code{1=no}, \code{2=lacunar}, \code{3=lacmarginal}, \code{4=laccentral}}
     \item{\code{changesinnode}}{a factor with levels \code{1=no}, \code{2=lacunar}, \code{3=lacmarginal}, \code{4=laccentral}}
     \item{\code{changesinstru}}{a factor with levels \code{1=no}, \code{2=grainy}, \code{3=droplike}, \code{4=coarse}, \code{5=diluted}, \code{6=reticular}, \code{7=stripped}, \code{8=faint}}
     \item{\code{specialforms}}{a factor with levels \code{1=no}, \code{2=chalices}, \code{3=vesicles}}
     \item{\code{dislocationof}}{a factor with levels \code{1=no}, \code{2=yes}}
     \item{\code{exclusionofno}}{a factor with levels \code{1=no}, \code{2=yes}}
     \item{\code{noofnodesin}}{a factor with levels \code{1=0-9}, \code{2=10-19}, \code{3=20-29}, \code{4=30-39}, \code{5=40-49}, \code{6=50-59}, \code{7=60-69}, \code{8=>=70}}
       \item{\code{response}}{a factor with levels \code{1=normalfind}, \code{2=metastases}, \code{3=malignlymph}, \code{4=fibrosis}}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

This lymphography domain was obtained from the University Medical Centre, Institute of Oncology, Ljubljana, Yugoslavia. Thanks go to M. Zwitter and M. Soklic for providing the data.
}

\source{https://archive.ics.uci.edu/ml/datasets/Lymphography}

\examples{
data(lymphography)}
\keyword{datasets}
