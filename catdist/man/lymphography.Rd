\name{lymphography}
\alias{lymphography}
\docType{data}
\title{The Lymphography Domain Data Set (UCI)}
\description{
This is lymphography domain obtained from the University Medical Centre, Institute of Oncology, Ljubljana, Yugoslavia. It was repeatedly used in the machine learning literature.
}

\usage{data("lymphography")}
\format{
   A data frame with 147 observations on the following 19 variables.
  \describe{
    \item{\code{class}}{a factor with levels \code{normalfind}, \code{metastases}, \code{malignlymph}, \code{fibrosis}}
    \item{\code{lymphatics}}{a factor with levels \code{normal}, \code{arched}, \code{deformed}, \code{displaced}}
    \item{\code{blockofaffere}}{a factor with levels \code{no}, \code{yes}}
    \item{\code{bloflymphc}}{a factor with levels \code{no}, \code{yes}}
    \item{\code{bloflymphs}}{a factor with levels \code{no}, \code{yes}}
    \item{\code{bypass}}{a factor with levels \code{no}, \code{yes}}
    \item{\code{extravasates}}{a factor with levels \code{no}, \code{yes}}
    \item{\code{regenerationof}}{a factor with levels \code{no}, \code{yes}}
    \item{\code{earlyuptakein}}{a factor with levels \code{no}, \code{yes}}
    \item{\code{lymnodesdimin}}{a factor with levels \code{0}, \code{1}, \code{2}, \code{3}}
      \item{\code{lymnodesenlar}}{a factor with levels \code{1}, \code{2}, \code{3}, \code{4}}
     \item{\code{changesinlym}}{a factor with levels \code{oval}, \code{round}}
     \item{\code{defectinnode}}{a factor with levels \code{no}, \code{lacunar}, \code{lacmarginal}, \code{laccentral}}
     \item{\code{changesinnode}}{a factor with levels \code{no}, \code{lacunar}, \code{lacmarginal}, \code{laccentral}}  
     \item{\code{changesinstru}}{a factor with levels \code{no}, \code{grainy}, \code{droplike}, \code{coarse}, \code{diluted}, \code{reticular}, \code{stripped}, \code{faint}}  
     \item{\code{specialforms}}{a factor with levels \code{no}, \code{chalices}, \code{vesicles}}  
     \item{\code{dislocationof}}{a factor with levels \code{no}, \code{yes}}  
     \item{\code{exclusionofno}}{a factor with levels \code{no}, \code{yes}}  
     \item{\code{noofnodesin}}{a factor with levels \code{0-9}, \code{10-19}, \code{20-29}, \code{30-39}, \code{40-49}, \code{50-59}, \code{60-69}, \code{>=70}}  
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