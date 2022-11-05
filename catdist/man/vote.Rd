\name{vote}
\alias{vote}
\docType{data}
\title{Congressional Voting Records Data Set (UCI)}
\description{
This data set includes votes for each of the U.S. House of Representatives Congressmen on the 16 key votes identified by the CQA. The CQA lists nine different types of votes: voted for, paired for, and announced for (these three simplified to yea), voted against, paired against, and announced against (these three simplified to nay), voted present, voted present to avoid conflict of interest, and did not vote or otherwise make a position known (these three simplified to an unknown disposition). The first variable indicates the political party of the congressmen.
}

\usage{data("vote")}
\format{
   A data frame with 434 observations on 16 different key issues. 
  \describe{
    \item{\code{class}}{\code{democrat}, \code{republican}}
    \item{\code{handicapped-infants}}{\code{y}, \code{n}}
    \item{\code{water-project-cost-sharing}}{\code{y}, \code{n}}
    \item{\code{adoption-of-the-budget-resolution}}{\code{y}, \code{n}}
    \item{\code{physician-fee-freeze}}{\code{y}, \code{n}}
    \item{\code{el-salvador-aid}}{\code{y}, \code{n}}
    \item{\code{religious-groups-in-schools}}{\code{y}, \code{n}}
    \item{\code{anti-satellite-test-ban}}{\code{y}, \code{n}}
    \item{\code{aid-to-nicaraguan-contras}}{\code{y}, \code{n}}
    \item{\code{mx-missile}}{\code{y}, \code{n}}
    \item{\code{immigration}}{\code{y}, \code{n}}
    \item{\code{synfuels-corporation-cutback}}{\code{y}, \code{n}}
     \item{\code{education-spending}}{\code{y}, \code{n}}
       \item{\code{superfund-right-to-sue}}{\code{y}, \code{n}}
          \item{\code{crime}}{\code{y}, \code{n}}
        \item{\code{duty-free-exports}}{\code{y}, \code{n}}
        \item{\code{export-administration-act-south-africa}}{\code{y}, \code{n}}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\examples{
data(vote)}
\keyword{datasets}