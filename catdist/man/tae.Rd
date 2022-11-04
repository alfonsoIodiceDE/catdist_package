\name{tae}
\alias{tae}
\docType{data}
\title{Teaching Assistant Evaluation Data Set (UCI)}
\description{
The data consist of evaluations of teaching performance over three regular semesters and two summer semesters of 151 teaching assistant (TA) assignments at the Statistics Department of the University of Wisconsin-Madison. The scores were divided into 3 roughly equal-sized categories ("low", "medium", and "high") to form the class variable.
}

\usage{data("tae")}
\format{
   A data frame with 151 observations on 6 variables.
  \describe{
    \item{\code{TA}}{Whether of not the TA is a native English speaker (binary); 1=English speaker, 2=non-English speaker}
    \item{\code{Instructor}}{Course instructor (categorical, 25 categories)}
    \item{\code{Course}}{Course (categorical, 26 categories)}
    \item{\code{Semester}}{Summer or regular semester (binary) 1=Summer, 2=Regular}
    \item{\code{Class_size}}{Class size (numerical)}
    \item{\code{class}}{1=Low, 2=Medium, 3=High}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\examples{
data(tae)}
\keyword{datasets}