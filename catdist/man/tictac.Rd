\name{tictac}
\alias{tictac}
\docType{data}
\title{Tic-Tac-Toe Endgame Data Set (UCI)}
\description{
This data set contains the complete set of possible board configurations at the end of tic-tac-toe games, where "x" is assumed to have played first. First nine attributes are representing nine fields on tic-tac-toe board and tenth is class attribute which contains information if x player won (i.e., true when "x" has one of 8 possible ways to create a "three-in-a-row").
}

\usage{data("tictac")}
\format{
   A data frame with 958 observations on 10 categorical variables.
  \describe{
    \item{\code{TL}}{top left square}
    \item{\code{TM}}{top middle square}
    \item{\code{TR}}{top right square}
    \item{\code{ML}}{middle left square}
    \item{\code{MM}}{middle middle square}
    \item{\code{MR}}{middle right square}
    \item{\code{BL}}{bottom left square}
    \item{\code{BM}}{bottom middle square}
    \item{\code{BR}}{bottom right square}
    \item{\code{class}}{true: x won, false: x lost}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\examples{
data(tictac)}
\keyword{datasets}