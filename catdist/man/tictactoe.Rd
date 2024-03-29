\name{tictactoe}
\alias{tictactoe}
\docType{data}
\title{Tic-Tac-Toe Endgame Data Set (UCI)}
\description{
This data set contains the complete set of possible board configurations at the end of tic-tac-toe games, where "x" is assumed to have played first. First nine attributes are representing nine fields on tic-tac-toe board and tenth is response attribute which contains information if x player won (i.e., true when "x" has one of 8 possible ways to create a "three-in-a-row").
}

\usage{data("tictactoe")}
\format{
   A data frame with 958 observations on 10 categorical variables (including the class variable, "class").
  \describe{
    \item{\code{TL}}{top left square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{TM}}{top middle square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{TR}}{top right square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{ML}}{middle left square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{MM}}{middle middle square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{MR}}{middle right square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{BL}}{bottom left square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{BM}}{bottom middle square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{BR}}{bottom right square, a factor with levels \code{b}, \code{o}, \code{x}}
    \item{\code{response}}{true: \code{x} won, false: \code{x} lost}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/Tic-Tac-Toe+Endgame}

\examples{
data(tictactoe)}
\keyword{datasets}
