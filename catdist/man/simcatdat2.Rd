\name{simcatdat2}
\alias{simcatdat2}
\docType{data}
\title{
Simulated categorical data with a strong association with the cluster structure}
\description{
The dataset consists of 1,000 simulated observations, each described by 13 categorical attributes, and is segmented into four distinct clusters. Among these attributes, the initial four are identified as active, playing a significant role in the dataset, while the subsequent eight are deemed noise variables, contributing no direct relevance. The active variables exhibit a strong association with the response variable, as evidenced by Cramér's V values of approximately 0.7. In contrast, while the noise variables lack an association with the response, they do exhibit inter-variable associations, with pairwise Cramér's V values of approximately 0.3. Further details on the data generation process are available in the study by Van de Velden et al. (2017).
}

\usage{data("simcatdat2")}
\format{
   A data frame with 1000 observations and 13 variables (including the response variable, "response")
  \describe{
    \item{\code{a}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{b}}{one of \code{1}, \code{2}, \code{3}, or \code{4}}
    \item{\code{c}}{one of \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, or \code{6}}
    \item{\code{d}}{\code{1} pr \code{2}}
    \item{\code{i}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{j}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{k}}{one of \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, or \code{6}}
    \item{\code{l}}{one of \code{1}, \code{2}, \code{3}, \code{4}, or \code{5}}
    \item{\code{m}}{one of \code{1}, \code{2}, or \code{3}}
    \item{\code{n}}{one of \code{1}, \code{2}, or \code{3}}
    \item{\code{o}}{one of \code{1}, \code{2}, \code{3}, \code{4}, \code{5}, \code{6} or \code{7}}
    \item{\code{p}}{\code{1}, or \code{2}}
    \item{\code{response}}{\code{1}, \code{2}, \code{3} or \code{4}}
  }
}
\references{
van de Velden, M., Iodice D'Enza, A., & Palumbo, F. (2017). Cluster correspondence analysis. *Psychometrika*, *82*, 158-185.
}

\source{van de Velden, M., Iodice D'Enza, A., & Palumbo, F. (2017). Cluster correspondence analysis. *Psychometrika*, *82*, 158-185.}

\examples{
data(simcatdat2)}
\keyword{datasets}
