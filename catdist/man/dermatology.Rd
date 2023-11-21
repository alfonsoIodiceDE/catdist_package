\name{dermatology}
\alias{dermatology}
\docType{data}
\title{Dermatology (UCI)}
\description{
This data set contains 366 patients (8 with missing values) with 34 attributes, which is partitioned into 6 types of erythemato-squamous disease. The first 12 attributes are clinical (erythema, scaling, etc.), the next 22 are histopathological (decrease of melanin, spongiosis, etc.) and the last one is a class variable with six different skin diseases. The family history feature has the value 1 if any of these diseases has been observed in the family, and 0 otherwise. The age feature simply represents the age of the patient. Every other feature (clinical and histopathological) was given a degree in the range of 0 to 3. Here, 0 indicates that the feature was not present, 3 indicates the largest amount possible, and 1, 2 indicate the relative intermediate values.
}

\usage{data("dermatology")}
\format{
   A data frame with 366 observations on 35 variables (including the class attribute, "response").
  \describe{
    \item{\code{erythema}}{\code{0} to \code{3}}        \item{\code{scaling}}{\code{0} to \code{3}}         \item{\code{definite.borders}}{\code{0} to \code{3}}
     \item{\code{itching}}{\code{0} to \code{3}}      \item{\code{koebner.phenomenon}}{\code{0} to \code{3}}
\item{\code{polygonal.papules}}{\code{0} to \code{3}}
\item{\code{follicular.papules}}{\code{0} to \code{3}}
\item{\code{oral.mucosal.involvement}}{\code{0} to \code{3}}
\item{\code{knee.and.elbow.involvement}}{\code{0} to \code{3}}
\item{\code{scalp.involvement}}{\code{0} to \code{3}}
\item{\code{family.history}}{\code{0} or \code{1}}
\item{\code{Age}}{Age of the patient}               \item{\code{melanin.incontinence}}{\code{0} to \code{3}}
\item{\code{eosinophils.in.the.infiltrate}}{\code{0} to \code{3}}
\item{\code{PNL.infiltrate}}{\code{0} to \code{3}}
\item{\code{fibrosis.of.the.papillary.dermis}}{\code{0} to \code{3}}
\item{\code{exocytosis}}{\code{0} to \code{3}}
\item{\code{acanthosis}}{\code{0} to \code{3}}
\item{\code{hyperkeratosis}}{\code{0} to \code{3}}  \item{\code{parakeratosis}}{\code{0} to \code{3}}   \item{\code{clubbing.of.the.rete.ridges}}{\code{0} to \code{3}}
\item{\code{elongation.of.the.rete.ridges}}{\code{0} to \code{3}}
\item{\code{thinning.of.the.suprapapillary.epidermis}}{\code{0} to \code{3}}
\item{\code{spongiform.pustule}}{\code{0} to \code{3}}
\item{\code{munro.microabcess}}{\code{0} to \code{3}}
\item{\code{focal.hypergranulosis}}{\code{0} to \code{3}}
\item{\code{disappearance.of.the.granular.layer}}{\code{0} to \code{3}}
\item{\code{vacuolisation.and.damage.of.basal.layer}}{\code{0} to \code{3}}
\item{\code{spongiosis}}{\code{0} to \code{3}}
\item{\code{saw.tooth.appearance.of.retes}}{\code{0} to \code{3}}
\item{\code{follicular.horn.plug}}{\code{0} to \code{3}}
 \item{\code{perifollicular.parakeratosis}}{\code{0} to \code{3}}
\item{\code{inflammatory.monoluclear.inflitrate}}{\code{0} to \code{3}}                 \item{\code{band.like.infiltrate}}{\code{0} to \code{3}}
\item{\code{response}}{\code{1=psoriasis}, \code{2=seboreic dermatitis}, \code{3=lichen planus}, \code{4=pityriasis rosea}, \code{5=cronic dermatitis}, \code{6=pityriasis rubra pilaris}}
  }
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/dermatology}

\examples{
data(dermatology)}
\keyword{datasets}
