\name{soybean}
\alias{soybean}
\docType{data}
\title{Soybean (Large) Data Set (UCI)}
\description{
There are 19 classes, only the first 15 of which have been used in prior work. The folklore seems to be that the last four classes are unjustified by the data since they have so few examples. There are 35 categorical attributes, some nominal and some ordered. The value 'dna' means does not apply. The values for attributes are encoded numerically, with the first value encoded as '0', the second as '1', and so forth.
}

\usage{data("soybean")}
\format{
  A data frame with 307 observations and 36 variables (including the class attribute, "response").
  \describe{
    \item{\code{date}}{0 = april, 1=may, 2=june, 3=july, 4=august, 5=september, 6=october, ?}
    \item{\code{plant.stand}}{0=normal, 1=lt-normal, ?}
    \item{\code{precip}}{0=lt-norm, 1=norm, 2=gt-norm, ?}
    \item{\code{temp}}{0=lt-norm, 1=norm, 2=gt-norm, ?}
    \item{\code{hail}}{0=yes, 1=no, ?}
    \item{\code{crop.hist}}{0=diff-lst-year, 1=same-lst-yr, 2=same-lst-two-yrs, 3=same-lst-sev-yrs, ?}
    \item{\code{area.damaged}}{0=scattered, 1=low-areas, 2=upper-areas, 3=whole-field, ?}
    \item{\code{severity}}{0=minor, 1=pot-severe, 2=severe, ?}
    \item{\code{seed.tmt}}{0=none, 1=fungicide, 2=other, ?}
    \item{\code{germination}}{0=90-100\%, 1=80-89\%, 2=lt-80\% ,?}
    \item{\code{plant.growth}}{0=norm, 1=abnorm, ?}
    \item{\code{leaves}}{0=norm, 1=abnorm}
    \item{\code{leafspots.halo}}{0=absent, 1=yellow-halos, 2=no-yellow-halos, ?}
    \item{\code{leafspots.marg}}{0=w-s-marg, 1=no-w-s-marg, 2=dna, ?}
    \item{\code{leafspot.size}}{0=lt-1/8, 1=gt-1/8, 2=dna, ?}
    \item{\code{leaf.shread}}{0=absent, 1=present, ?}
    \item{\code{leaf.malf}}{0=absent, 1=present, ?}
    \item{\code{leaf.mild}}{0=absent, 1=upper-surf, 2=lower-surf, ?}
    \item{\code{stem}}{0=norm, 1=abnorm, ?}
    \item{\code{lodging}}{0=yes, 1=no, ?}
    \item{\code{stem.cankers}}{0=absent, 1=below-soil, 2=above-soil, 3=above-sec-nde, ?}
    \item{\code{canker.lesion}}{0=dna, 1=brown, 2=dk-brown-blk, 3=tan, ?}
    \item{\code{fruiting.bodies}}{0=absent, 1=present, ?}
    \item{\code{external.decay}}{0=absent, 1=firm-and-dry, 2=watery, ?}
    \item{\code{mycelium}}{0=absent, 1=present, ?}
    \item{\code{int.discolor}}{0=none, 1=brown, 3=black, ?}
    \item{\code{sclerotia}}{0=absent, 1=present, ?}
    \item{\code{fruit.pods}}{0=norm, 1=diseased, 2=few-present, 3=dna, ?}
    \item{\code{fruit.spots}}{0=absent, 1=colored, brown-w/blk-specks, 2=distort, 3=dna, ?}
    \item{\code{seed}}{0=norm, 1=abnorm, ?}
    \item{\code{mold.growth}}{0=absent, 1=present, ?}
    \item{\code{seed.discolor}}{0=absent, 1=present, ?}
    \item{\code{seed.size}}{0=norm, 1=lt-norm, ?}
    \item{\code{shriveling}}{0=absent, 1=present, ?}
    \item{\code{roots}}{0=norm, 1=rotted, 2=galls-cysts, ?}
       \item{\code{response}}{diaporthe-stem-canker, charcoal-rot, rhizoctonia-root-rot, phytophthora-rot, brown-stem-rot, powdery-mildew, downy-mildew, brown-spot, bacterial-blight, bacterial-pustule, purple-seed-stain, anthracnose, phyllosticta-leaf-spot, alternarialeaf-spot, frog-eye-leaf-spot, diaporthe-pod-&-stem-blight, cyst-nematode, 2-4-d-injury, herbicide-injury}
}
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/Soybean+(Large)}

\examples{
data(soybean)}
\keyword{datasets}
