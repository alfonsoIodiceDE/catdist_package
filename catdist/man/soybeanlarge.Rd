\name{soybeanlarge}
\alias{soybeanlarge}
\docType{data}
\title{Soybean (Large) Data Set (UCI)}
\description{
There are 19 classes, only the first 15 of which have been used in prior work. The folklore seems to be that the last four classes are unjustified by the data since they have so few examples. There are 35 categorical attributes, some nominal and some ordered. The value 'dna' means does not apply. The values for attributes are encoded numerically, with the first value encoded as '0', the second as '1', and so forth. 
}

\usage{data("soybeanlarge")}
\format{
  A data frame with 307 observations and 36 variables (including the class attribute, "class"). 
  \describe{
      \item{\code{class}}{diaporthe-stem-canker, charcoal-rot, rhizoctonia-root-rot, phytophthora-rot, brown-stem-rot, powdery-mildew, downy-mildew, brown-spot, bacterial-blight, bacterial-pustule, purple-seed-stain, anthracnose, phyllosticta-leaf-spot, alternarialeaf-spot, frog-eye-leaf-spot, diaporthe-pod-&-stem-blight, cyst-nematode, 2-4-d-injury, herbicide-injury}
    \item{\code{date}}{april, may, june, july, august, september, october, ?}
    \item{\code{plant-stand}}{normal, lt-normal, ?}
    \item{\code{precip}}{lt-norm, norm, gt-norm, ?}
    \item{\code{temp}}{lt-norm, norm, gt-norm, ?}
    \item{\code{hail}}{yes, no, ?}
    \item{\code{crop-hist}}{diff-lst-year, same-lst-yr, same-lst-two-yrs, same-lst-sev-yrs, ?}
    \item{\code{area-damaged}}{scattered, low-areas, upper-areas, whole-field, ?}
    \item{\code{severity}}{minor, pot-severe, severe, ?}
    \item{\code{seed-tmt}}{none, fungicide, other, ?}
    \item{\code{germination}}{90-100\%, 80-89\%, lt-80\% ,?}
    \item{\code{plant-growth}}{norm, abnorm, ?}
    \item{\code{leaves}}{norm, abnorm}
    \item{\code{leafspots-halo}}{absent, yellow-halos, no-yellow-halos, ?}
    \item{\code{leafspots-marg}}{w-s-marg, no-w-s-marg, dna, ?}
    \item{\code{leafspot-size}}{lt-1/8, gt-1/8, dna, ?}
    \item{\code{leaf-shread}}{absent, present, ?}
    \item{\code{leaf-malf}}{absent, present, ?}
    \item{\code{leaf-mild}}{absent, upper-surf, lower-surf, ?}
    \item{\code{stem}}{norm, abnorm, ?}
    \item{\code{lodging}}{yes, no, ?}
    \item{\code{stem-cankers}}{absent, below-soil, above-soil, above-sec-nde, ?}
    \item{\code{canker-lesion}}{dna, brown, dk-brown-blk, tan, ?}
    \item{\code{fruiting-bodies}}{absent, present, ?}
    \item{\code{external decay}}{absent, firm-and-dry, watery, ?}
    \item{\code{mycelium}}{absent, present, ?}
    \item{\code{int-discolor}}{none, brown, black, ?}
    \item{\code{sclerotia}}{absent, present, ?}
    \item{\code{fruit-pods}}{norm, diseased, few-present, dna, ?}
    \item{\code{fruit spots}}{absent, colored, brown-w/blk-specks, distort, dna, ?}
    \item{\code{seed}}{norm, abnorm, ?}
    \item{\code{mold-growth}}{absent, present, ?}
    \item{\code{seed-discolor}}{absent, present, ?}
    \item{\code{seed-size}}{norm, lt-norm, ?}
    \item{\code{shriveling}}{absent, present, ?}
    \item{\code{roots}}{norm, rotted, galls-cysts, ?}
}
}
\references{
Dua, D. and Graff, C. (2019). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\source{https://archive.ics.uci.edu/ml/datasets/Soybean+(Large)}

\examples{
data(soybeanlarge)}
\keyword{datasets}