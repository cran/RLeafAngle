% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interBeta.R
\name{interBeta}
\alias{interBeta}
\title{Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of Beta function.}
\usage{
interBeta(AngleLower, AngleUpper, meu, neu)
}
\arguments{
\item{AngleLower}{The lower limit of leaf angle intervals in degree.}

\item{AngleUpper}{The upper limit of leaf angle intervals in degree.}

\item{meu}{One of the parameters for Beta function.}

\item{neu}{One of the parameters for Beta function.}
}
\value{
Compute the fraction leaf area of Beta function.
}
\description{
Compute the fraction leaf area given leaf angle intervals (in degree) and the parameters of Beta function.
}
\examples{
interBeta(40,50,1.616,2.188)
}
\references{
Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
Comparison of leaf angle distribution functions: effects on extinction coefficient and 
fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
}
\author{
Wei-Min Wang (wmwang AT gmail.com)
}
