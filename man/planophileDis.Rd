% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/planophileDis.R
\name{planophileDis}
\alias{planophileDis}
\title{Compute the planophile distribution of leaf zenith angle.}
\usage{
planophileDis()
}
\value{
The planophile distribution of leaf zenith angle.
}
\description{
Compute the planophile distribution of leaf zenith angle.
}
\examples{
sDis<-planophileDis()
plot(c(4.5, 13.5, 22.5, 31.5, 40.5, 49.5, 58.5, 67.5, 76.5, 85.5), sDis, 
xlab=expression(Leaf~zenith~angle~~(""^"o")), ylab="Leaf area freqency")

}
\references{
Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
Comparison of leaf angle distribution functions: effects on extinction coefficient and 
fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
}
\author{
Wei-Min Wang (wmwang AT gmail.com)
}
