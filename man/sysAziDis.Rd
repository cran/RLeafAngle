% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sysAziDis.R
\name{sysAziDis}
\alias{sysAziDis}
\title{Compute the symmetric distribution of leaf azimuth angle.}
\usage{
sysAziDis()
}
\value{
The symmetric distribution of leaf azimuth angle.
}
\description{
Compute the symmetric distribution of leaf azimuth angle.
}
\examples{
aDis<-sysAziDis()
plot(c(10,30,50,70,90,110,130,150,170,190,210,230,250,270,290,310,330,350), aDis, 
xlab=expression(Leaf~azimuth~angle~~(""^"o")), ylab="Leaf area freqency")
}
\references{
Wang, W. M., Li, Z. L., & Su, H. B. (2007). 
Comparison of leaf angle distribution functions: effects on extinction coefficient and 
fraction of sunlit foliage. Agricultural and Forest Meteorology, 143(1), 106-122.
}
\author{
Wei-Min Wang (wmwang AT gmail.com)
}
