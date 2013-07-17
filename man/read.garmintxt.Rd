\name{read.garmintxt}
\alias{read.garmintxt}
\title{Read text data files produced by Garmin GPS units}
\usage{
  read.garmintxt(file, encoding = "latin1")
}
\arguments{
  \item{file}{file name}

  \item{encoding}{file encoding (usually "latin1" but if
  you're having probelms, try changing this.)}
}
\description{
  This function reads data pulled from Garmin GPS units
  using the \code{gpsbabel} program.
}
\examples{
library(readGPS)

# args
fn <- "akutco.txt"

dat <- read.garmintxt(fn)
}
\author{
  David L. Miller
}

