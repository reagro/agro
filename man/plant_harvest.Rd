\name{plant_harvest}

\alias{plant_harvest}

\title{
Estimate planting and harvesting months from monthly rainfall
}

\description{
Estimate planting and harvesting months from monthly rainfall
}


\usage{
plant_harvest(x, min_prec=30, max_prec=90, max_len=5)
}
 
\arguments{
  \item{x}{numeric vector of length 12}
  \item{min_prec}{minimum precipitation threshold for a month to be considered viable for inclusion into a growing season}
  \item{max_prec}{maximum precipitation threshold (as above)}
  \item{max_len}{maximum length of the growing season to be considered}
}

\value{
Matrix with two columns (planting, harvesting) and 12 rows (months). 1 is TRUE, 0 is FALSE.
}

\author{
Robert J. Hijmans and Camila Bonilla
}

\examples{
rain <- c(0,1:10 * 10, 0)
rain
plant_harvest(rain)

rain <- c(0,rep(50,4), 0, 0, rep(50,4), 0)
rain
plant_harvest(rain)
}


