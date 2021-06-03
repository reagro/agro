\name{get_simple_URI}

\alias{get_simple_URI}

\title{
Clean an URI so that it can be used as a filename
}

\description{
Replace "/" and ":" in an URI with "_" so that they can be used as filenames
}


\usage{
get_simple_URI(x, reverse=FALSE)
}
 
\arguments{
  \item{x}{character, a URI}
  \item{reverse}{logical. If \code{TRUE}, the reverse operation is done}
}

\value{
character
}


\examples{
uri <- "doi:10.7910/DVN/UNLRGC"
u <- get_simple_URI(uri)
u
get_simple_URI(u, reverse=TRUE)
}


