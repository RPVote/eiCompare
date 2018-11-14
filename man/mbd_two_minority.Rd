\name{mbd_two_minority}
\alias{mbd_two_minority}

\title{Multinomial Dirichlet Bayes Draw Two Candidates, and White/Minority voters
  
}
\description{Extract posterior means and credible intervals. Need to call variables V1, V2. When two=F, add in V3; Race names = VtdAVap_cor, VtdMVap_cor
}
\usage{mbd_two_minority(md, colnames, two=TRUE)
  
}

\arguments{
  \item{md}{object from ei.MD.bayes() return}
  \item{colnames}{Vector of candidate names. Stick to c(V1,V2) or c(V1,V2,V3)}
  \item{two}{Logical. Two candidates (TRUE), or three (FALSE)}
}

\value{List with two data frames
  
}
\references{
  
}
\author{
  Loren Collingwood <loren.collingwood@ucr.edu>
}

\examples{
  
  # EXAMPLE: NOT RUN #
  
}
