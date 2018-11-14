\name{mbd_two}
\alias{mbd_two}

\title{Multinomial Dirichlet Bayes Draw Two Candidates
  
}
\description{Extract posterior means and credible intervals. Need to label candidate vote variables: V1, V2, when two=F, add V3; Hispanic = VtdHVap_cor, White = VtdAVap_cor, Black = VtdBVap_cor
  
}
\usage{mbd_two(md, colnames, two=TRUE)
  
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
