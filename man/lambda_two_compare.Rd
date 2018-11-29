\name{lambda_two_compare}
\alias{lambda_two_compare}

\title{
  Lambda Two Compare
}
\description{
  Compares two vectors of lambdas, usually one racial group's support for two separate candidates, or two separate groups' support for the same candidate.
}
\usage{
  lambda_two_compare(lmd, cnames, group_name="Latino", cand1or2=1)
}

\arguments{
  \item{lmd}{data.frame() object returned from md_bayes_draw_lambda()
  }
  \item{cnames}{Vector of character (column) names, needs to match relevant column names in md_bayes_draw_lambda return.
  }
  \item{group_name}{Character string for name appearing in posterio plot. Default is "Latino")
  }
  \item{cand1or2}{Numeric. Either 1 or 2. Default = 1. Which pairing over the other.}
}

\value{
  Data frame of probably one scenario over the other by 10 pct., by 5 pct., greater than 0
}
\references{
  eiPack, King et. al. (http://gking.harvard.edu/eiR)
}
\author{
  Loren Collingwood <loren.collingwood@ucr.edu>; Justin Gross <jhgross@umass.edu>
}

\examples{
  
  # TOY DATA EXAMPLE
  canda <- c(10,8, 10, 4, 8)
  candb <- 20-canda
  white <- c(15, 12, 18, 6, 10)
  black <- 20 - white
  toy <- data.frame(canda, candb, white, black)
  
  # Generate formula for passage to ei.reg.bayes() function
  form <- formula(cbind(canda,candb) ~ cbind(black, white)) 
  # Then excute md_bayes_draw(); not run here due to time
  # lmd <- md_bayes_draw_lambda(toy, c(2,3), form )
  # Function Prep #
  # cnames <- c("lambda.black.canda", "lambda.black.candb")

  # Canda a over candb among black voters#
  #lambda_two_compare(lmd, cnames=cnames, cand1or2 = 1)
  
}
