\name{summary.ei}
\alias{summary.ei}
\title{
Summarize Ecological Inference Estimates
}
\description{
`summary' method for the class `ei'.

}
\usage{
## S3 method for class 'ei'
\method{summary}{ei}(object, \ldots)
}

\arguments{
  \item{object}{
\code{object} An ei object from the function ei.
}
  \item{\dots}{
\code{\dots} A list of options to return in graphs. See values below.
}
}

\references{
Gary King (1997). A Solution to the Ecological Inference Problem. Princeton: Princeton University Press.
}
\author{
Gary King <<email: king@harvard.edu>> and Molly Roberts <<email: roberts8@fas.harvard.edu>>
}
\note{
Used within ei_est_gen() function
}

\examples{
# TOY DATA EXAMPLE
canda <- c(.1, .09, .85, .9, .92)
candb <- 1-canda
white <- c(.8, .9, .10, .08, .11)
black <- 1 - white
total <- c(30,80, 70, 20, 29)
toy <- data.frame(canda, candb, white, black, total)

# CREATE VECTORS
cands <- c("canda")
race_group <- c("~ black") # only use one group for example
table_names <- c("EI: PCT Black", "EI: PCT White")

# RUN ei_est_gen()
# KEEP DATA TO JUST ONE ROW FOR EXAMPLE (time) ONLY!
results <- ei_est_gen(cands, race_group, "total",
                      data = toy[c(1,3,5),], table_names = table_names, sample=100)
results

}
