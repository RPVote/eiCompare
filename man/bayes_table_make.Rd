\name{bayes_table_make}
\alias{bayes_table_make}
\title{
EI:RxC Bayes Table Make
}
\description{
Creates data.frame() table from eiPack RxC output, in the same format as ei_est_gen
}
\usage{
bayes_table_make(ei_bayes_object, cand_vector, table_names)
}

\arguments{
  \item{ei_bayes_object}{
Output from eiPack ei.reg.bayes() function
}
  \item{cand_vector}{
Character vector of candidate name variables, usually "pct_johns" or something
}
  \item{table_names}{
Character vector of column names, e.g., c("RxC: Pct Hisp", "RxC: Pct Asian")
}
}
\value{
Data frame object in similar vein to ei_est_gen
}
\references{
O. Lau, R. T. Moore, and M. Kellermann. eipack: RxC ecological inference and higher-dimension data
management. New Functions for Multivariate Analysis, 18(1):43, 2006.
}
\author{
Loren Collingwood <loren.collingwood@ucr.edu>
}

\examples{
# TOY DATA EXAMPLE
canda <- runif(5)
candb <- 1-canda
white <- runif(5)
black <- 1 - white
total <- round( runif(5, min=20, max=40), 0)

toy <- data.frame(canda, candb, white, black, total)

cands <- c("canda", "candb")
table_names <- c("RxC: PCT Black", "RxC PCT White")

# Generate formula for passage to ei.reg.bayes() function
form <- formula(cbind(canda,candb) ~ cbind(black, white)) 
# Run Bayesian model
suppressWarnings (
ei_bayes <- ei.reg.bayes(form, data=toy, sample=100, truncate=TRUE)
)
# Table Creation, using function bayes_table_make
ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector= cands, table_names = table_names)
ei_bayes_res

\donttest{
# Load Package Data
data(corona)
# Create Character Vectors
cands <- c("pct_husted","pct_spiegel","pct_ruth","pct_button","pct_montanez","pct_fox")
table_names <- c("RxC: Pct Hisp", "RxC: Pct Asian", "RxC: Pct White")

# Generate formula for passage to ei.reg.bayes() function
form <- formula(cbind(pct_husted,pct_spiegel,pct_ruth,pct_button,pct_montanez,pct_fox) 
~ cbind(pct_hisp, pct_asian, pct_white)) 
# Run Bayesian model
suppressWarnings (
ei_bayes <- ei.reg.bayes(form, data=corona, sample=10000, truncate=TRUE)
)
# Table Creation, using function bayes_table_make
ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector= cands, table_names = table_names)
ei_bayes_res
}
}
