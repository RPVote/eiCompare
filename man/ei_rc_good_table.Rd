\name{ei_rc_good_table}
\alias{ei_rc_good_table}

\title{
Create EI Comparison Table
}
\description{
Takes output from EI model, EI RxC model, Goodman regression, and puts them into a data frame table for useful analysis and comparison.
}
\usage{
ei_rc_good_table(ei, rc, good, groups, include_good = F)
}

\arguments{
  \item{ei}{
Table/data frame object result from ei_est_gen. This assumes beta_yes=F in ei_est_gen(). See example below for beta_yes=T in ei_est_gen().
}
  \item{rc}{
Table/data frame from EI:RxC process from bayes_table_make()
}
  \item{good}{
Table/data frame from Goodman regression, from goodman_generalize(). Default is nothing
}
  \item{groups}{
Character vector of voting blocks (e.g., c("Latino", "White"))
}
  \item{include_good}{
Logical, default is FALSE (F), Set to TRUE (T) if including a Goodman table/data object
}

}
\value{
Object of class ei_compare containing a 1. data.frame() slot of comparisons across the three models; 2. Character vector of group names used for later plotting 
}
\references{
eiPack, King et. al. (http://gking.harvard.edu/eiR)
}
\author{
Loren Collingwood <loren.collingwood@ucr.edu>
}
\note{
Most of the time the user will not include the Goodman table, as they are interested in the EI vs. EI:RxC comparison
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

# Generate formula for passage to ei.reg.bayes() function
form <- formula(cbind(canda,candb) ~ cbind(black, white)) 
# Run Bayesian model
suppressWarnings (
  ei_bayes <- ei.reg.bayes(form, data=toy, sample=100, truncate=TRUE)
)

table_names <- c("RxC: PCT Black", "RxC: PCT White")
cands <- c("canda", "candb")
ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector= cands, table_names = table_names)
ei_bayes_res <- ei_bayes_res[c(1,2,5),]
# Combine Results, results in object of class ei_compare 
ei_rc_combine <- ei_rc_good_table(results, ei_bayes_res, 
                                  groups= c("Black", "White")
)
# Produces data and character vector, which can be sent to plot()
ei_rc_combine


\donttest{
# Warning: Takes a while to run
# Load corona data
data(corona)
# Generate character vectors
cands <- c("pct_husted","pct_spiegel","pct_ruth","pct_button","pct_montanez","pct_fox")
race_group3 <- c("~ pct_hisp", "~ pct_asian", "~ pct_white")
table_names <- c("EI: Pct Lat", "EI: Pct Asian", "EI: Pct White")
# Run EI iterative Fitting
results <- ei_est_gen(cand_vector=cands, race_group = race_group3,
			total = "totvote", data = corona, table_names = table_names)

# EI: RxC model 
# Generate formula
form <- formula(cbind(pct_husted,pct_spiegel,pct_ruth,pct_button,pct_montanez,pct_fox) 
~ cbind(pct_hisp, pct_asian, pct_white)) 
suppressWarnings (
ei_bayes <- ei.reg.bayes(form, data=corona, sample=10000, truncate=TRUE)
)
# RxC table names
table_names <- c("RxC: Pct Hisp", "RxC: Pct Asian", "RxC: Pct White")
# Table Creation, using function bayes_table_make in ei_est_generalize.R file
ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector= cands, table_names = table_names)


# Combine Results, results in object of class ei_compare 
ei_rc_combine <- ei_rc_good_table(results, ei_bayes_res, 
				groups= c("Latino", "Asian", "White")
				)
ei_rc_combine

# If set beta_yes=T in ei_est_gen():
#ei_rc_combine2 <- ei_rc_good_table(results[[1]], ei_bayes_res, groups= c("Black", "White"))

}
}