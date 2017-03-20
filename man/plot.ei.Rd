\name{plot.ei}
\alias{plot.ei}
\title{
Plotting Ecological Inference Estimates
}
\description{
'plot' method for the class 'ei'.
}

\usage{
## S3 method for class 'ei'
\method{plot}{ei}(x, \ldots)
}

\arguments{
  \item{x}{
An ei object from the function ei.
}
  \item{\dots}{
A list of options to return in graphs. See values below.

}
}

\details{
Returns any of a set of possible graphical objects, mirroring those in 
the examples in King (1997). Graphical option lci is a logical value 
specifying the use of the Law of Conservation of Ink, where the implicit 
information in the data is represented through color gradients, i.e. the 
color of the line is a function of the length of the tomography line. 
This can be passed as an argument and is used for ``tomogD'' and ``tomog'' plots.
}

\value{
\item{tomogD}{Tomography plot with the data only. See Figure 5.1, page 81.}
\item{tomog}{Tomography plot with ML contours. See Figure 10.2, page 204.}
\item{tomogCI}{Tomography plot with 80\% confidence intervals. Confidence intervals appear on the screen in red with the remainder of the tomography line in yellow. The confidence interval portion is also printed thicker than the rest of the line. See Figure 9.5, page 179.}
\item{tomogCI95}{Tomography plot with 95\% confidence intervals. Confidence intervals appear on the screen in red with the remainder of the tomography line in yellow. The confidence interval portion is also printed thicker than the rest of the line. See Figure 9.5, page 179.}
\item{tomogE}{Tomography plot with estimated mean posterior B_ib and B_iw points.}
\item{tomogP}{Tomography plot with mean posterior contours.}
\item{betab}{Density estimate (i.e., a smooth version of a histogram) of point estimates of B_ib's with whiskers.}
\item{betaw}{Density estimate (i.e., a smooth version of a histogram) of point estimates of B_iw's with whiskers.}
\item{xt}{Basic X_i by T_i scatterplot.}
\item{xtc}{Basic X_i by T_i scatterplot with circles sized proportional to N_i.}
\item{xtfit}{X_i by T_i plot with estimated E(T_i|X_i) and conditional 80\% confidence intervals. See Figure 10.3, page 206.}
\item{xtfitg}{xtfit with Goodman's regression line superimposed.}
\item{estsims}{All the simulated B_ib's by all the simulated B_iw's. The simulations should take roughly the same shape of the mean posterior contours, except for those sampled from outlier tomography lines.}
\item{boundXb}{X_i by the bounds on B_ib (each precinct appears as one vertical line), see the lines in the left graph in Figure 13.2, page 238.}
\item{boundXw}{X_i by the bounds on B_iw (each precinct appears as one vertical line), see the lines in the right graph in Figure 13.2, page 238.}
\item{truth}{Compares truth to estimates at the district and precinct-level. Requires truth in the ei object. See Figures 10.4 (page 208) and 10.5 (page 210).}
\item{movieD}{For each observation, one tomography plot appears with the line for the particular observation darkened. After the graph for each observation appears, the user can choose to view the next observation (hit return), jump to a specific observation number (type in the number and hit return), or stop (hit ``s'' and return).}
\item{movie}{For each observation, one page of graphics appears with the posterior distribution of B_ib and B_iw and a plot of the simulated values of B_ib and B_iw from the tomography line. The user can choose to view the next observation (hit return), jump to a specific observation number (type in the number and hit return), or stop (hit ``s'' and return).}
}


\references{
Gary King (1997). A Solution to the Ecological Inference Problem. Princeton: Princeton University Press.
}
\author{
Gary King <<email: king@harvard.edu>> and Molly Roberts <<email: roberts8@fas.harvard.edu>>
}
\note{
Used within the ei_est_gen()
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
                      data = toy[c(1,3,5),], table_names = table_names, 
                      sample=100, tomog=TRUE)

# Delete Pdf files produced
getwd() # Show working directory
list.files() # show all files in directory
# To remove pdf files, make sure no other pdf files in directory
# system ("rm *.pdf ")
}

