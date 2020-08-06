#' Homogeneous Precinct Analysis
#'
#' Creates matrix table of homogeneous precinct analysis results
#' 
#' homog_race
#'
#' @param x precinct-level dataset
#' @param r character vector for race column names
#' @param cands character vector for candidate column names
#' @param total "character vector indicating precinct total column name
#' @param cp numeric; homogeneous precinct cut-point, e.g., 0.80; default = 0.80
#' @warn_row = threshold number of precincts necessary to conduct analysis; default = 5
#' All racial groups need to have at least n number of precincts at or above warn_row level
#' or error will be thrown.
#' @return matrix with results
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>; <loren.collingwood@@gmail.com>
#' @author Stephen Popick
#' @examples
#' # Toy data example
#' cand_a <- c( rep(.8, 10), rep(.2, 10))
#' cand_b <- 1 - cand_a
#' white <- c(rep(.7, 5), rep(.85, 5), rep(.1, 5), rep(.05, 5))
#' black <- 1 - white
#' total <- c ( rep(200, 5), rep(100, 5), rep(80, 5), rep(300, 5) )
#' toy <- data.frame(cand_a, cand_b, white, black, total)
#'
#' # Create vectors for iterative EI function
#' cands <- c("cand_a", "cand_b)
#' 
#' homog_race(x = toy,
#'            r = c("white", "black"),
#'            cands = c("cand_a", "cand_b"),
#'            total = "total")
#'
#' @export homog_race


homog_race <- function(x, r, cands, total, cp = 0.80, warn_row = 5) {
    
    # Set up Return Matrix #
    
    # Number columns (race goes into columns)
    nc <- length(r) 
    
    # Number rows (candidate goes along rows)
    nr <- length(cands)
    
    mat_hold <- matrix(NA, nrow = nr, ncol = nc)
    
    # Loop over columns (race) #
    for (j in 1:nc) {
        
        homog <- x[x[,r[j]] >= cp, ]
        cat(paste("Number of observations for racial group: ", r[j], sep=""))
        print(nrow(homog))
        cat("\n")
        
        stopifnot("Number of rows for racial group lower than threshold" = nrow(homog) >= warn_row)
        
        # Loop over rows (candidates) #
        for (i in 1:nr) {
            
            mat_hold[i,j] <- stats::weighted.mean(x = homog[, cands[i]], 
                                                  w = homog[,total])
        }
    }
    # Label Rows and Columns #
    rownames(mat_hold) <- cands
    colnames(mat_hold) <- r
    
    # Return Matrix #
    return(mat_hold)
    
}
