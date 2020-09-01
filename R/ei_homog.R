#' Homogeneous Precinct Analysis
#'
#' Creates matrix table of homogeneous precinct analysis results by racial/ethnic group. The idea, 
#' for example, is to get a basic sense of voting behavior by racial group, examine candidate 
#' preference in districts that are above 80% white, 80% black, etc.
#' 
#' ei_homog
#'
#' @param data A data.frame() object containing precinct-level turnout data by race and candidate
#' @param cand_cols A character vector listing the column names for turnout for each candidate
#' @param race_cols A character vector listing the column names for turnout by race
#' @param totals_col The name of the column containing total votes cast in each precinct
#' @param cp numeric; homogeneous precinct cut-point, e.g., 0.80; default = 0.80
#' @warn_row = numeric; threshold number of precincts racial group must be above to 
#' conduct analysis; default = 5. For example, with three groups, whites, blacks, Hispanics, 
#' each group must have at least 5 precincts with at least 80% share of the population for 
#' that group. All racial groups need to have at least n number of precincts at or above 
#' warn_row level or error will be thrown.
#' @return matrix with homogeneous precinct results, columns = race groups, rows = candidates
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
#' # Default Example #
#'ei_homog(data = toy,
#'         race_cols = c("white", "black"),
#'         cand_cols = c("cand_a", "cand_b"),
#'         totals_col = "total")
#'            
#' # Verbosity Example #
#'ei_homog(data = toy,
#'         race_cols = c("white", "black"),
#'         cand_cols = c("cand_a", "cand_b"),
#'         totals_col = "total", 
#'         verbose = TRUE)
#'            
#' # Adjust Cut Point (cp) to 0.70
#'ei_homog(data = toy,
#'         race_cols = c("white", "black"),
#'         cand_cols = c("cand_a", "cand_b"),
#'         totals_col = "total", 
#'         cp = 0.70,
#'         verbose = TRUE)
#'            
#' # Set Precincts to anything above 3
#'ei_homog(data = toy,
#'         race_cols = c("white", "black"),
#'         cand_cols = c("cand_a", "cand_b"),
#'         totals_col = "total", 
#'         warn_row = 3,
#'         verbose = TRUE)
#'
#' @export ei_homog


ei_homog <- function (data, cand_cols, race_cols, totals_col, cp = 0.8, warn_row = 5, 
                      verbose = FALSE) 
{
    mat_hold <- matrix(NA, nrow = length(race_cols), ncol = length(cand_cols))
    for (j in seq_len(length(race_cols))) {
        # j <- 1
        homog <- data[data[, race_cols[j]] >= cp, ]
        if (verbose) {
            message(paste("Number of observations for racial group: ", 
                          race_cols[j], sep = ""))
            print(nrow(homog))
        }
        stopifnot(`Number of rows for racial group lower than threshold. Change warn_row threshold and/or check data to see if homogeneous precinct analysis even possible for all race groups.` = nrow(homog) >= 
                      warn_row)
        for (i in seq_len(length(cand_cols))) {
            mat_hold[j, i] <- stats::weighted.mean(x = homog[, cand_cols[i]], w = homog[, totals_col])
        }
    }
    rownames(mat_hold) <- race_cols
    colnames(mat_hold) <- cand_cols
    mat_hold <- t(mat_hold)
    
    return(mat_hold)
    
}

