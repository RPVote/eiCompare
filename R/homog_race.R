# Function: homog_race #
# Loren Collingwood, alpha version for eiCompare v2.5
# loren.collingwood@ucr.edu; loren.collingwood@gmail.com
# with input from Stephen Popick, stephenpopick@gmail.com 

# Goal: Conducts homogeneous analysis for multiple racial groups and candidates #
# Function Params #
# @ x = dataset; includes candidate percents; race; total vote by precinct
# @ r = character vector for race column names
# @ cands = character vector for candidate column names
# @ cp = numeric; homogeneous threshold
# @ warn_row = threshold number of precincts necessary to conduct analysis; 
#   stops if not greater than this threshold for any of the racial groups

homog_race <- function(x, r, cands, total, cp = 0.80, warn_row = 10) {
    
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

# Example from SMR data #

#homog_race(x = lac_10,
#           r = c("pct_latino", "pct_other"),
#           cands = c("pct_delatorre", "pct_jones"),
#           total = "votescast",
#           cp = .90)

# When at least one of the groups pct_X at .80 or higher has fewer than 10 rows
# you'll get thrown an error and then lower the threshold.

#homog_race(x = oc_12,
#           r = c("pct_latino", "pct_asian", "pct_white"),
#           cands = cands,
#           total = "tot_vote",
#           cp = .70)
