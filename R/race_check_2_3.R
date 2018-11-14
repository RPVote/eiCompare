race_check_2_3 <- function(dat, split=c(3,4), catch=FALSE, catch_col=NULL, 
                           print_sides=TRUE){
  # data set with 1 no vote column, w candidate votes, then 4 demographics with last as other
  # e.g.: novote, V1, V2, VtdAVap_cor, VtdBVap_cor, VtdHVap_cor, VtdOVap_cor
  # will produce error if a problem with data
  
  # Check negative values in "No Vote/Other vote" and "Race/Other" variables #
  if(catch) {
    if (all(dat[,catch_col[1]] > 0)) {
      cat("All Catch variable 1 (Votes) values positive\n")
    } else {
      cat("Catch variable 1 (Votes) has negative values, must be zero or positive")
    }
    if (all(dat[,catch_col[2]] > 0)) {
      cat("All Catch variable 2 (Demographics) values positive\n")
    } else {
      cat("Catch variable 2 (Demographics) has negative values, must be zero or positive")
    }
  }
  # Check each Equation side is matching column totals by row #
  race_vote_split=split
  left_dat <- apply(dat[, 1:race_vote_split[1] ], 1, sum)
  right_dat <- apply(dat[, race_vote_split[2]:ncol(dat) ], 1, sum)
  
  if (print_sides){
    cat("Equation left hand side aggregate\n")
    print(left_dat)
    cat("Equation right hand side aggregate\n")
    print(right_dat)
  }
  # Check 
  if (all.equal(left_dat, right_dat) == TRUE){
    print("Both sides of equation equal, booya, proceed")
    return(dat = data.frame(dat, left_dat, right_dat, diff=left_dat - right_dat))
  } else {
    cat("Rows numbers that are unequal. Even out rounding.\n")
    print( which(left_dat != right_dat) )
    return(dat = data.frame(dat, left_dat, right_dat, diff=left_dat - right_dat))
  }
  
}