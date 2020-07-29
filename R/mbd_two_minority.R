#' Multinomial Dirichlet Bayes Draw Two Candidates, and White/Minority voters
#'
#' Extract posterior means and credible intervals. Need to call variables V1,
#' V2. When two=FALSE, add in V3; Race names = VtdAVap_cor, VtdMVap_cor
#'
#'
#' @param md object from ei.MD.bayes() return
#' @param colnames Vector of candidate names. Stick to c(V1,V2) or c(V1,V2,V3)
#' @param two Logical. Two candidates (TRUE), or three (FALSE)
#' @return List with two data frames
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#'
#'
#' @export mbd_two_minority
mbd_two_minority <- function(md, colnames, two = TRUE) {
  # takes output from md_bayes_draw
  # Two candidates, variables labeled the same
  # V1, V2. When two=F, V3; 3 candidates
  # VtdAVap_cor, VtdMVap_cor
  if (two) {
    M.num.v1.est <- md[, "ccount.VtdMVap_cor.V1"]
    M.num.v2.est <- md[, "ccount.VtdMVap_cor.V2"]
    minority <- mean_and_ci(cbind(M.num.v1.est, M.num.v2.est))
    minority

    # White #
    W.num.v1.est <- md[, "ccount.VtdAVap_cor.V1"]
    W.num.v2.est <- md[, "ccount.VtdAVap_cor.V2"]
    white <- mean_and_ci(cbind(W.num.v1.est, W.num.v2.est))
    white

    # Adjust output to begin to match what we have
    # Rbind the mean/CI's together
    res <- rbind(minority, white)
    # Transpose; ordering needs to be the same
    minority2 <- t(t(c(res[1, ], res[2, ])))
    white2 <- t(t(c(res[3, ], res[4, ])))

    rxc <- cbind(minority2, white2)
    rxc <- round(rxc, 5) * 100 # Rounding
    row.names(rxc)[c(1, 4)] <- colnames
    suppressWarnings(rxc <- data.frame(row.names(rxc), rxc))
    colnames(rxc) <- c("Candidate", "RxC: Minority", "RxC: White")

    return(list(rowXcolumn = rxc, minority = minority, white = white))
  } else { # For 3-candidates, spaghetti

    # Minority Vote #
    M.num.v1.est <- md[, "ccount.VtdMVap_cor.V1"]
    M.num.v2.est <- md[, "ccount.VtdMVap_cor.V2"]
    M.num.v3.est <- md[, "ccount.VtdMVap_cor.V3"]
    minority <- mean_and_ci(cbind(M.num.v1.est, M.num.v2.est, M.num.v3.est))
    minority

    # White #
    W.num.v1.est <- md[, "ccount.VtdAVap_cor.V1"]
    W.num.v2.est <- md[, "ccount.VtdAVap_cor.V2"]
    W.num.v3.est <- md[, "ccount.VtdAVap_cor.V3"]
    white <- mean_and_ci(cbind(W.num.v1.est, W.num.v2.est, W.num.v3.est))
    white

    # Adjust output to begin to match what we have
    # Rbind the mean/CI's together
    res <- rbind(minority, white)

    # Transpose; ordering needs to be the same
    minority2 <- t(t(c(res[1, ], res[2, ], res[3, ])))
    white2 <- t(t(c(res[4, ], res[5, ], res[6, ])))

    rxc <- cbind(minority2, white2)
    rxc <- round(rxc, 5) * 100 # Rounding
    row.names(rxc)[c(1, 4, 7)] <- colnames
    suppressWarnings(rxc <- data.frame(row.names(rxc), rxc))
    colnames(rxc) <- c("Candidate", "RxC: Minority", "RxC: White")

    return(list(rowXcolumn = rxc, minority = minority, white = white))
  }
}
