#' Multinomial Dirichlet Bayes Draw Two Candidates
#'
#' Extract posterior means and credible intervals. Need to label candidate vote
#' variables: V1, V2, when two=FALSE, add V3; Hispanic = VtdHVap_cor, White =
#' VtdAVap_cor, Black = VtdBVap_cor
#'
#'
#' @param md object from ei.MD.bayes() return
#' @param colnames Vector of candidate names. Stick to c(V1,V2) or c(V1,V2,V3)
#' @param two Logical. Two candidates (TRUE), or three (FALSE)
#' @return List with two data frames
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#'
#' @export mbd_two
mbd_two <- function(md, colnames, two = TRUE) {
  # takes output from md_bayes_draw
  # Two candidates, variables labeled the same
  # V1, V2, , when two=F, V3; 3 candidates
  # VtdHVap_cor, VtdAVap_cor, VtdBVap_cor
  if (two) {
    # Hispanic
    H.num.v1.est <- md[, "ccount.VtdHVap_cor.V1"]
    H.num.v2.est <- md[, "ccount.VtdHVap_cor.V2"]
    hispanic <- mean_and_ci(cbind(H.num.v1.est, H.num.v2.est))

    # Black Vote #
    B.num.v1.est <- md[, "ccount.VtdBVap_cor.V1"]
    B.num.v2.est <- md[, "ccount.VtdBVap_cor.V2"]
    black <- mean_and_ci(cbind(B.num.v1.est, B.num.v2.est))
    black

    # White #
    W.num.v1.est <- md[, "ccount.VtdAVap_cor.V1"]
    W.num.v2.est <- md[, "ccount.VtdAVap_cor.V2"]
    white <- mean_and_ci(cbind(W.num.v1.est, W.num.v2.est))
    white

    # Adjust output to begin to match what we have
    # Rbind the mean/CI's together
    res <- rbind(hispanic, black, white)
    # Transpose; ordering needs to be the same
    hisp2 <- t(t(c(res[1, ], res[2, ])))
    black2 <- t(t(c(res[3, ], res[4, ])))
    white2 <- t(t(c(res[5, ], res[6, ])))

    rxc <- cbind(hisp2, black2, white2)
    rxc <- round(rxc, 5) * 100 # Rounding
    row.names(rxc)[c(1, 4)] <- colnames
    suppressWarnings(rxc <- data.frame(row.names(rxc), rxc))
    colnames(rxc) <- c("Candidate", "RxC: Hispanic", "RxC: Black", "RxC White")

    return(list(rowXcolumn = rxc, hispanic = hispanic, black = black, white = white))
  } else { # For 3-candidates, spaghetti
    # Hispanic
    H.num.v1.est <- md[, "ccount.VtdHVap_cor.V1"]
    H.num.v2.est <- md[, "ccount.VtdHVap_cor.V2"]
    H.num.v3.est <- md[, "ccount.VtdHVap_cor.V3"]
    hispanic <- mean_and_ci(cbind(H.num.v1.est, H.num.v2.est, H.num.v3.est))

    # Black Vote #
    B.num.v1.est <- md[, "ccount.VtdBVap_cor.V1"]
    B.num.v2.est <- md[, "ccount.VtdBVap_cor.V2"]
    B.num.v3.est <- md[, "ccount.VtdBVap_cor.V3"]
    black <- mean_and_ci(cbind(B.num.v1.est, B.num.v2.est, B.num.v3.est))
    black

    # White #
    W.num.v1.est <- md[, "ccount.VtdAVap_cor.V1"]
    W.num.v2.est <- md[, "ccount.VtdAVap_cor.V2"]
    W.num.v3.est <- md[, "ccount.VtdAVap_cor.V3"]
    white <- mean_and_ci(cbind(W.num.v1.est, W.num.v2.est, W.num.v3.est))
    white

    # Adjust output to begin to match what we have
    # Rbind the mean/CI's together
    res <- rbind(hispanic, black, white)

    # Transpose; ordering needs to be the same
    hisp2 <- t(t(c(res[1, ], res[2, ], res[3, ])))
    black2 <- t(t(c(res[4, ], res[5, ], res[6, ])))
    white2 <- t(t(c(res[7, ], res[8, ], res[9, ])))

    rxc <- cbind(hisp2, black2, white2)
    rxc <- round(rxc, 5) * 100 # Rounding
    row.names(rxc)[c(1, 4, 7)] <- colnames
    suppressWarnings(rxc <- data.frame(row.names(rxc), rxc))
    colnames(rxc) <- c("Candidate", "RxC: Hispanic", "RxC: Black", "RxC White")

    return(list(rowXcolumn = rxc, hispanic = hispanic, black = black, white = white))
  }
}
