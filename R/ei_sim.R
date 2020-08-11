#' Importance sampling from iterative EI
#'
#' This is a modified version of the ei.sim function in the ei package. This
#' function is modified to enable users to enter their desired sample size,
#' instead of having the sample size be fixed at 99.
#'
#' @param ei.object The output of ei::ei() where simulate is set to FALSE.
#' @param samples The number of samples to use.
#'
#' @return an ei object from the ei package.
ei_sim <- function(ei.object, samples) {
  samples <- samples + 1
  hessian <- ei.object$hessianC
  erho <- ei.object$erho
  esigma <- ei.object$esigma
  ebeta <- ei.object$ebeta
  ealphab <- ei.object$ealphab
  ealphaw <- ei.object$ealphaw
  numb <- ei.object$numb
  covs <- ei.object$covs
  Rfun <- ei.object$Rfun
  x <- ei.object$x
  t <- ei.object$t
  n <- ei.object$n
  Zb <- ei.object$Zb
  Zw <- ei.object$Zw
  truth <- ei.object$truth
  id <- ei.object$id
  precision <- ei.object$precision
  # Begin Importance Sampling
  message("Importance Sampling..")
  keep <- matrix(data = NA, ncol = (length(ei.object$phi)))
  resamp <- 0
  while (dim(keep)[1] < samples) {
    keep <- .samp(t, x, n, Zb, Zw, ei.object$phi, hessian, samples, keep,
      numb = numb, covs, erho, esigma,
      ebeta, ealphab, ealphaw, Rfun
    )
    resamp <- resamp + 1
  }

  # Extract values from importance sampling
  keep <- keep[2:samples, ]
  mu <- keep[, 1:2]
  sd <- keep[, 3:4]
  rho <- keep[, 5]
  Bb0v <- keep[, 6:(5 + numb)]
  Bw0v <- keep[, (6 + numb):length(ei.object$phi)]
  sd[, 1] <- exp(sd[, 1])
  sd[, 2] <- exp(sd[, 2])

  # Reparamterize
  Zb <- as.matrix(Zb)
  Zw <- as.matrix(Zw)
  Bb0v <- as.matrix(Bb0v)
  Bw0v <- as.matrix(Bw0v)
  mu1 <- mu[, 1] * (.25 + sd[, 1]^2) + .5 + t(as.matrix(apply(
    Zb, 2,
    function(x) x - mean(x)
  )) %*% t(Bb0v))
  mu2 <- mu[, 2] * (.25 + sd[, 2]^2) + .5 + t(as.matrix(apply(
    Zw, 2,
    function(x) x - mean(x)
  )) %*% t(Bw0v))

  # phin <- dmvnorm(psi, par, log=T)
  rho <- (exp(2 * rho) - 1) / (exp(2 * rho) + 1)
  psi <- cbind(mu1, mu2, sd, rho)
  bb <- psi[, 1:length(x)]
  bw <- psi[, (length(x) + 1):(length(x) * 2)]
  sb <- psi[, (length(x) * 2 + 1)]
  sw <- psi[, (length(x) * 2 + 2)]
  rho <- psi[, (length(x) * 2 + 3)]
  omx <- 1 - x
  sbw <- rho * sb * sw
  betab <- matrix(nrow = length(x), ncol = dim(keep)[1])
  betaw <- matrix(nrow = length(x), ncol = dim(keep)[1])
  homoindx <- ifelse(x == 0, 1, 0)
  homoindx <- ifelse(x == 1, 2, homoindx)
  enumtol <- .0001
  cT0 <- t < enumtol & homoindx == 0
  cT1 <- t > (1 - enumtol) & homoindx == 0
  ok <- ifelse(homoindx == 0 & cT0 == 0 & cT1 == 0, T, F)
  wh <- homoindx == 1
  bl <- homoindx == 2
  for (i in 1:dim(keep)[1]) {
    sig2 <- sb[i]^2 * x^2 + sw[i]^2 * omx^2 + sbw[i] * 2 * x * omx
    omega <- sb[i]^2 * x + sbw[i] * omx
    eps <- t - (bb[i, ]) * x - (bw[i, ]) * omx
    mbb <- bb[i, ] + omega / sig2 * eps
    vbb <- sb[i]^2 - (omega^2) / sig2
    vbb <- ifelse(vbb < 1 * 10^-32, .0001, vbb)
    s <- ifelse(vbb >= 0 & vbb != Inf & !is.na(vbb), sqrt(vbb), NaN)
    bounds <- bounds1(x, t, n)
    out <- NULL
    for (j in 1:length(x[ok])) {
      out[ok][j] <- msm::rtnorm(1,
        mean = mbb[ok][j], sd = s[ok][j],
        lower = bounds[ok, ][j, 1],
        upper = bounds[ok, ][j, 2]
      )
    }
    out[wh] <- NA
    out[bl] <- t[bl]
    out[cT1] <- bounds[cT1, 1]
    out[cT0] <- bounds[cT0, 1]
    betab[, i] <- out
  }
  omx <- 1 - x
  for (j in 1:length(x[ok])) {
    betabs <- betab[ok, ][j, ]
    betaw[ok, ][j, ] <- t[ok][j] / omx[ok][j] - betabs * x[ok][j] / omx[ok][j]
  }

  if (sum(wh) > 0) {
    betaw[wh, ] <- as.matrix(rep(1, dim(keep)[1])) %*% t(as.matrix(t[wh]))
  }

  if (sum(bl) > 0) {
    betaw[bl, ] <- NA
    # betaw[bl,] <- as.matrix(rep(1,dim(keep)[1]))%*%t(as.matrix(t[bl]))
  }
  if (sum(cT1) > 0) {
    betaw[cT1, ] <-
      as.matrix(rep(1, dim(keep)[1])) %*% t(as.matrix(bounds[cT1, 3]))
  }
  if (sum(cT0) > 0) {
    betaw[cT0, ] <-
      as.matrix(rep(1, dim(keep)[1])) %*% t(as.matrix(bounds[cT0, 3]))
  }

  mbetab <- apply(betab, 1, mean)
  mbetaw <- apply(betaw, 1, mean)
  sdbetab <- apply(betab, 1, sd)
  sdbetaw <- apply(betaw, 1, sd)
  output <- list(
    ei.object$phi, ei.object$hessian, hessian, psi, mbetab, mbetaw,
    sdbetab, sdbetaw, betab, betaw, resamp, erho, esigma,
    ebeta, ealphab, ealphaw, numb, x, t, n, Zb, Zw,
    truth,
    precision, id
  )

  names(output) <- c(
    "phi", "hessian", "hessianC", "psi", "betab", "betaw",
    "sbetab",
    "sbetaw", "betabs", "betaws", "resamp", "erho",
    "esigma", "ebeta", "ealphab", "ealphaw", "numb",
    "x", "t", "n", "Zb", "Zw",
    "truth", "precision", "id"
  )
  class(output) <- "ei"
  return(output)
}


#' Implement importance sampling
#'
#' This function is pulled directly from the ei package to enable a modified
#' version of ei.sim()
#'
#' @importFrom mvtnorm dmvnorm rmvnorm
.samp <- function(t, x, n, Zb, Zw, par, varcv, nsims, keep, numb, covs,
                  erho, esigma, ebeta, ealphab, ealphaw, Rfun) {
  import1 <- NULL

  varcv2 <- solve(varcv) / 4

  draw <- mvtnorm::rmvnorm(nsims, par[covs], varcv2)
  varcv3 <- solve(varcv2)
  phiv <- mvtnorm::dmvnorm(draw, par[covs], varcv2, log = T)
  zbmiss <- ifelse(covs[6] == FALSE, TRUE, FALSE)
  zwmiss <- ifelse(covs[(6 + numb)] == FALSE, TRUE, FALSE)
  if (zbmiss == TRUE & zwmiss == FALSE) {
    draw <- cbind(draw[, 1:5], rep(1, nsims), draw[, (5 + numb):sum(covs)])
  }
  if (zbmiss == FALSE & zwmiss == TRUE) {
    draw <- cbind(draw, rep(1, nsims))
  }
  if (zbmiss == TRUE & zwmiss == TRUE) {
    draw <- cbind(draw, rep(1, nsims), rep(1, nsims))
  }
  # for(i in 1:nsims){
  # import1[i] <- -like(as.vector(draw[i,]),
  # t, x, n, Zb, Zw, numb=numb, erho, esigma, ebeta,
  # ealphab, ealphaw, Rfun) - phiv[i]
  # }

  # Calculates importance ratio
  import1 <- apply(as.matrix(1:nsims), 1, function(i) {
    -like(as.vector(draw[i, ]), t, x, n, Zb, Zw,
      numb = numb, erho, esigma, ebeta, ealphab, ealphaw, Rfun
    )
    - phiv[i]
  })

  ok <- !is.nan(import1)
  lnir <- import1 - max(import1[ok])
  ir <- NA
  ir[ok] <- exp(lnir[ok])
  # print(mean(is.finite(ir)))
  tst <- ifelse(is.finite(ir), ir > runif(1, 0, 1), FALSE)
  # print(sum(tst))
  keep <- rbind(keep, draw[tst, ])
  return(keep)
}

#' Likelihood function for ecological inference.
#'
#' This function is pulled directly from the ei package to enable a modified
#' version of ei.sim()
#'
like <- function(param, y, x, n, Zb, Zw, numb, erho, esigma, ebeta,
                 ealphab, ealphaw, Rfun) {

  # Transform parameters
  Bb0 <- param[1]
  Bw0 <- param[2]
  sb0 <- param[3]
  sw0 <- param[4]
  rho0 <- param[5]
  Bb0v <- param[6:(5 + numb)]
  Bw0v <- param[(numb + 6):length(param)]
  sb <- exp(sb0)
  sw <- exp(sw0)
  Zb <- as.matrix(Zb)
  Zw <- as.matrix(Zw)
  bb <- Bb0 * (.25 + sb^2) + .5 +
    as.matrix(apply(Zb, 2, function(x) x - mean(x))) %*% as.matrix(Bb0v)
  bw <- Bw0 * (.25 + sw^2) + .5 +
    as.matrix(apply(Zw, 2, function(x) x - mean(x))) %*% as.matrix(Bw0v)
  rho <- (exp(2 * rho0) - 1) / (exp(2 * rho0) + 1)
  sigb2 <- sb^2
  sigw2 <- sw^2
  sigbw <- rho * sb * sw

  # print(c(mean(Bb0),mean(Bw0),sb0,sw0,rho0,mean(Bb0v),mean(Bw0v)))
  # print(c(mean(bb),mean(bw),sb,sw,rho))

  # Create Demographic Categories
  homoindx <- ifelse(x == 0, 1, 0)
  homoindx <- ifelse(x == 1, 2, homoindx)
  enumtol <- .0001
  cT0 <- y < enumtol & homoindx == 0
  cT1 <- y > (1 - enumtol) & homoindx == 0
  ok <- ifelse(homoindx == 0 & cT0 == 0 & cT1 == 0, T, F)

  # Compute likelihood for different categories

  # 0<T<1, 0<X<1
  omx <- 1 - x
  mu <- bb * x + bw * omx
  epsilon <- y - mu
  s2 <- sigb2 * (x^2) + sigw2 * (omx^2) + 2 * sigbw * x * omx
  omega <- sigb2 * x + sigbw * omx
  ebb <- bb + (omega / s2) * epsilon
  vbb <- sigb2 - (omega^2) / s2
  vbb <- ifelse(vbb < 1 * 10^-32, .0001, vbb)
  bounds <- bounds1(x, y, n)
  s <- ifelse(vbb >= 0 & vbb != Inf & !is.na(vbb), sqrt(vbb), NaN)
  res <- NULL
  b.s <- (bounds[ok, ][, 2] - ebb[ok]) / s[ok]
  as <- (bounds[ok, ][, 1] - ebb[ok]) / s[ok]
  res[ok] <- log(pnorm(as, lower.tail = F) - pnorm(b.s, lower.tail = F))
  # res[ok] <- ifelse(abs(res[ok])==Inf, log(1*10^-15),res[ok])
  # res[ok] <- ifelse(abs(res[ok])==Inf, NaN,res[ok])
  # res[ok] <- log(pnorm(bounds[ok,2], mean=ebb[ok], sd=s[ok]) -
  ## pnorm(bounds[ok,1],#mean=ebb[ok], sd=s[ok]))s
  # res[ok] <- ifelse(res[ok]==NA,-999,res[ok])
  # print(summary(res))
  R <- NULL
  bs <- as.matrix(cbind(bb, bw))
  R[ok] <- .createR(ok, Rfun, bb, bw, sb, sw, rho, x)

  # print(summary(R))

  # lliki  <- -.5*(log(s2[ok])+epsilon[ok]^2/s2[ok]) + res[ok] - R[ok]
  llik.het <- -.5 * sum((log(s2[ok]) + (epsilon[ok]^2) / (s2[ok])))
  llik.het <- llik.het + sum(res[ok]) - sum(R[ok])

  # Homogenous precincts

  # X=0
  wh <- homoindx == 1
  llik.wh <- 0
  if (sum(wh) > 0) {
    epsilon <- y[wh] - bw[wh]
    llik.whi <- -.5 * (log(sigw2) + (epsilon^2) / (sigw2))
    llik.wh <- -.5 * sum((log(sigw2) + (epsilon^2) / (sigw2)))
    bnds <- cbind(rep(0, sum(wh)), rep(1, sum(wh)))
    Ebb <- bb[wh] + rho * (sb / sw) * epsilon
    vbb <- sigb2 * (1 - rho^2)
    vbb <- ifelse(vbb < 1 * 10^-32, .0001, vbb)
    s <- ifelse(vbb >= 0 & vbb != Inf & !is.na(vbb), sqrt(vbb), NaN)
    b.s <- (bnds[, 2] - Ebb) / s
    as <- (bnds[, 1] - Ebb) / s
    res <- log(pnorm(as, lower.tail = F) - pnorm(b.s, lower.tail = F))
    # res <- log(pnorm(bnds[,2], mean=Ebb, sd=s) - pnorm(bnds[,1],
    # mean=Ebb, sd=s))
    R[wh] <- .createR(wh, Rfun, bb, bw, sb, sw, rho, x)
    llik.wh <- llik.wh + sum(res) - sum(R[wh])
  }

  # X=1
  bl <- homoindx == 2
  llik.bl <- 0
  if (sum(bl) > 0) {
    epsilon <- y[bl] - bb[bl]
    llik.bl <- -.5 * sum((log(sigb2) + (epsilon^2) / (sigb2)))
    bnds <- cbind(rep(0, sum(bl)), rep(1, sum(bl)))
    Ebb <- bw[bl] + rho * (sw / sb) * epsilon
    vbb <- sigw2 * (1 - rho^2)
    vbb <- ifelse(vbb < 1 * 10^-32, .0001, vbb)
    s <- ifelse(vbb >= 0 & vbb != Inf & !is.na(vbb), sqrt(vbb), NaN)
    b.s <- (bnds[, 2] - Ebb) / s
    as <- (bnds[, 1] - Ebb) / s
    res <- log(pnorm(as, lower.tail = F) - pnorm(b.s, lower.tail = F))
    # res <- log(pnorm(bnds[,2], mean=Ebb, sd=s) - pnorm(bnds[,1],
    # mean=Ebb, sd=s)) #res[ok] <- ifelse(abs(res[ok])==Inf,
    # NaN,res[ok])
    R[bl] <- .createR(bl, Rfun, bb, bw, sb, sw, rho, x)
    llik.bl <- llik.bl + sum(res) - sum(R[bl])
  }

  # T=0, 0<X<1
  llik.cT0 <- 0
  if (sum(cT0) > 0) {
    bb.cT0 <- bs[cT0, ][1]
    bw.cT0 <- bs[cT0, ][2]
    sigma <- matrix(c(sigb2, sigbw, sigbw, sigw2), nrow = 2)
    if (sum(cT0) == 1) {
      first <- log(dmvnorm(c(0, 0), mean = bs[cT0, ], sigma = sigma))
      second <- .createR(cT0, Rfun, bb, bw, sb, sw, rho, x)
      llik.cT0 <- sum(first) - sum(second)
    }
    else {
      first <- apply(
        bs[cT0, ], 1,
        function(x) {
          log(mvtnorm::dmvnorm(c(0, 0),
            mean = as.vector(x),
            sigma = sigma
          ))
        }
      )
      second <- NULL
      second <- .createR(cT0, Rfun, bb, bw, sb, sw, rho, x)
      llik.cT0 <- sum(first) - sum(second)
    }
  }

  # T=1, 0<X<1
  llik.cT1 <- 0
  if (sum(cT1) > 0) {
    bb.cT1 <- bs[cT1, ][1]
    bw.cT1 <- bs[cT1, ][2]
    sigma <- matrix(c(sigb2, sigbw, sigbw, sigw2), nrow = 2)
    if (sum(cT1) == 1) {
      first <- log(mvtnorm::dmvnorm(c(1, 1), mean = bs[cT1, ], sigma = sigma))
      # qi <- pmvnorm(lower=c(-bb.cT1/sb,-bw.cT1/sw),
      # upper=c(-bb.cT1/sb + 1/sb, -bw.cT1/sw + 1/sw),
      # mean=c(0,0), #corr=matrix(c(1,rho,rho,1), nrow=2))
      # qi <- ifelse(qi<0 | qi==0, 1*10^-322,qi)
      # second = ifelse(qi<0|qi==0, -999,log(qi))
      second <- .createR(cT1, Rfun, bb, bw, sb, sw, rho, x)
      llik.cT1 <- sum(first) - sum(second)
    }
    if (sum(cT1) > 1) {
      first <- apply(
        as.matrix(bs[cT1, ]), 1,
        function(x) {
          log(mvtnorm::dmvnorm(c(1, 1),
            mean = as.vector(x),
            sigma = sigma
          ))
        }
      )
      second <- NULL
      second <- .createR(cT1, Rfun, bb, bw, sb, sw, rho, x)
      # for (i in 1:length(bb.cT1)){
      # qi <- pmvnorm(lower=c(-bb.cT1[i]/sb,-bw.cT1[i]/sw), upper=c(-bb.cT1[i]/sb + 1/sb, -bw.cT1[i]/sw + 1/sw), mean=c#(0,0), corr=matrix(c(1,rho,rho,1), nrow=2))
      # qi <- ifelse(qi<0 | qi==0, 1*10^-322,qi)
      # second[i] = ifelse(qi<0|qi==0, -999,log(qi))
      # }
      llik.cT1 <- sum(first) - sum(second)
    }
  }
  llik <- llik.het + llik.bl + llik.wh + llik.cT0 + llik.cT1


  # Priors
  prior <- 0
  lpdfnorm <- log(dnorm(rho0, 0, sd = erho))
  if (esigma > 0) prior <- prior - (1 / (2 * esigma^2)) * (sigb2 + sigw2)
  if (erho > 0) prior <- prior + lpdfnorm
  if (ebeta > 0 & (mean(bb) < 0)) prior <- prior - .5 * ((mean(bb)^2) / ebeta)
  if (ebeta > 0 & mean(bb) > 1) prior <- prior - .5 * ((mean(bb) - 1)^2 / ebeta)
  if (ebeta > 0 & mean(bw) < 0) prior <- prior - .5 * ((mean(bw)^2) / ebeta)
  if (ebeta > 0 & mean(bw) > 1) prior <- prior - .5 * ((mean(bw) - 1)^2 / ebeta)
  if (sum(is.na(ealphab)) == 0) {
    prior <- prior +
      sum(dmvnorm(Bb0v, ealphab[, 1], sigma = diag(ealphab[, 2]^2), log = T))
  }
  if (sum(is.na(ealphaw)) == 0) {
    prior <- prior +
      sum(dmvnorm(Bw0v, ealphaw[, 1], sigma = diag(ealphaw[, 2]), log = T))
  }
  llik <- llik + prior
  # print(-llik)
  if (is.na(llik) | abs(llik) == Inf) llik <- NaN
  return(-llik)
}



#' This function is pulled directly from the ei package to enable a modified
#' version of ei.sim()
.createR <- function(sub, Rfun, bb, bw, sb, sw, rho, x, numb, numw) {
  out <- NULL
  lower <- cbind(-bb[sub] / sb, -bw[sub] / sw)
  upper <- cbind(-bb[sub] / sb + 1 / sb, -bw[sub] / sw + 1 / sw)
  mean <- c(0, 0)
  corr <- matrix(c(1, rho, rho, 1), nrow = 2)

  if (Rfun == 1) {
    out <- NULL
    makeR <- function(i) {
      qi <- pmvnorm(
        lower = lower[i, ], upper = upper[i, ], mean = mean,
        corr = corr
      )
    }
    out <- foreach(i = 1:length(x[sub]), .combine = "c") %dopar% makeR(i)
    # out <- apply(as.matrix(1:length(x[sub])), 1, makeR)
    out <- ifelse(out < 0 | out == 0, 1 * 10^-322, out)
    out <- log(out)
    # if(sum(is.na(out))>0|sum((out==Inf))>0) print("R not real")
    out <- ifelse(is.na(out) | abs(out == Inf), 999, out)
    return(out)
  }
  if (Rfun == 2) {
    makeR <- function(i) {
      qi <- sadmvn(
        lower = lower[i, ], upper = upper[i, ], mean = mean,
        varcov = corr
      )
    }
    # out <- foreach(i = 1:length(x[sub]), .combine="c") %dopar% makeR(i)
    out <- apply(as.matrix(1:length(x[sub])), 1, makeR)
    out <- ifelse(out < 0 | out == 0, 1 * 10^-322, out)
    out <- log(out)
    # if(sum(is.na(out))>0|sum((out==Inf))>0) print("R not real")
    out <- ifelse(is.na(out) | abs(out == Inf), 999, out)
    # return(out)
    # for(i in 1:length(x[sub])){
    # qi <- sadmvn(lower=lower[i,], upper=upper[i,], mean=mean, varcov=corr)
    # qi <- ifelse(qi<0|qi==0, 1*10^-322,qi)
    # out[i] <- log(qi)
    # if(is.na(out[i])|abs(out[i]==Inf)) print("R not real")
    # out[i] <- ifelse(is.na(out[i])|abs(out[i]==Inf), 999, out[i])
    # }
    return(out)
  }
  if (Rfun == 3) {
    fun <- function(x) dmvnorm(x, mean, corr)
    for (i in 1:length(x[sub])) {
      qi <- adaptIntegrate(fun,
        lowerLimit = lower[i, ],
        upperLimit = upper[i, ]
      )$integral
      out[i] <- log(qi)
      # if(is.na(out[i])|abs(out[i]==Inf)) print("R not real")
      out[i] <- ifelse(is.na(out[i]) | abs(out[i] == Inf), 999, out[i])
    }
    return(out)
  }
  ## THIS VERSION RELIES ON mvnprd FROM PACKAGE mvtnormpcs WHICH IS NOW ARCHIVED
  if (Rfun == 4) {
    print("Option Rfun==4 is no longer possible")
    stop()
    ##  for(i in 1:length(x[sub])){
    ##    qi <- mvnprd(A=upper[i,], B=lower[i,],
    ##                 BPD=c(rho,rho),INF=rep(2,2))$PROB
    ##    qi <- ifelse(qi<0|qi==0, 1*10^-322,qi)
    ##    out[i] <- log(qi)
    # if(is.na(out[i])|abs(out[i]==Inf)) print("R not real")
    ##    out[i] <- ifelse(is.na(out[i])|abs(out[i]==Inf), 999, out[i])
    ##  }
    ##  return(out)
  }

  if (Rfun == 5) {
    lower <- lower[1, ]
    upper <- upper[1, ]
    # qi <- pmvnorm(lower=lower, upper=upper, mean=mean, corr=corr)
    qi <- mnormt::sadmvn(lower = lower, upper = upper, mean = mean, varcov = corr)
    qi <- ifelse(qi < 1 * 10^-14, 1 * 10^-14, qi)
    qi <- log(qi)
    # if(is.na(qi)|abs(qi)==Inf) print ("R not real")
    qi <- ifelse((is.na(qi) | abs(qi) == Inf), 999, qi)
    out <- rep(qi, length(x[sub]))
    return(out)
  }
}
