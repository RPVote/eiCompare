ei.reg.bayes.conf.int <- function(ei_bayes){
  conf_int <- apply(ei_bayes$draws, 1:2, function(x) quantile(x, probs=c(.025, .5, .975)) )
  tab_out <- t ( as.data.frame(conf_int) )
  return ( tab_out )
}