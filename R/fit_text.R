#' @title Extract CFI, SRMR, and RMSEA into a text report
#' @description This function CFI, SRMR, and RMSEA from a previously fitted lavaan model into a text report (only robust measures sofar)


fit.text <- function(fit){
  fit2 <- (round(t(lavaan::fitmeasures(fit, fit.measures = c ("chisq.scaled", "df.scaled" ,"cfi.robust", "srmr", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust"))), digits = 3))
  fit2[,1] <- round(fit2[,1], 1)
  colnames(fit2) <- c("x2", "df","cfi rob", "srmr", "rmsea rob", "LL", "UL")
  paste0('x2', '(', fit2[,2], ') = ', fit2[,1], '; CFI = ', fit2[,3], '; ', 'SRMR = ', fit2[,4], '; RMSEA = ', fit2[,5], ', 90% CI [', fit2[,6], ', ', fit2[,7], ']')
}
