#' @title Extract CFI, SRMR, and RMSEA into a table
#' @description This function extracts CFI, SRMR, and RMSEA from a previously fitted lavaan model into a table syntax that can be paste into a word document and then be converted into a table (only robust measures sofar)


fit.table <- function(fit){
  fit2 <- (round(t(fitmeasures(fit, fit.measures = c ("chisq.scaled", "df.scaled" ,"cfi.robust", "srmr", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust"))), digits = 3))
  colnames(fit2) <- c("x2 rob", "df","cfi rob", "srmr", "rmsea rob", "LL", "UL")
  return(write.table(fit2, sep = ';'))
}
