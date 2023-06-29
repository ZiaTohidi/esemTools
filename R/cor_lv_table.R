#' Model-implied correlation table with significance asterisks
#'
#' \code{cor_lv_table} takes a 'lavaan' object and returns a correlation table with significance asterisks
#'
#' @param fit
#'
#' @return
#' @export
#'
#' @examples

cor_lv_table <- function(fit){
  if(class(fit) != 'lavaan') {
    stop("The 'fit' arguement must be an object of class 'lavaan'")
  }
  lvs <- fit@pta$vnames$lv[[1]]
  ps <- lavaan::parameterestimates(object = fit, standardized = T)
  cors <- ps[(nrow(ps)-(length(lvs)*(length(lvs)-1)/2)+1):nrow(ps),] # select the correlation lines
  cor.mat <- lavaan::lavInspect(fit, 'cor.lv')
  p.mat <- matrix(nrow = length(lvs), ncol = length(lvs))
  p.mat[lower.tri(p.mat)==T] <- cors$pvalue
  p.mat[lower.tri(p.mat)==F] <- t(p.mat)[lower.tri(p.mat)==F]
  cor.sign <- matrix(nrow = length(lvs), ncol = length(lvs))
  cor.sign <- ifelse(p.mat<.01, '**',ifelse(p.mat<.05, '*', ''))
  cor.tab <- matrix(data = paste0(round(cor.mat, 2),cor.sign), nrow = length(lvs), ncol = length(lvs))
  diag(cor.tab) <- '–'
  cor.tab <- sub('0.', '.', cor.tab)
  dimnames(cor.tab)[[1]] <- lvs
  dimnames(cor.tab)[[2]] <- 1:length(lvs)
  return(cor.tab)
}
