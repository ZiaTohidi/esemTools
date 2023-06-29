

#' Generate mediation analysis syntax for lavaan
#' \Code{med_syntax} generates mediation analysis syntax from a CFA model, with the first latent variable as the independent, the last as dependent, and the rest as mediators.
#' @param fit.cfa
#'
#' @return
#' @export
#'
#' @examples
med_syntax <- function(fit.cfa) {
  iv <- fit.cfa@pta$vnames$lv[[1]][1]
  dv <- fit.cfa@pta$vnames$lv[[1]][length(fit.cfa@pta$vnames$lv[[1]])]
  m <- fit.cfa@pta$vnames$lv[[1]][c(-1,-length(fit.cfa@pta$vnames$lv[[1]]))]
  a.path <- paste0('a.', m)
  b.path <- paste0('b.', m)
  ab.path <- paste0(a.path, '*', b.path)
  m.cov <- c()
  for(i in 1:(length(m)-1)) {
    m.cov[i] <- paste0(m[i], ' ~~ ', paste0(m[(i+1):length(m)], collapse = ' + '))
  }
  med.mod <- paste0(
    '#Direct effects \n ',
    paste0(dv, ' ~ c*', iv, ' \n '),
    paste0(dv, ' ~ ',
           paste0(b.path,'*', m, collapse = ' + '), ' \n '),
    paste0(m, ' ~ ', a.path, '*', iv, collapse = ' \n '), ' \n',
    '#Indirect effects \n ',
    paste0('ab.', m, ' := ', ab.path, collapse = ' \n '), '\n',
    '#Total effect \n ',
    paste0('total := ', 'c + ',
           paste(ab.path, collapse = ' + ')
    ), '\n',
    '#Covariance among mediators \n ',
    paste0(m.cov, collapse = ' \n ')
  )
  return(med.mod)
}
