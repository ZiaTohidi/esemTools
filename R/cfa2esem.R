#' Generate ESEM syntax from a CFA fit
#'
#' \code{cfa2esem} takes a CFA fit from lavaan and returns its corresponding ESEM syntax.
#' @param cfa.fit An fitted model of class \code{lavaan}
#'
#' @return A string syntax for its corresponding ESEM model
#' @export
#'
#' @examples
#' # fit the CFA model
#'
#' cfa.mod <- ' visual =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed =~ x7 + x8 + x9'
#'
#' cfa.fit <- lavaan::cfa(cfa.mod, data = HolzingerSwineford1939, estimator = 'MLR')
#'
#' summary(cfa.fit, std = T, fit = T)
#'
#'
#' # fit the ESEM model
#'
#' esem.mod <- cfa2esem(cfa.fit)
#'
#' esem.fit <- cfa(esem.mod, data = HolzingerSwineford1939, estimator = 'MLR')
#'
#' summary(esem.fit, std = T, fit = T)
#'
cfa2esem <- function(cfa.fit) {
  syntax.esem <- paste0(
    paste0(
      paste0('efa("efa1") * ', cfa.fit@pta$vnames$lv[[1]]),
      collapse = ' + \n'),
    ' =~ ',
    paste0(
      cfa.fit@pta$vnames$ov[[1]], collapse = ' + '
    )
  )
  return(syntax.esem)
}
