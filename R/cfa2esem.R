#' Generate ESEM syntax from a CFA fit
#'
#' \code{cfa2esem} takes a CFA fit from lavaan and returns its corresponding ESEM syntax.
#' @param cfa.fit
#'
#' @return
#' @export
#'
#' @examples
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
