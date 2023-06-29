
#' ESEM loading pattern matrix
#'
#' \code{esem_loadings} takes an ESEM fit from lavaan and returns a loading pattern matrix. Currently, only traditional ESEM models are supported, not other extended methods such as set ESEM.
#' @param fit
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
esem_loadings <- function(fit, digits = 3) {
  ovs <- fit@pta$vnames$ov[[1]]
  lvs <- fit@pta$vnames$lv[[1]]
  load.mat <- matrix(nrow = length(ovs), ncol = length(lvs), dimnames = list(NULL, lvs))
  for(i in 1:length(lvs)) {
    load.mat[,i] <- fit@ParTable$est[fit@ParTable$op=='=~' & fit@ParTable$lhs==lvs[i]]
  }
  load.mat <- data.frame(ovs, round(load.mat, digits = digits))
  return(load.mat)
}
