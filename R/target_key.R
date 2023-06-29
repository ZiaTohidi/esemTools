#' Make target key for target rotation
#'
#' \code{target_key} takes a CFA fit from lavaan and creates a target key to be used in ESEM or EFA with target rotation.
#' @param model
#' @param data
#'
#' @return
#' @export
#'
#' @examples
tar.key <- function (model, data) {
  fit.cfa <- lavaan::cfa(model = model)
  dat.ordered <- dplyr::select(data, fit.cfa@pta$vnames$ov[[1]])
  first.i <- fit.cfa@pta$vidx$lv.marker[[1]]
  last.i <- c(fit.cfa@pta$vidx$lv.marker[[1]][-1] - 1, length(fit.cfa@pta$vnames$ov[[1]]))
  target.key <- psych::make.keys(ncol(dat.ordered), eval(parse(text = paste0("list(", paste0(fit.cfa@pta$vnames$lv[[1]], " = ", first.i, ":", last.i, collapse = ", "), ")"))))
  target.key <- psych::scrub(target.key, isvalue = 1)
  return(target.key)
}
