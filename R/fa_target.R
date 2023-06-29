#' @title Fit an EFA with target rotation
#' @description This function fits an EFA with an oblique target rotation, with the target keys extracted from a lavaan model syntax
#' @importFrom dplyr %>%

fa.target <- function(model, data, n_iter = NULL) {
  fit.cfa <- lavaan::cfa(model = model)
  dat.ordered <- dplyr::select(data, fit.cfa@pta$vnames$ov[[1]])
  first.i <- fit.cfa@pta$vidx$lv.marker[[1]]
  last.i <- c(fit.cfa@pta$vidx$lv.marker[[1]] [-1] - 1, length(fit.cfa@pta$vnames$ov[[1]]))
  target.key <- psych::make.keys(ncol(dat.ordered), eval(parse(text = paste0('list(',paste0(fit.cfa@pta$vnames$lv[[1]], ' = ', first.i, ':', last.i, collapse = ', '), ')')))
  ) %>%
    psych::scrub(isvalue = 1)
  res <- psych::fa(dat.ordered, nfactors = fit.cfa@pta$nfac[[1]], rotate = 'targetQ', Target = target.key, n.iter = n_iter)
  attr(x = res$loadings, which = 'dimnames')[[2]] <- fit.cfa@pta$vnames$lv[[1]][readr::parse_number(dimnames(res$loadings)[[2]])]
  return(res)
}
