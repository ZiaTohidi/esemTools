
#' Pick referent variables to be used in ESEM-within-CFA
#'
#' \code{pick_referent} takes an ESEM fit and select referent variables to be used in ESEM-within-CFA.
#'
#' @param esem.fit A traditional ESEM fitted model of class \code{lavaan}
#'
#' @return A named list containing one referent variable per each latent factor
#' @export
#'
#' @examples
#' # fit the CFA model
#'
#' cfa.mod <- 'visual  =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed   =~ x7 + x8 + x9'
#'
#' cfa.fit <- lavaan::cfa(cfa.mod, data = HolzingerSwineford1939, estimator = 'MLR')
#'
#' # fit the ESEM model
#'
#' esem.mod <- esemTools::cfa2esem(cfa.fit)
#'
#' esem.fit <- lavaan::cfa(esem.mod, data = HolzingerSwineford1939, estimator = 'MLR')
#'
#' # select referent variables
#'
#' refs <- pick_referent(esem.fit)
#'
#' # fit the EWC
#'
#' ewc.mod <- esemTools::esem2ewc(esem.fit, referents = refs)
#'
#' ewc.fit <- lavaan::cfa(ewc.mod, data = HolzingerSwineford1939, estimator = 'MLR')
#'

pick_referent <- function(esem.fit) {
  f.loadings <- esemTools::esem_loadings(fit = esem.fit)
  abs.loadings <- lapply(f.loadings[-1], abs) |> as.data.frame()
  diffs <- data.frame(1:nrow(f.loadings))
  referents <- list()
  for(i in 1:(ncol(f.loadings)-1)) {
    diffs[i] <- abs.loadings[i] - rowMeans(abs.loadings[-i])
    referents[[i]] <- f.loadings$ovs[diffs[i] == max(diffs[i])]
  }
  names(referents) <- colnames(f.loadings[,-1])
  return(referents)
}
