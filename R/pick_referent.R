
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
pick_referent <- function(esem.fit, efa.block = NULL) {
  nefa <- esem.fit@Model@nefa
  efa.labels <- names(esem.fit@Model@lv.efa.idx[[1]])
  f.loadings <- list()
  abs.loadings <- list()
  diffs <- list()
  referents <- list()
  referents[[i]] <- list()
  for(i in 1:nefa) {
    f.loadings[[i]] <- esemTools::esem_loadings(fit = esem.fit, efa.block = as.double(i))
    abs.loadings[[i]] <- lapply(f.loadings[[i]][-1], abs) |> as.data.frame()
    diffs[[i]] <- data.frame(1:nrow(f.loadings[[i]]))
    referents[[i]] <- list()
    for(j in 1:(ncol(f.loadings[[i]])-1)) {
      diffs[[i]][j] <- abs.loadings[[i]][j] - rowMeans(abs.loadings[[i]][-j])
      referents[[i]][[j]] <- f.loadings[[i]]$ovs[diffs[[i]][j] == max(diffs[[i]][j])]
    }
    names(referents[[i]]) <- colnames(f.loadings[[i]][,-1])
  }
  names(referents) <- efa.labels
  return(referents)
}
