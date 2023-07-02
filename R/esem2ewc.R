
#' ESEM-within-CFA syntax generator
#'
#' \code{esem2ewc} takes an ESEM fit from lavaan and generates the corresponding ESEM-within-CFA syntax. (for each latent variable, one referent item must be supplied.)

#' @param esem.fit
#' @param referents
#'
#' @return
#' @export
#'
#' @examples
#'
esem2ewc <- function(esem.fit, referents){
  nefa <- esem.fit@Model@nefa
  efa.labels <- names(esem.fit@Model@lv.efa.idx[[1]])
  efa.lv.length <- lapply(esem.fit@Model@lv.efa.idx[[1]], length)
  if(class(esem.fit) != 'lavaan') {
    stop("'esem.fit' must be a fitted ESEM model of class 'lavaan'")
  }
  if(esem.fit@Model@nefa == 0) {
    stop("This is not an ESEM model.")
  }
  if(is.list(referents[[1]]) == FALSE) {
    referents <- list(referents)
  }
  if(nefa != length(referents)) {
    stop(paste0("Your model has ", nefa, " EFA blocks; Supply ", nefa, " lists of referents."))
  }
  if(setequal(unlist(lapply(referents, length)), unlist(efa.lv.length)) == FALSE)
     {stop("The number of referent variables you specified is not identical to the number of EFA factors in the model.")}
  f.loadings <- list()
  ovs <- list()
  lvs <- list()
  bs <- list()
  syn <- list()
  syntax <- list()
  for(i in 1:nefa) {
    f.loadings[[i]] <- esemTools::esem_loadings(esem.fit, efa.block = efa.labels[i])
    ovs[[i]] <- f.loadings[[i]][,1]
    lvs[[i]] <- colnames(f.loadings[[i]])[-1]
    bs[[i]] <- f.loadings[[i]][,-1]
    syn[[i]] <- matrix(nrow = length(ovs[[i]]), ncol = length(lvs[[i]]), dimnames = list(c(ovs[[i]]), c(lvs[[i]])))
    for(j in 1:length(ovs[[i]])) {
      syn[[i]][j,] <- paste0('start(', bs[[i]][j,], ') * ', ovs[[i]][j])
    }
    for(j in 1:length(lvs[[i]])) {
      syn[[i]][ovs[[i]]==referents[[i]][[j]],-j] <- paste0(bs[[i]][ovs[[i]]==referents[[i]][[j]],-j], ' * ',referents[[i]][[j]])
    }
    syn.2 <- as.list(1:nefa)
    for(j in 1:length(lvs[[i]])) {
      syn.2[[i]][j] <- paste(syn[[i]][,j], collapse = ' + ')
    }
    syntax[[i]] <- paste(lvs[[i]], ' =~ ', syn.2[[i]], collapse = ' \n ')
  }
  ewc.syn <- paste(syntax, collapse = ' \n ')
  warning("When fitting the model, make sure that 'std.lv' is set to 'TRUE'.")
  return(ewc.syn)
}
