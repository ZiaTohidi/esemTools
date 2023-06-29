
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
esem2ewc <- function(esem.fit, referents){
  temp <- esemTools::esem_loadings(esem.fit)
  ovs <- temp[,1]
  lvs <- colnames(temp)[-1]
  bs <- temp[,-1]
  syn <- matrix(nrow = length(ovs), ncol = length(lvs), dimnames = list(c(ovs), c(lvs)))
  for(i in 1:length(ovs)) {
    syn[i,] <- paste0('start(', bs[i,], ') * ', ovs[i])
  }
  for(i in 1:length(lvs)) {
    syn[ovs==referents[[i]],-i] <- paste0(bs[ovs==referents[[i]],-i], ' * ',referents[[i]])
  }
  syn.2 <- c()
  for(i in 1:length(lvs)) {
    syn.2[i] <- paste(syn[,i], collapse = ' + ')
  }
  syntax <- paste(lvs, ' =~ ', syn.2, collapse = ' \n ')
  return(syntax)
}
