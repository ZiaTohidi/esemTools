#' @title Generate parceled data set
#' @description This function Creates a data frame with the parcel scores calculated for items within subscales based on a CFA structure
#' @import lavaan

parcel.it <- function(data, model, p.size = 2) {
  fit.cfa <- lavaan::cfa(model)
  dat.t <- dplyr::select(dat, eval(parse(text = fit.cfa@pta$vnames$ov)))
  n.fac <- fit.cfa@pta$nfac[[1]]
  name.lv <- fit.cfa@pta$vnames$lv[[1]]
  item.on.lv <- fit.cfa@ParTable$lhs [1:ncol(dat.t)]
  dat.list <- list()
  for (i in 1:n.fac) {
    dat.list[[i]] <- dat.t[,item.on.lv==name.lv[i]]
  }
  key.parc.list <- list()
  dat.parc.list <- list()
  for(i in 1:n.fac) {
    key.parc.list[[i]] <- psych::parcels(dat.list[[i]], size = p.size)
    if(sum(key.parc.list[[i]]) < ncol(dat.list[[i]])){
      key.parc.list[[i]] <- as.data.frame(key.parc.list[[i]])
      key.parc.list[[i]] [, (ncol(key.parc.list[[i]])+1)] <- 0
      key.parc.list[[i]] [rowSums(key.parc.list[[i]])==0, (ncol(key.parc.list[[i]]))] <- 1
      key.parc.list[[i]] <- as.matrix(key.parc.list[[i]])
    }
    dat.parc.list[[i]] <- psych::scoreItems(keys = key.parc.list[[i]], items = dat.list[[i]])
    colnames(dat.parc.list[[i]]$scores) <- paste0(name.lv[i], 1:ncol(dat.parc.list[[i]]$scores))
  }
  dat.parcel <- data.frame()
  t.cbind <- paste0('dat.parc.list', '[[', 1:n.fac, ']]$scores', collapse = ', ')
  dat.parcel <- eval(parse(text = paste('cbind(', t.cbind, ')')))
  return(dat.parcel)
}
