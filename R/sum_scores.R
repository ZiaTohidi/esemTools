#' @title Calculate Sum Scores from CFA Model

#' @param data
#'
#' @param model
#'
#' @description This function takes a data frame and a CFA model syntax and calculates sum scores for each latent variable

sum_scores <- function (data, model) {
  fit.cfa <- lavaan::cfa(model = model)
  lv.t <- fit.cfa@pta$vnames$lv[[1]]
  ov.t <- fit.cfa@pta$vnames$ov[[1]]
  lv.of.ov <- fit.cfa@ParTable$lhs[1:length(ov.t)]
  dat.t <- data[, ov.t]
  dat.out <- data.frame(1:nrow(data))
  for (i in 1:length(lv.t)) {
    dat.out[, i] <- rowMeans(dat.t[, lv.of.ov==lv.t[i]], na.rm = T) * length(lv.of.ov[lv.of.ov==lv.t[i]])
  }
  colnames(dat.out) <- paste0(lv.t, "_s")
  return(dat.out)
}
