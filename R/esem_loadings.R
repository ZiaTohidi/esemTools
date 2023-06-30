
#' ESEM loading pattern matrix
#'
#' \code{esem_loadings} takes an ESEM fit from lavaan and returns a loading pattern matrix. Currently, only traditional ESEM models are supported, not other extended methods such as set ESEM.
#'
#' @param fit
#' @param digits
#' @param efa.block
#' @param standardized
#'
#' @return A factor loading matrix for an EFA block
#' @export
#'
#' @examples
#' library(lavaan)
#' library(esemTools)
#'
#' esem.mod <- 'efa("efa") * visual +
#'              efa("efa") * textual +
#'              efa("efa") * speed =~
#'              x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9'
#'
#' esem.fit <- cfa(esem.mod, data = HolzingerSwineford1939)
#'
#' esem_loadings(esem.fit, standardized = T, digits = 2)


esem_loadings <- function(fit, efa.block = NULL, standardized = TRUE, digits = 3) {
  if(class(fit) != 'lavaan') {
    stop("'fit' must be a fitted ESEM model of class 'lavaan")
  }
  if(fit@Model@nefa == 0) {
    stop("This is not an ESEM model.")
  }
  if(fit@Model@nefa > 1 && is.null(efa.block) == TRUE) {
    warning(paste("Your model has", fit@Model@nefa, "EFA blocks. The first block is used."))
  }
  if(fit@Model@nefa == 1 && is.null(efa.block) == TRUE) {
    block.n <- 1
    block <- names(fit@Model@lv.efa.idx[[1]])
    } else {
      if(typeof(efa.block) == 'double') {
        if(efa.block <= fit@Model@nefa) {
          block.n <- efa.block
          block <- names(fit@Model@lv.efa.idx[[1]])[as.integer(efa.block)]
          } else {
            stop(paste0("The number specified for 'efa.blocks' exceeds the total EFA blocks."))
          }
        } else {
          if(typeof(efa.block) == 'character') {
            if(efa.block %in% names(esem.fit@Model@lv.efa.idx[[1]])) {
              block.n <- (1:fit@Model@nefa)[names(fit@Model@lv.efa.idx[[1]])==efa.block]
              block <- efa.block
              } else {
                stop(paste0("The EFA block '", efa.block, "' does not exist in your model."))
                }
            } else {
              stop("'efa.block' must be a string character or a number specifying which EFA block to be used.")
            }
        }
      }
    ovs <- fit@pta$vnames$ov.efa[[block.n]]
  lvs <- fit@pta$vnames$lv.efa[[block.n]]
  ests <- lavaan::parameterestimates(fit, standardized = standardized)
  if(standardized == TRUE) {
    ests$est <- ests$std.all
  }
  ests <- ests[ests$op == '=~' & ests$efa==block, ]
  load.mat <- matrix(nrow = length(ovs), ncol = length(lvs), dimnames = list(NULL, lvs))
  for(i in 1:length(lvs)) {
    for(j in 1:length(ovs)) {
      load.mat[j,i] <- ests$est[ests$lhs==lvs[i] & ests$rhs==ovs[j]]
    }
  }
  load.mat <- data.frame(ovs, round(load.mat, digits = digits))
  return(load.mat)
}
