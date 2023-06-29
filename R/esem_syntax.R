#' @param efa.fit
#'
#' @param anchors
#'
#' @title Generate ESEM model syntax automatically
#' @description This function generates an ESEM model syntax based on a previously fitted EFA model and a list of anchor variables or items
#' @importFrom dplyr %>%


esem_syntax <- function(efa.fit, anchors){
  st.fix <- list()
  ltext <- list()
  for(i in 1:ncol(efa.fit$loadings)) {
    st.fix[[i]] <- ifelse(rownames(efa.fit$loadings)==anchors[i], '(', 'start(')
    ltext[[i]] <- paste0(st.fix[[i]], round(efa.fit$loadings[,i], 2), ')*', collapse = ' + ', rownames(efa.fit$loadings))
  }
  mod.text <- paste0(colnames(efa.fit$loadings), ' =~ ', ltext, collapse = '\n')
  return(mod.text)
}
