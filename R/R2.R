
#' @title R-Squared from Correlation Matrix
#' @description This function takes a correlation matrix and calculates multiple squared correlation between each variable and all other variables

R2 <- function(r){
  n.var <- ncol(r)
  r.yx <- list()
  r.xx <- list()
  r2.val <- data.frame(R2 = 1:n.var)
  for(i in 1:n.var){
    r.yx[[i]] <- as.matrix(r[-i,i])
    r.xx[[i]] <- r[-i, -i]
    r2.val$R2[i] <- t(r.yx[[i]]) %*% solve(r.xx[[i]]) %*% r.yx[[i]]
  }
  if(is.null(colnames(r))==F) {rownames(r2.val) = colnames(r)}
  return(r2.val)
}
