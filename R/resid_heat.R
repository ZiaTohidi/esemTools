
library(lavaan)
library(ggplot2)
require(reshape2)

#' Heat map for residuals
#'
#' \code{resid_heat} return a heat map, depicting the residual covariance among observed variables
#' @param fit
#' @param threshold
#' @param show.values
#' @param font.size
#'
#' @return
#' @export
#'
#' @examples

heat_resid <- function(fit, threshold = FALSE, show.values = FALSE, font.size = NA) {
  require(ggplot2)

  if(class(fit) != 'lavaan') {
    stop("'fit' must be an object of class 'lavaan'")
  }
  if(is.logical(threshold) != T) {
    warning("Ignoring the 'threshold' arguement, as it must be logical.")
  }
  if(is.logical(show.values) == F) {
    warning("Ignoring the 'show.values' arguement, as it must be logical.")
  }
  if(is.double(font.size) == F) {
    warning("Ignoring the 'font.size' arguement, as it must be a number.", call. = F)
  }

  resids <- lavResiduals(fit, 'cor')$cov |> round(2)
  diag(resids) <- NA
  cor.dat <- reshape2::melt(resids)
  cor.cat <- cor.dat
  cor.cat$value <- ifelse(cor.dat$value > .3, 'r > .3',
                          ifelse(cor.dat$value > .2, 'r > .2',
                                 ifelse(cor.dat$value < -.3, 'r < -.3',
                                        ifelse(cor.dat$value < -.2, 'r < -.2', '-.2 < r < .2')
                                 )
                          )
  )
  if(threshold == TRUE) {
    cor.cat$value <- as.character(cor.cat$value)
    cor.cat.plot <- ggplot(data = cor.cat, aes(x=Var1, y=Var2, fill=value)) +
      geom_tile(color = "gray") + scale_fill_manual(values=c('r > .3' = "blue4",
                                                             'r > .2' = 'blue1',
                                                             '-.2 < r < .2' = "white",
                                                             'r < -.2' = "red1",
                                                             'r < -.3' = 'red4'), na.value = 'black') + scale_y_discrete(limits = rev)
    cor.cat.plot$labels$fill <- 'residual r'
    cor.cat.plot$labels$x <- NULL
    cor.cat.plot$labels$y <- NULL
    if(show.values == T) {
      cor.cat.plot <- cor.cat.plot +
        geom_text(aes(label = cor.dat$value), size = font.size, na.rm = T)
    }
    return(cor.cat.plot)
  } else {
    cor.plot <- ggplot(data = cor.dat, aes(x=Var1, y=Var2, fill=value)) +
      geom_tile(color = "gray") + scale_fill_gradient2(high = 'blue4', low = 'red4', na.value = 'black') + scale_y_discrete(limits = rev)
    cor.plot$labels$fill <- 'residual r'
    cor.plot$labels$x <- NULL
    cor.plot$labels$y <- NULL
    if(show.values == T) {
      cor.plot <- cor.plot + geom_text(aes(label = value), size = font.size, na.rm = T)
    }
    return(cor.plot)
  }
}
