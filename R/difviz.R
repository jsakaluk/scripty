#' A Function that ggplots Uniform or Nonuniform DIF
#'
#' This function takes output from getdifests and creates a geom_tile viz to show uniform or non-uniform DIF. Rows for items that aren't flaged as showing sig. DIF are more translucent
#' @param ests output df of measurement model parameters from difests
#' @param type whether user wants to extract loading ("loads") or interpts "ints"
#' @param covtitle  character input of name for covariate (both on x-axis title, and in file name)
#' @param lvtitle character input for name of lv (for file naming)
#' @return a ggplot of uniform or nonuniform DIF
#' @export
#' @examples
#' uni.plot <- difviz(ests, type = "ints", covtitle = "Social Class", lvtitle "Satisfaction")

difviz <- function(ests, type = NULL, covtitle, lvtitle){
  dir.create("./output")
  if(type == "loads"){
    p <- ggplot2::ggplot(data = ests, aes(x=covlev, y=rhs, fill=std.all, alpha = sig)) +
      ggplot2::geom_tile()+
      ggplot2::labs(x = covtitle, y = "Item #")+
      ggplot2::scale_fill_continuous(name = "Standardized\nLoading", high = "black", low = "white")+
      ggplot2::scale_alpha_manual(values = c(0.4, 1), name = "Non-Uniform DIF")
    p
    ggplot2::ggsave(p, filename = sprintf("./output/%s_%snonuniform.png", lvtitle, trimws(covtitle)), height = 6, width = 8, dpi = 300)
  }else if(type == "ints"){
    p <- ggplot(data = ests, aes(x=covlev, y=lhs, fill=est, alpha = sig)) +
      ggplot2::geom_tile()+
      ggplot2::labs(x = covtitle, y = "Item #")+
      ggplot2::scale_fill_continuous(name = "Item\nItercept", high = "black", low = "white")+
      ggplot2::scale_alpha_manual(values = c(0.4, 1), name = "Uniform DIF")
    p
    ggplot2::ggsave(p, filename = sprintf("./output/%s_%suniform.png", lvtitle, trimws(covtitle)), height = 6, width = 8, dpi = 300)
  }
  return(p)
}
