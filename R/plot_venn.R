#' @title Plot Venn diagram for 2-, 3-, 4- dimensional data
#' 
#' @description Plots a 2-, 3-, or 4-dimensional Venn plot depending on the number of input values. The user can specify values, labels for each circle-group and colors. If the vector is of length 3, a 2-d plot is produced. If the vector is of length 7, a 3-d plot is produced. If the vector is length 15, a 4-d plot is produced.
#' @param x for a 3-d plot, a numeric vector of length 7, with a permutation of the names c("001","010","011","100","101","110","111").
#' @param \dots Additional arguments accepted by plot_venn2d, plot_venn3d or plot_venn4d
#' @details 
#' The '00', '000', '0000' groups are not plotted, so 'percent' data will not add up to 100 percent on the graph.
#' @note It requires package grid to be installed, and can be plotted according to specified grid parameters.
#' @examples 
#' y <- c(37,29,6,232,121,77,25)
#' names(y) <- c("001","010","011","100","101","110","111")
#' labels <- c("A","B","C")
#' plot.new()
#' plot_venn(y, labels, Colors=rainbow(7))
#' 
#' @importFrom stats aggregate uniroot
#' @importFrom utils head
#' 
plot_venn <-
function(x, ...)
{ # chooses which plotVenn function to call based on length of x

  if (missing(x)) plot_venn4d(...)
  else
  {
    if (length(x) == 3) plot_venn2d(x, ...)
    if (length(x) == 7) plot_venn3d(x, ...)
    if (length(x) == 15) plot_venn4d(x, ...)
    if (!length(x) %in% c(3,7,15))
      stop('Specified data cannot be plotted by any of plotVenn diagrams. Please, check your data. Make sure to remove the "000" value.')
  }

}
NULL
