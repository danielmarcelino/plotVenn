#' @title Match the areas of the 2 circles and their overlap to the input values for X, Y and the intersection of X and Y.
#' @description Calculates the radii of the two circles relative to a fixed center-to-center distance. The areas of the circles and their overlap will be proportionate the three input values of X alone, Y alone and the intersection of X and Y.
#' @param x_only a numeric value representing the relative size of X excluding its intersection with Y.
#' @param y_only a numeric value representing the relaitive size of Y excluding its intersection with X.
#' @param overlap a numeric value representing the relative size of the intersection of X and Y.
#' @param standardDistance a numeric value specifying the fixed center-to-center distance to which the radii are matched. The default value is that used by plotVenn2d
#'
#' @examples 
#'  plot.new()
#' plot_venn2d(rep("",3), radius=resize_circles(20,.5, 3), Title=NULL, 
#'           resizePlot=0.7, labels=c("",""))
#'           
#' @export
resize_circles <- function(x_only, y_only, overlap, standardDistance = sqrt(0.5))
{
	overlapArea <- function(r1, r2, d)
	{
		d1 <- (r1^2 - r2^2 + d^2) / (2 * d)
		d2 <- d - d1
		theta1 <- acos(d1 / r1)
		theta2 <- acos(d2 / r2)
		area1 <-  r1^2 * (theta1 - sin(2 * theta1) / 2)
		area2 <-  r2^2 * (theta2 - sin(2 * theta2) / 2)
		return (c(area1=area1, area2=area2, theta1=theta1, theta2=theta2, d1=d1, d2=d2))
	}

	if (x_only <= 0 | y_only <= 0 | overlap <= 0) return (NULL)
	
	ra1 <- sqrt((x_only + overlap) / pi)
	ra2 <- sqrt((y_only + overlap) / pi)
	
	r1 <- min(ra1,ra2)
	r2 <- max(ra1,ra2)
	bestFitDistance <- uniroot(function(d) sum(overlapArea(r1, r2, d)[1:2]) - overlap, c(r2 - r1+1e-6, r2 + r1 - 1e-6))$root

	return (c(ra1, ra2) / standardDistance / bestFitDistance)
}
