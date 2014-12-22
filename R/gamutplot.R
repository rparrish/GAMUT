#'
#' gamutplot
#'
#' returns the GAMUT dotplot and table
#'
#' @param title the main title for the plot
#' @param mydata data frame with group, numerator, denominator
#' @param reordering sort order for the table and plot
#' @param ... further arguments passed to or from other methods.
#' @return a list - table has the raw data and plot has the dotplot
#' @author Rollie Parrish
#' @export

gamutplot <- function(title = "test",
                       mydata,
                       reordering,
                       ...
                       ) {


    plotData <- mydata[mydata[,3] > 0, ]


    results <- plot_metrics(title,
        program_name = plotData$group,
        num = plotData[,2],
        den = plotData[,3],
        reordering = reordering,
        ...
    )

    results
}




