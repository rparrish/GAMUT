#' plot_metrics_wm
#'
#' Returns a dot plot for a given metric. Based on weighted averages
#'
#' @param metric Name of the GAMUT metric
#' @param program_name Vector of names of the transport program
#' @param wavg Weighted average
#' @param rate_overall the benchmark value from all records
#' @param num the total number of cases
#' @param reordering Sets the sorting order. Default is 'decreasing'
#' @param qlabel Default is 'Estimated rate'
#' @author Rollie Parrish
#' @export


plot_metrics_wm <- function (metric= "metric name", program_name, wavg, rate_overall, num, reordering="decreasing", qlabel="") {

  x <- data.frame(group=program_name, est=wavg)

  #return(str(x))


  dotplot_no_errors(x
                 , main=paste(metric
                              , "\n"
                              , "Overall: "
                              , round(rate_overall,0)
                              , "min. "
                              , "("
                              , num
                              , " cases)"
                 )
                 , reference.line=rate_overall
                 , reordering=reordering
                 , qlabel=qlabel

  )

  # return(cbind(program_name, x[2:6]))

}
