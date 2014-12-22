#' plot_metrics
#'
#' Returns a dot plot for a given metric. Based on numerators and denominators
#'
#' @param metric Name of the GAMUT metric
#' @param program_name Vector of names of the transport program
#' @param num vector of numerators for the metric
#' @param den vector of denominators
#' @param reordering Sets the sorting order. Default is 'decreasing'
#' @param qlabel Default is 'Estimated rate'
#' @param rate units of measure. Default is '\%'
#' @author Rollie Parrish
#' @export


plot_metrics <- function (metric= "metric name", program_name, num, den, reordering="decreasing", qlabel="Estimated rate", rate="%") {


  x <- get_CI(program_name
              , num
              , den
  )

  if (rate == "per 1000") {
    rate.overall <- sum(num)/sum(den)*1000
    x[4:6] <- x[4:6]*10
  } else
  if (rate == "per 10000") {
      rate.overall <- sum(num)/sum(den)*10000
      x[4:6] <- x[4:6]*100
    } else {
    rate.overall <- sum(num)/sum(den)*100
  }

  plot <- dotplot_errors(x
                 , main=paste(metric
                              , "\n"
                              , "Overall: "
                              , round(rate.overall,1)
                              , rate
                              , paste0(" (", sum(num),"/", sum(den),")")
                 )
                 , reference.line=rate.overall
                 , reordering=reordering
                 , qlabel=qlabel
                 )

  results.table <- (cbind(program_name, x[2:6]))
  names(results.table) <- c("Program", "den.", "num.", "lower CI", "rate", "upper CI")

  if (reordering == "increasing") {
    results.table <- results.table[ order(results.table[ ,5], results.table[ ,4], results.table[ ,6], results.table[ ,1]), ]
  } else {
    results.table <- results.table[ order(-results.table[ ,5], -results.table[ ,4], -results.table[ ,6], results.table[ ,1]), ]
  }


  invisible(list(plot=plot, table=results.table))
  #return(plot)

}
