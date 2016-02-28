#'
#' beeswarm_plot
#'
#' build a horizontal beeswarm plot with error bars for individual program
#'
#' @param data dataframe from pd() function that contains program_name, ID, redcap_data_access_group, num, den, label, and mark
#' @param title plot title
#' @return returns a beeswarm plot
#' @author Rollie Parrish
#' @export


beeswarm_plot <- function(data = NULL, title = "GAMUT Metric title")  {

    p <- {
    beeswarm(data$num/data$den,
             pch=1, horizontal = TRUE,
             xaxt = "n",
             bty = "n",
             main = paste0(title),
             xlab = paste0("\nGAMUT Overall: ",
                           round(sum(data$num)/sum(data$den)*100,1),
                           "%", " (",
                           sum(data$num), "/",
                           sum(data$den), ")"),method="hex"
    )

    indiv_points <-
        data %>%
        filter(program_name == params$program_name) %>%
        with(., prop.test(num, den))

    arrows(x0 = indiv_points$conf.int[1], y0=1.3,
           x1 = indiv_points$conf.int[2], y1=1.3,
           code = 3, angle = 90, col = "blue",
           length =.1)
    points(y = 1.3,x=indiv_points$estimate, pch=19, cex = 1.2, col = "blue") # add mean

    overall_mean <- sum(data$num)/sum(data$den)

    # add mean
    segments(y0 = .6, x0=overall_mean,
             y1 = 1.4, x1 = overall_mean,
             pch=16, lty = "dashed",
             col = "darkgrey")

    # add text labels
    text(x = indiv_points$estimate, y = 1.45, paste0("  ", round(indiv_points$estimate,3)*100,"%"),
         col = "blue")

    #text(x = overall_mean, y = .55, paste0("Overall: ", round(overall_mean,3)*100,"%"))

    axis(1, at=pretty(data$num/data$den),
         lab=paste0(pretty(data$num/data$den) * 100, "%"),

         las=TRUE)
    }

    #print(p)
    invisible(p)
}
