#'
#' get_CI
#'
#' Determine the confidence intervals for each program
#' based on numerators/denominators
#'
#' @param program_name Vector of names of the transport program
#' @param num Vector of numerators
#' @param den vector of denominators
#' @author Rollie Parrish
#' @export




get_CI <- function(program_name, num, den) {

    getIndividualCI <- function (num, den) {
        data <- prop.test(num, den)
        return(data.frame(lower=data$conf.int[1], est=data$estimate, upper=data$conf.int[2]))
    }

#  temp <- data.frame(t(mapply(get.CI, neoIntubSuccess$intubate_neo_success_1st, neoIntubSuccess$intubate_neo_attempts)))
  temp <- data.frame(t(mapply(getIndividualCI, num, den)))

  results <- data.frame(group=paste0(program_name)
                , n=den
                , k=num
#                , lower=unlist(temp$lower)
#                , est=unlist(temp$est)
#                , upper=unlist(temp$upper)

                , lower=round(unlist(temp$lower)*100,1)
                , est=round(unlist(temp$est)*100, 1)
                , upper=round(unlist(temp$upper)*100, 1)
                )
return(results)
#return(program_name)

}


