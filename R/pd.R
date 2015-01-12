#' pd
#'
#' Returns a data frame with aggregated numerators and denominators.
#' Filters out incomplete cases and those with more denominator cases
#' than numerators.
#'
#' @param ... comma separated list of variables to include. First variable should be
#' group, which is either "program_name" or "ID".
#' @author Rollie Parrish
#' @export

pd <- function(...) {
    program_data <-
        mydata %>%
        filter(redcap_event_name != "Initial") %>%
        select_(...) %>%
        filter(!is.na(num),
               !is.na(den),
               den > 0,
               den >= num) %>%
        group_by(group) %>%
        summarise_each(funs(sum))

    program_data$mark <- 16

    results <- program_data

    invisible(results)
}
