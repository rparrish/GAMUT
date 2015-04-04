#' pd
#'
#' Returns a data frame with aggregated numerators and denominators.
#' Filters out incomplete cases and those with more denominator cases
#' than numerators.
#'
#' @param dag list of redcap data access groups. Shows the program name for the
#' data access groups in the list and the anonymous ID for all others. If empty (default), then all
#' program names will be shown.
#' @param operator used to show multiple data access groups from the same operator (ie. AIM, MTR, etc.).
#'  Will display the program names when the first three letters of the program name matches the value
#'  in this field.
#' @param ... comma separated list of variables to include.
#' @author Rollie Parrish
#' @export
#' @import dplyr

pd <- function( dag = "", operator = "",  ...) {
    program_data <-
        monthly_data %>%
        #filter(redcap_event_name != "Initial") %>%
        select_("program_name", "ID", "redcap_data_access_group", ...) %>%
        filter(!is.na(num),
               !is.na(den),
               den > 0,
               den >= num) %>%
        group_by(program_name, ID, redcap_data_access_group) %>%

        #
        summarise_each(funs(sum)) %>%
        ungroup() %>%
        {
            if(dag != "") {
                mutate(., label = ifelse(redcap_data_access_group %in% dag, as.character(program_name), ID))
            } else {mutate(., label = program_name)}
        } %>%
        {
            if(operator != "") {
                mutate(., label = ifelse(substr(as.character(program_name), 1,3) == operator, as.character(program_name),label ))
            } else {.}
        }


    program_data$mark = 16

    results <- program_data

    invisible(results)
}
