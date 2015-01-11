

#require(GAMUT)


fields <- c(~unintended_hypothermia, ~total_neo_patients)

gamut_metrics <- function(fields, title="main title", ...) {

    data <-
        mydata %>%
        group_by(program_name, ID) %>%
        select_(.dots = fields)

    var_names <- names(data)
    colnames(data) <- c("program_name", "ID", "num", "den")

    program_table <-
        data %>%
        filter(den > 0) %>%
        do(filter(., complete.cases(.))) %>%
        summarise_each(funs(sum), num, den) %>%
        mutate(rate = num/den) %>%
        ungroup() %>%
        arrange(rate) %>%
        filter(den > 20) %>%
        mutate(cumsum = cumsum(den),
               cum_percent = cumsum / sum(den)
               )

    monthly_counts <-
        data %>%
        do(filter(., complete.cases(.))) %>%
        group_by(program_name, ID) %>%
        summarise(months_reported = n()) %>%
        mutate(fully_reporting = ifelse(months_reported >= 11, 1,0))

    program_table <-
        monthly_counts %>%
        inner_join(program_table) %>%
        ungroup() %>%
        arrange(cum_percent)

    overall_benchmark <-
        program_table %>%
        filter(cum_percent <= .1) %>%
        filter(cum_percent == max(cum_percent)) %>%
        select(benchmark_rate = rate)

    overall_total <-
        program_table %>%
        summarise(reporting = n(),
                  fully_reporting = sum(fully_reporting),
                  percent_fully_rep = fully_reporting/reporting,
                  num = sum(num),
                  den = sum(den),
                  overall_rate = num/den)

    overall_quantiles <-
        program_table %>%
        summarise(tenth = quantile(rate, .1),
                  median = median(rate),
                  ninetieth = quantile(rate, .9)
                  )


    overall <- cbind(overall_total, overall_benchmark, overall_quantiles)

    ## boxplot
    #
    bp <- boxplot(program_table$rate,
                  main = title,
                  frame = FALSE,
                  axes = FALSE,
                  horizontal=TRUE,
                  boxwex=.05,
                  staplewex=0,
                  whisklty="solid")

    results <- list(program_table = program_table,
                    overall = overall)

    results

}

bp <- function() {
    boxplot(program_table$rate,
            frame = FALSE,
            #axes = FALSE,
            horizontal=TRUE,
            boxwex=.05,
            staplewex=0,
            whisklty="solid")
    title("world", line = -2)
    text(x$overall$median, .95, "hi")
    text(x$overall$ninetieth, .95, "hi")

}




