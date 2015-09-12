

# this file is listed in .Rbuildignore

## monthly trend
# get the programs with at least 10 months

trend_plot <- function(measures, main=NULL, threshold=10, monthly_data = monthly_data) {
    require(dplyr)
    data <-
        monthly_data %>%
        select_(.dots = measures, "month", "program_name") %>%
        filter(!is.na(num),
           !is.na(den),
           den > 0,
           den >= num)

    programs_to_include <-
        data %>%
        group_by(program_name) %>%
        tally() %>%
        filter(n >= threshold)

    program_data <-
        data %>%
        group_by(program_name) %>%
        summarise_each(funs(sum), -month) %>%
        ungroup() %>%
        mutate(rate= num/den)

    trend_data <-
        data %>%
        inner_join(programs_to_include) %>%
        group_by(month) %>%
        summarise_each(funs(sum), -c(n,program_name)) %>%
        mutate(rate = num/den)

    monthly_plot <-
        plot(rate ~ month, data = trend_data, type="b",
             main=paste0(measures$num[1], "\nPrograms reporting at least ", threshold, " months"),
             pch=16,
             las=1)

    program_plot <-
        beeswarm(program_data$rate,
                 method="center",
                 pch=16,
                 main=measures$num[1]
                 )

    results <-
        list(data=data,
             program_data=program_data,
             trend_data=trend_data,
             programs=programs_to_include,
             plot=monthly_plot,
             program_plot=program_plot
             )
    results
}

measures <- list(num="ped_adv_airway_capno",
                 den="ped_adv_airway_cases"
                 )

trend_plot(measures, main="hello")

trend_plot(list(num="neo_adv_airway_vent", den="neo_adv_airway_cases"), threshold=3)
trend_plot(list(num="ped_adv_airway_vent", den="ped_adv_airway_cases"), threshold=3)
trend_plot(list(num="adult_adv_airway_cases", den="total_adult_patients"), threshold=3)

trend_plot(list(num="adult_adv_airway_capno", den="adult_adv_airway_cases"))
trend_plot(list(num="adult_adv_airway_vent", den="adult_adv_airway_cases"))
