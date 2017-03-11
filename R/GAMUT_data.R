#'
#' GAMUT_Data
#'
#' get GAMUT data from REDCap, transform/aggregate by
#' individual program. Save as metricData to
#' GAMUT.Rdata file in the data folder
#'
#' @author Rollie Parrish
#' @export

GAMUT_data <- function(file="data/GAMUT.Rdata") {
    ## load data
    #source(".REDCap_config.R")

    GAMUT_data <- tbl_df(redcap_read_oneshot(redcap_uri=uri,
                                      token=GAMUT_token,
                                      export_data_access_groups=TRUE,
                                      raw_or_label = "label")$data)

    AIM_data <- tbl_df(redcap_read_oneshot(redcap_uri=uri,
                                    token=AIM_token,
                                    export_data_access_groups=TRUE,
                                    raw_or_label = "label")$data)

    AEL_data <- tbl_df(redcap_read_oneshot(redcap_uri=uri,
                                    token=AEL_token,
                                    export_data_access_groups=TRUE,
                                    raw_or_label = "label")$data)

    MTr_data <- tbl_df(redcap_read_oneshot(redcap_uri=uri,
                                    token=MTr_token,
                                    export_data_access_groups=TRUE,
                                    raw_or_label = "label")$data)

    redcap_data <- bind_rows(GAMUT_data, AIM_data, AEL_data, MTr_data)

    mydata <- redcap_data %>%
        filter(redcap_data_access_group != "zzz_retired__do_no") %>%
        mutate(program_name = as.factor(program_name),
               redcap_event_name = as.factor(redcap_event_name),
               redcap_data_access_group = as.factor(redcap_data_access_group),
               program_info_complete = as.factor(program_info_complete),
               monthly_data_complete = as.factor(monthly_data_complete)
               )

    program_info <- mydata %>%
        filter(redcap_event_name == "Initial") %>%
        select(program_name,redcap_data_access_group:program_info_complete)

    ID.lookup <- data.frame(
        program_name = levels(mydata$program_name)
        , ID = anonymize(as.factor(mydata$program_name))
    )

    #mydata <- mydata %>% inner_join(ID.lookup)

    monthly_data <-
        mydata %>%
        filter(redcap_event_name != "Initial") %>%
        filter(!is.na(total_patients)) %>%
        droplevels() %>%
        mutate(month = as.Date(zoo::as.yearmon(as.character(redcap_event_name)))) %>%
        select(program_name,  month, redcap_data_access_group, total_patients:monthly_data_complete)

    metricData_count <- monthly_data %>%
        group_by(program_name) %>%
        summarise(months_reported = n())

    metric_data <-
        redcap_read_oneshot(redcap_uri = uri, token = metric_token, raw_or_label = "label")$data %>%
        tbl_df()

    GAMUT_date_loaded <- date()

    save(redcap_data, mydata,
         program_info,  monthly_data,
         metric_data,
         GAMUT_date_loaded,
         file=file)
}
