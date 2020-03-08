
#' send_redcap_to_mysql.R
#'
#' updates the GAMUT dashboard database tables
#'
#' @return a list - table has the raw data and plot has the dotplot
#' @author Rollie Parrish
#' @export

send_to_mysql <- function() {

    usethis::ui_done("Load REDCap data")
   uri <- Sys.getenv("REDCAP_GAMUT_uri")

    metric_details <- tbl_df(
        redcap_read_oneshot(
            redcap_uri = uri,
            token = Sys.getenv("metric_details_token"),
            export_data_access_groups = FALSE, 
            raw_or_label = "label"
        )$data
    )

    GAMUT_data <- tbl_df(
        redcap_read_oneshot(
            redcap_uri = uri,
            token = Sys.getenv("GAMUT_token"),
            export_data_access_groups = TRUE, #guess_max = 10000,
            guess_type = FALSE,
            raw_or_label = "label"
        )$data %>%
            mutate_at(vars(total_patients:sre), as.numeric)
    )
     AIM_data <- tbl_df(
        redcap_read_oneshot(
            redcap_uri = uri,
            token = Sys.getenv("AIM_token"),
            export_data_access_groups = TRUE, #guess_max = 10000,
            guess_type = FALSE,
            raw_or_label = "label"
        )$data %>%
            mutate_at(vars(total_patients:sre), as.numeric)
    ) 

    AEL_data <- tbl_df(
        redcap_read_oneshot(
            redcap_uri = uri,
            token = Sys.getenv("AEL_token"),
            export_data_access_groups = TRUE, #guess_max = 10000,
            guess_type = FALSE,
            raw_or_label = "label"
        )$data %>%
            mutate_at(vars(total_patients:sre), as.numeric)
    )

    MTr_data <- tbl_df(
        redcap_read_oneshot(
            redcap_uri = uri,
            token = Sys.getenv("MT_token"),
            export_data_access_groups = TRUE, #guess_max = 10000,
            guess_type = FALSE,
            raw_or_label = "label"
        )$data %>%
            mutate_at(vars(total_patients:sre), as.numeric)
    )

     redcap_data <-
        bind_rows(GAMUT_data, AIM_data, AEL_data, MTr_data)

     usethis::ui_done("Processing GAMUT data")
     
    metadata <-
        data.frame(key = c("GAMUT_date_loaded"),
                   value = Sys.time())


    mydata <- redcap_data %>%
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

    # ## Bedside STEMI Times
    # bedside_stemi <- plyr::ddply(mydata[mydata$stemi_cases > 0, c(1,2,39,40,41,42)], .(program_name, ID),
    #                        function(x) data.frame(
    #                            bedside_stemi.wavg=weighted.mean(x$mean_bedside_stemi, x$stemi_cases, na.rm=TRUE)
    #                        )
    # )

    mydata <- mydata %>% inner_join(ID.lookup, by = "program_name")

    monthly_data <-
        mydata %>%
        filter(redcap_event_name != "Initial") %>%
        droplevels() %>%
        filter(!is.na(total_patients)) %>%
        mutate(month = as.Date(paste("01", as.character(redcap_event_name)), format = "%d %b %Y")) %>%
        select(program_name, ID, month, redcap_data_access_group, total_patients:monthly_data_complete)




    usethis::ui_done("Updating monthly data")
    

## Send to MySQL
    conn <-  dbConnect(
        RMySQL::MySQL(),
        username = Sys.getenv("GAMUT_MYSQL_USERNAME"),
        password = Sys.getenv("GAMUT_MYSQL_PASSWORD"),
        host = Sys.getenv("GAMUT_MYSQL_HOST"),
        port = 3306,
        dbname = Sys.getenv("GAMUT_MYSQL_DBNAME")
    )


    dbWriteTable(
        conn,
        name = "metric_details",
        value = data.frame(metric_details),
        overwrite = TRUE
    )
    dbWriteTable(
        conn,
        name = "GAMUT_data",
        value = data.frame(GAMUT_data),
        overwrite = TRUE
    )
    dbWriteTable(
        conn,
        name = "AEL_data",
        value = data.frame(AEL_data),
        overwrite = TRUE
    )
    dbWriteTable(
        conn,
        name = "MTr_data",
        value = data.frame(MTr_data),
        overwrite = TRUE
    )
    dbWriteTable(
        conn,
        name = "redcap_data",
        value = data.frame(redcap_data),
        overwrite = TRUE
    )
    dbWriteTable(
        conn,
        name = "metadata",
        value = data.frame(metadata),
        overwrite = TRUE
    )
    dbWriteTable(
        conn,
        name = "monthly_data",
        value = data.frame(monthly_data),
        overwrite = TRUE
    )

    dbDisconnect(conn)

    usethis::ui_done("All done!")
    
}



