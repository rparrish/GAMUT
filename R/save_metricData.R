#'
#' save_metricData
#'
#' get GAMUT data from REDCap, transform/aggregate by
#' individual program. Save as metricData to
#' GAMUT.Rdata file in the data folder
#'
#' @author Rollie Parrish
#' @export

save_metricData <- function() {


use.ID <- TRUE

## load data
uri <- "https://ampa.org/redcap/api/"
GAMUT_token <- "C212EB36C4A8347D35690465E350E6C3"
AIM_token <- "E17BB265BCA6A364356DE042C60DDC58"
AEL_token <- "EEA2ADCE64D53B28BDD2BA4432922B30"

GAMUT_data <- redcap_read_oneshot(redcap_uri=uri, token=GAMUT_token,
                           export_data_access_groups=TRUE,
                           raw_or_label = "label",
                           sslversion=NULL)$data

AIM_data <- redcap_read_oneshot(redcap_uri=uri, token=AIM_token,
                                  export_data_access_groups=TRUE,
                                  raw_or_label = "label",
                                  sslversion=NULL)$data

AEL_data <- redcap_read_oneshot(redcap_uri=uri, token=AEL_token,
                                  export_data_access_groups=TRUE,
                                  raw_or_label = "label",
                                  sslversion=NULL)$data

#raw <- read.csv("data/GAMUTDatabase_DATA_2014-10-27_1843.csv", stringsAsFactors=FALSE, nrows=1)
#redcap_data <- read.csv("data/GAMUTDatabase_DATA_LABELS_2014-10-27_1827.csv", stringsAsFactors=FALSE)

redcap_data <- rbind(GAMUT_data, AIM_data, AEL_data)

names(redcap_data) <- names(raw)

redcap_data$program_name <- as.factor(redcap_data$program_name)
redcap_data$redcap_event_name <- as.factor(redcap_data$redcap_event_name)
redcap_data$redcap_data_access_group <- as.factor(redcap_data$redcap_data_access_group)

mydata <- redcap_data


#mydata <- read.csv("../Data/SOTMPediatricQuality_raw.csv")
# remove demographics rows
program_info <- mydata[mydata$redcap_event_name == "Initial",1:10]

mydata <- mydata[mydata$redcap_event_name != "Demographics",]
mydata <- mydata[mydata$redcap_event_name != "Initial",]


## complete data only
mydata <- mydata[mydata$monthly_data_complete == "Complete", ]


#mydata$total_patients <- mydata$total_neo_patients + mydata$total_peds_patients + mydata$total_adult_patients

#mydata$total_runs <- mydata$total_neo_runs + mydata$total_peds_runs

ID.lookup <- data.frame(
    program_name = levels(mydata$program_name)
    , ID = anonymize(as.factor(mydata$program_name))
)

ID.lookup <- merge(ID.lookup, program_info[,c("program_name", "dm_email")], by="program_name")

mydata <- merge(mydata, ID.lookup, by=c("program_name"))

mydata <- mydata[,c(1,72,12:70)]


#metricData <- aggregate( . ~ redcap_data_access_group + ID  , FUN=sum, data=mydata[c(1,3:23,48,49,50)])
metricData <- aggregate( . ~ program_name + ID,
                        FUN=sum,
                        na.rm=TRUE, na.action=NULL,
                        data=mydata
)

ID.lookup <- merge(ID.lookup, metricData[2:3], by="ID", all.x=TRUE)

#timeData <- ddply(mydata[, c(1, 3,20,49,50)], .(redcap_data_access_group, ID),
mobilization <- ddply(mydata[mydata$mean_mobilization > 0,
                             c(1,2,3,39,40,41)], .(program_name, ID),
                      function(x) data.frame(
                          mobilization.wavg=weighted.mean(x$mean_mobilization, x$total_patients, na.rm=TRUE)
                      )
)

bedside_stemi <- ddply(mydata[mydata$stemi_cases > 0, c(1,2,39,40,41,42)], .(program_name, ID),
                       function(x) data.frame(
                           bedside_stemi.wavg=weighted.mean(x$mean_bedside_stemi, x$stemi_cases, na.rm=TRUE)
                       )
)
bedside_stemi <- bedside_stemi[bedside_stemi$bedside_stemi.wavg >= 0, ]

scene_stemi <- ddply(mydata[mydata$stemi_cases > 0, c(1,2,39,40,41,42)], .(program_name, ID),
                     function(x) data.frame(
                         scene_stemi.wavg=weighted.mean(x$mean_scene_stemi, x$stemi_case, na.rm=TRUE)
                     )
)
scene_stemi <- scene_stemi[scene_stemi$scene_stemi.wavg >= 0, ]




if (use.ID == TRUE) {
    metricData$group <- metricData$ID
    mobilization$group <- mobilization$ID
    bedside_stemi$group <- bedside_stemi$ID
    scene_stemi$group <- scene_stemi$ID
} else {
    metricData$group <- metricData$program_name
    mobilization$group <- mobilization$program_name
    bedside_stemi$group <- bedside_stemi$program_name
    scene_stemi$group <- scene_stemi$program_name
}

GAMUT_date_loaded <- date()


save(mydata, metricData, ID.lookup, GAMUT_date_loaded,
     mobilization, scene_stemi, bedside_stemi,
     program_info,
     file="data/GAMUT.Rdata")

}
