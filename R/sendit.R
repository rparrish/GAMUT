#' sendit.R
#'
#' Wrapper function for httr that sends PDF report to a list of recipients.
#' Requires REDCap Sendit API module and a valid REDCap user account.
#'
#' @param recipients a comma-separated list of email addresses
#' @param filename the file to be sent
#' @param subject email subject
#' @param message email message
#' @param expireDays number of days that the file will be available. Default is 7.
#' @param emailFrom index number the sending email address for
#' the specified REDCapR username. Default is 1 (first one)
#' @param url address of the SenditAPI module
#' @param username
#' @param password
#'
#'
#' @author Rollie Parrish
#' @export
#' @import httr


sendit <- function(recipients, filename, subject, message,
                   expireDays=15, emailFrom=1, url="http://httpbin.org/post",
                   username, password) {

    body <- list(username = username,
                 password = password,
                 id = 0,
                 emailFrom = 1,
                 recipients = recipients,
                 subject = subject,
                 message = message,
                 expireDays = expireDays,
                 file = upload_file(filename),
                 submit = "Send+It%21"
    )
    response <- POST(url = url, body = body, encode = "multipart")

    results <- content(response, as = "text")
    results

}
