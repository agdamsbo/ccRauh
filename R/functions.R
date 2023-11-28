#' Read data file
#'
#' @param path file path
#'
#' @return data frame
read_csv <- function(path) {
  read.csv(path)
}

data <- targets::tar_read(df_labmeeting)

#' Data frame to ical calendar
#'
#' @param data calendar data
#' @param path file path to export .ics file
#'
#' @return writes ics file
df2ical <- function(data, ics.path = here::here("ccrauh_labmeeting.ics")) {
  # Based on example from https://github.com/ATFutures/calendar/issues/36
  data |>
    dplyr::transmute(
      SUMMARY = paste0("Lab meeting (", ifelse(titel == "", "TBD", titel), ")"),
      DTSTART = lubridate::ymd_hms(paste(date, start), tz = "CET"),
      DTEND = lubridate::ymd_hms(paste(date, end), tz = "CET"),
      LOCATION = place,
      UID = replicate(nrow(data), calendar::ic_guid())
    ) |>
    calendar::ical() |>
    calendar::ic_write(file = ics.path)
}

#' Commit and push .ics calendar file
#'
#' @param ics.path 
#'
#' @return
ical_push <- function(ics.path) {
  git2r::add(path = ics.path)
  # Suppressing error if nothing to commit
  tryCatch(git2r::commit(message = "Updated calendar"), error=function(e){})
  git2r::push(
    name = "origin",
    refspec = "refs/heads/main",
    credentials = git2r::cred_ssh_key(),
    set_upstream = FALSE
  )
}
