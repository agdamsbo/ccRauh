#' Read data file
#'
#' @param path file path
#'
#' @return data frame
read_csv <- function(path) {
  # readr::read_csv(path)
  read.csv(path)
}

#' Converts time character string to H:M character string
#'
#' @param time data
#'
#' @return
time2hm <- function(time) {
  as.POSIXlt(hms::as_hms(time)) |>
    format("%H:%M") |>
    as.character()
}

#' Cleans up the lab meeting schedule for printing
#'
#' @param data lab meetings
#'
#' @return
clean_meetings <- function(data) {
  data |>
    dplyr::filter(date>Sys.Date()) |> 
    dplyr::transmute(
      Date = format(as.POSIXct(date),"%d.%b.%Y"),
      Time = glue::glue("{time2hm(start)} - {time2hm(end)}"),
      Subject = ifelse(titel == "", "Emne følger", titel),
      Location = place
    ) 
}


#' Get indexed recuring meeting dates
#'
#' @param day day of the week. Default is "Monday"
#' @param index monthly occurance. Default is 1.
#' @param start Date to start counting. Default is Sys.Date()
#' @param stop Date to stop. Defaults to one year after start.
#' @param only.work logical to only include work days. Default to TRUE
#' @param work.calendar Calendar to use for workdays. Passed to 
#' `RQuantLib::isBusinessDay()`
#'
#' @return
#' @export
#'
#' @examples
#' 
#' indexed_meet()
indexed_meet <- function(day = "Monday", index = 1, start = Sys.Date(), stop=NULL, only.work=TRUE, work.calendar="Denmark") {
if (is.null(stop)) stop <- as.Date(start + lubridate::dmonths(12))
  
  ## Counts from the start of the month from the starting data
  ## Will only include the relevant 
  seq.start <- start-(lubridate::day(start)-1)
  
  dates <- seq(from=seq.start, to=stop, by = "days")
  
  if (only.work){
  dates <- dates[RQuantLib::isBusinessDay(calendar="Denmark", dates=dates)]
  }
  
  df <- tibble::tibble(
    dates = dates,
    days = weekdays(dates),
    ym = format(dates, "%Y%m")
  ) |>
    dplyr::filter(days %in% day) |>
    dplyr::group_by(ym, days) |>
    dplyr::mutate(rank = rank(dates)) |>
    dplyr::filter(rank %in% index,
                  dates > start) |>
    dplyr::ungroup()
  
  
  df[["dates"]]
}

#' Creates data.frame of upcoming meetings.  Content to be filled manually.
#'
#' @param t.start time to start
#' @param t.end time to end
#' @param ... passed on to indexed_meet()
#'
#' @return
#' @export
#'
#' @examples
next_meetings <- function(t.start = "09:00:00", t.end = "10:00:00", ...) {
  data.frame(
    date = as.character(indexed_meet(...)),
    place = "",
    titel = "",
    start = t.start,
    end = t.end
  )
}

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
  tryCatch(git2r::commit(message = "Updated calendar"), error = function(e) {})
  git2r::push(
    name = "origin",
    refspec = "refs/heads/main",
    credentials = git2r::cred_ssh_key(),
    set_upstream = FALSE
  )
}
