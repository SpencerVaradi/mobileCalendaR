# from https://gist.github.com/etiennebr/83b1b2f8433c3303a01a9c354c9904cb

first_day_in_month = function(x) {
  day(x) = 1
  hour(x) = 0
  minute(x) = 0
  second(x) = 0
  return(x)
}

first_day_in_year = function(x) {
  month(x) = 1
  return(first_day_in_month(x))
}

week0 = function(x, origin = first_day_in_year(x)) {
  week(x - days(wday(origin - 1)))
}

weekrow <- function(dt) {
  # Reads a date object and returns weekrow
  # where weekrow starts at 1
  year <- year(dt)
  month <- month(dt)
  day <- day(dt)
  wday_first <- wday(ymd(paste(year, month, 1), quiet = TRUE))
  offset <- 7 + (wday_first - 2)
  weekrow <- ((day + offset) %/% 7) - 1
  return(weekrow)
}

#' Create a calendar list
#' @param begin day to select month from (default now())
#' @param length number of days for the calendar (default days_in_month())
#' @return a data.frame with day, week, week day, julian day, month, year
#' @examples
#' cal = calendar()
#' qplot(lubridate::wday(date, label=TRUE),
#'      weekrow(date), label=day, data = cal, geom = c("tile", "text"),
#'      color = I("lightgrey")) +
#'   scale_y_reverse()
calendar = function(begin = first_day_in_month(now()), length = days(days_in_month(begin) - 1)) {
  dates = seq(begin, begin + length, by = "1 day")

  cal <- data.frame(
    day = day(dates),
    week = week0(dates),
    wday = wday(dates),
    doy = yday(dates),
    month = month(dates),
    year = year(dates),
    date = dates
  )

  return(cal)
}