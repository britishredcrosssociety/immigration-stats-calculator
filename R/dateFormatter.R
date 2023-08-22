date_formatter <- function(d) {
  paste0(day(d), " ", month.name[month(d)], " ", year(d))
}
