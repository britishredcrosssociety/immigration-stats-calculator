#' Show a custom error message if the app has trouble downloading/processing data from the Home Office website.
#'
#' @param additional_msg Shiny/HTML tagged text to display in the error message.
#'
stats_error <- function(additional_msg) {
  div(
    p("I'm having trouble fetching these statistics from the Home Office. There could be a couple of reasons for this:"),
    tags$ul(
      tags$li("I can't connect to their website."),
      tags$li("Home Office has changed the format of the data."),
      tags$li("The files no longer exist or have changed their names.")
    ),

    additional_msg
  )
}
