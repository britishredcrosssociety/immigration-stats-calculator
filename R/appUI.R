ui <- function() {
  fluidPage(
    titlePanel("Home Office immigration stats helper"),

    radioButtons(
      "topicChoice",
      "Select which topic you'd like to calculate statistics for",
      choices = list("Channel crossings" = "channel", "Grant rates" = "grants"),
      selected = NULL
    ),

    uiOutput("instructions"),

    fileInput(
      "file1", "Upload file from Home Office",
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv", ".xls", ".xlsx", ".ods")
    ),

    downloadButton("downloadData", "Calculate and download statistics")
  )
}
