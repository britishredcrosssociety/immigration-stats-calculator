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
    p("3. Click the 'Browse...' button below and choose the file you just got from the Home Office website."),
    p("4. The results will appear below; copy and paste them into your briefing document or click the 'Download statistics in a spreadsheet' if you want to save a copy to your computer."),

    fileInput(
      "file1", "Upload file from Home Office",
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv", ".xls", ".xlsx", ".ods")
    ),

    uiOutput("results") |> withSpinner(color = "#ee2a24", size = 2),

    downloadButton("downloadData", "Download statistics in a spreadsheet")
  )
}
