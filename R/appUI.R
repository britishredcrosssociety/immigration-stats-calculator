ui <- function() {
  fluidPage(
    titlePanel("Home Office immigration stats helper"),

    radioButtons(
      "topicChoice",
      "Select the topic you'd like to calculate statistics for",
      choices = list("Channel crossings" = "channel", "Grant rates" = "grants", "Asylum backlog" = "backlog"),
      selected = character(0)
    ),

    conditionalPanel(
      condition = "['grants', 'channel', 'backlog'].includes(input.topicChoice)",

      uiOutput("instructions"),
      p("3. Click the 'Browse...' button below and choose the file you just got from the Home Office website."),
      p("4. The results will appear below. It may take a few moments to do the calculations. You can copy and paste the results into your briefing document."),

      fileInput(
        "file1", "Upload file from Home Office",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv", ".xls", ".xlsx", ".ods")
      ),

      uiOutput("results") |> withSpinner(color = "#ee2a24", size = 2)
    )

    # downloadButton("downloadData", "Download statistics in a spreadsheet")
  )
}
