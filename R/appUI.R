ui <- function() {
  fluidPage(
    # theme = shinytheme("flatly"),  # Use a Bootstrap theme for a nicer look
    includeCSS("inst/www/styles.css"),

    titlePanel("Home Office Immigration Stats Helper"),

    tags$p(
      tags$span(class = "phase-banner", "BETA"),
      "This is work in progress. New stats will be added soon."
    ),

    tags$hr(),  # horizontal line for separation

    h2("Choose a topic"),
    radioButtons(
      "topicChoice",
      label = NULL,
      choices = list(
        "Asylum backlog" = "backlog",
        "Asylum support" = "support",
        "Channel crossings" = "channel",
        "Family reunion" = "reunion",
        "Grant rates" = "grants"
      ),
      selected = character(0),
      inline = TRUE  # Display the choices in a row
    ),

    # conditionalPanel(
    #   condition = "['grants', 'channel', 'backlog'].includes(input.topicChoice)",
    #
    #   tags$br(),  # Add a bit of spacing
    #   h2("2. Download data from the Home Office"),
    #   uiOutput("instructions"),
    #
    #   tags$br(),  # Add a bit of spacing
    #   h2("3. Upload Home Office data"),
    #   wellPanel(  # Use a well for visual emphasis
    #     tags$p("Click the 'Upload' button and choose the file you just downloaded from the Home Office website."),
    #     fileInput(
    #       "file1", "Upload",
    #       multiple = FALSE,
    #       accept = c(
    #         "text/csv",
    #         "text/comma-separated-values,text/plain",
    #         ".csv", ".xls", ".xlsx", ".ods"
    #       ),
    #       buttonLabel = "Browse...",  # Custom label for file input button
    #       width = "100%"
    #     )
    #   ),
    #
    #
    # )

    tags$br(),  # Add a bit of spacing
    uiOutput("results") |> withSpinner(color = "#ee2a24", size = 2),

    # Show a select box for nationalities if the user is looking at grant rates
    # or family reunion stats
    conditionalPanel(
      condition = "['grants', 'reunion'].includes(input.topicChoice)",

      # tags$br(),  # Add a bit of spacing
      selectizeInput(
        "selected_nationalities",
        "Filter specific nationalities",
        choices = nationalities,
        selected = c("Afghanistan", "Iran", "Eritrea", "Sudan", "Syria"),
        multiple = TRUE
      )
    ),

    # Grant rate trends graph
    conditionalPanel(
      condition = "input.topicChoice === 'grants'",

      plotlyOutput("grant_trends")
    )
  )
}
