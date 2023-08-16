server <- function(input, output, session) {

  # observeEvent(input$topicChoice, {
  #
  # })
  #

  output$instructions <- renderUI({
    if (input$topicChoice == "channel") {
      div(
        p("1. Go to ", a("https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables", href = "https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables")),
        p("2. Click the 'Detailed datasets' link to download the latest statistics."),
        p("3. Click the 'Browse...' button below and choose the file you just got from the Home Office website."),
        p("4. Click 'Calculate and download statistics' to get your summary for Channel Crossings")
      )
    } else if (input$topicChoice == "grants") {
      p("Grant rate instructiosn go here")
    }
  })

  # ---- Channel crossings stats ----
  calc_irregular_migration <- reactive({
    irregular_migration <-
      read_excel(input$file1$datapath, sheet = "Data - Irr_D01", skip = 1)

    # Wrangling
    irregular_migration <-
      irregular_migration |>
      mutate(Date = yq(Quarter)) |>
      # mutate(Date = as.Date(as.yearqtr(Quarter, format = "%Y Q%q"), frac = 1)) |>
      mutate(Quarter = quarter(Date)) |>
      relocate(Date) |>
      drop_na()

    # Number of small boat arrivals in most recent quarter
    small_boat_arrivals_last_quarter <-
      irregular_migration |>
      filter(Date == max(Date)) |>
      filter(`Method of entry` == "Small boat arrivals") |>
      summarise(Total = sum(`Number of detections`)) |>
      pull(Total)

    # Number of small boat arrivals over last 12 months
    small_boat_arrivals_last_12_months <-
      irregular_migration |>
      filter(Date >= max(Date) - dmonths(11)) |>
      filter(`Method of entry` == "Small boat arrivals") |>
      summarise(Total = sum(`Number of detections`)) |>
      pull(Total)

    # % change in number of people crossing - compared to previous year to date
    small_boat_arrivals_year_before <-
      irregular_migration |>
      filter((Date >= max(Date) - dmonths(22)) & (Date <= max(Date) - dmonths(11))) |>
      filter(`Method of entry` == "Small boat arrivals") |>
      summarise(Total = sum(`Number of detections`)) |>
      pull(Total)

    percent_change <- scales::percent((small_boat_arrivals_last_12_months - small_boat_arrivals_year_before) / small_boat_arrivals_year_before, accuracy = 0.1)

    channel_data <-
      tibble(
        `Number of small boat arrivals in most recent quarter` = small_boat_arrivals_last_quarter,
        `Number of small boat arrivals over last 12 months` = small_boat_arrivals_last_12_months,
        `% change in number of people crossing - compared to previous year to date` = percent_change
      )

    return(channel_data)
  })

  # ---- Calculate and download statistics ----
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$topicChoice == "channel") {
        paste("channel-crossings", ".csv", sep = "")
      } else if (input$topicChoice == "grants") {
        paste("grants", ".csv", sep = "")
      }
    },

    content = function(file) {

      output_data <- NULL

      if (input$topicChoice == "channel") {
        output_data <- calc_irregular_migration()
      }

      write_csv(output_data, file)
    }
  )

}
