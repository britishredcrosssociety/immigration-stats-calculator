options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {

  # observeEvent(input$topicChoice, {
  #
  # })
  #

  output$instructions <- renderUI({
    if (input$topicChoice == "channel") {
      div(
        p("1. Go to ", a("https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables", href = "https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables")),
        p("2. Click the 'Detailed datasets' link to download the latest statistics.")
      )
    } else if (input$topicChoice == "grants") {
      url <- "https://www.gov.uk/government/statistical-data-sets/asylum-and-resettlement-datasets#asylum-applications-decisions-and-resettlement"

      div(
        p("1. Go to ", a(url, href = url)),
        p("2. In the 'Asylum applications, decisions and resettlement' section, click the 'Asylum applications, initial decisions and resettlement' link to download the most recent data.")
      )
    }
  })

  output$results <- renderUI({
    req(input$file1)

    output_data <- NULL

    if (input$topicChoice == "channel") {
      output_data <- calc_irregular_migration()
    } else if (input$topicChoice == "grants") {
      output_data <- calc_grant_rates()
    }

    # lapply(1:ncol(output_data), function(i) {
    #   p(paste(names(output_data)[i], output_data[,i], sep = ": "))
    # })

    output_data
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
        `Number of small boat arrivals in most recent quarter` = scales::comma(small_boat_arrivals_last_quarter),
        `Number of small boat arrivals over last 12 months` = scales::comma(small_boat_arrivals_last_12_months),
        `% change in number of people crossing - compared to previous year to date` = percent_change
      )

    return(channel_data)
  })

  # ---- Grant rates stats ----
  calc_grant_rates <- reactive({
    decisions_resettlement <-
      read_excel(input$file1$datapath, sheet = "Data - Asy_D02", skip = 1)

    # Wrangling
    decisions_resettlement <-
      decisions_resettlement |>
      # mutate(Date = yq(Quarter)) |>
      mutate(Date = zoo::as.Date(as.yearqtr(Quarter, format = "%Y Q%q"), frac = 1)) |>
      relocate(Date) |>

      mutate(
        `Applicant type` = if_else(`Applicant type` == "Dependant", "Dependant", "Main applicant"),
        `Case outcome` = case_when(
          `Case outcome` %in% c("Non-substantiated withdrawal", "Non-Substantiated Withdrawal") ~ "Non-Substantiated Withdrawal",
          `Case outcome` %in% c("Other refusals", "Other Refusals") ~ "Other Refusals",
          TRUE ~ `Case outcome`
        )
      ) |>

      drop_na()

    # Grants, refusals, withdrawals, and grant rates for most recent quarter
    recent_quarter <-
      decisions_resettlement |>
      filter(Date == max(Date)) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    recent_quarter_decisions <- recent_quarter$Grant + recent_quarter$Refused
    recent_quarter_grant_rate <- recent_quarter$`Initial grant rate`
    recent_quarter_refusals <- recent_quarter$Refused / (recent_quarter$Grant + recent_quarter$Refused)

    # Grants, refusals, withdrawals, and grant rates over last 12 months
    recent_year <-
      decisions_resettlement |>
      filter(Date >= max(Date) - dmonths(11)) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    recent_year_decisions <- recent_year$Grant + recent_year$Refused
    recent_year_grant_rate <- recent_year$`Initial grant rate`
    recent_year_refusals <- recent_year$Refused / (recent_year$Grant + recent_year$Refused)

    # Grants, refusals, withdrawals, and grant rates over the 12 months prior (to the most recent year)
    prior_year <-
      decisions_resettlement |>
      filter((Date >= max(Date) - dmonths(22)) & (Date <= max(Date) - dmonths(11))) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    withdrawals_change <- (recent_year$Withdrawn - prior_year$Withdrawn) / prior_year$Withdrawn

    # Calculate date ranges
    date_recent_quarter <- max(decisions_resettlement$Date)
    date_recent_quarter_txt <- date_formatter(date_recent_quarter)

    date_prior_year <-
      decisions_resettlement |>
      filter((Date >= max(Date) - dmonths(22)) & (Date <= max(Date) - dmonths(11))) |>
      distinct(Date)
    date_prior_year_txt <- date_formatter(max(date_prior_year$Date))

    # grants_data <-
    #   tibble(
    #     `Number of initial decisions made over last quarter (not including resettlement)` = scales::comma(recent_quarter_decisions),
    #     `Initial grant rate during last quarter` = scales::percent(recent_year_grant_rate, accuracy = 0.1),
    #     `Percentage of refusals in most recent quarter (compared to total initial decisions)` = scales::percent(recent_quarter_refusals, accuracy = 0.1),
    #
    #     `Number of initial decisions made over the last 12 months (not including resettlement)` = scales::comma(recent_year_decisions),
    #     `Initial grant rate over the last 12 months` = scales::percent(recent_year_grant_rate, accuracy = 0.1),
    #     `Percentage of refusals over the last 12 months (compared to total initial decisions)` = scales::percent(recent_year_refusals, accuracy = 0.1),
    #
    #     `Number of withdrawals in last quarter` = scales::comma(recent_quarter$Withdrawn),
    #     `Number of withdrawals over the last 12 months` = scales::comma(recent_year$Withdrawn),
    #     `Number of withdrawals in the prior year` = scales::comma(prior_year$Withdrawn),
    #     `Percentage change in withdrawals` = scales::percent(withdrawals_change, accuracy = 0.1)
    #   )

    html_output <-
      div(
        p("Number of initial decisions made over last quarter, as of", date_recent_quarter_txt, ": ", scales::comma(recent_quarter_decisions)),
        p("Initial grant rate during last quarter, as of", date_recent_quarter_txt, ": ", scales::percent(recent_quarter_grant_rate, accuracy = 0.1)),
        p("Percentage of refusals (compared to total initial decisions), as of", date_recent_quarter_txt, ": ", scales::percent(recent_quarter_refusals, accuracy = 0.1)),
        p(),
        p("Number of initial decisions made over the last 12 months (year ending", date_recent_quarter_txt, "): ", scales::comma(recent_year_decisions)),
        p("Initial grant rate over the last 12 months (year ending", date_recent_quarter_txt, "): ", scales::percent(recent_year_grant_rate, accuracy = 0.1)),
        p("Percentage of refusals over the last 12 months (year ending", date_recent_quarter_txt, "): ", scales::percent(recent_year_refusals, accuracy = 0.1)),
        p(),
        p("Number of withdrawals in last quarter, as of", date_recent_quarter_txt, ": ", scales::comma(recent_quarter$Withdrawn)),
        p("Number of withdrawals over the last 12 months (year ending", date_recent_quarter_txt, "): ", scales::comma(recent_year$Withdrawn)),
        p("Number of withdrawals in the prior year (year ending", date_prior_year_txt, "): ", scales::comma(prior_year$Withdrawn)),
        p("Percentage change in withdrawals (year ending", date_prior_year_txt, " to year ending ", date_recent_quarter_txt, "): ", scales::percent(withdrawals_change, accuracy = 0.1)),
        p(""),
        p("Initial decisions referrs to grants and refusals for main applicants only; withdrawals do not count as decisions. Figures do not include resettlement.")
      )

    # return(grants_data)
    return(html_output)
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
