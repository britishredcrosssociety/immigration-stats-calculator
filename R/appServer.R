options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {

  ho_file <- reactiveVal()

  observeEvent(input$topicChoice, {
    ho_file(NULL)
  })

  observeEvent(input$file1, {
    ho_file(input$file1$datapath)
  })

  output$instructions <- renderUI({
    if(is.null(input$topicChoice)) return(NULL)

    url <- NULL
    second_instruction <- NULL

    if (input$topicChoice == "channel") {
      url <- "https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables#detailed-datasets"
      second_instruction <- "2. Click the 'Detailed datasets' link to download the latest statistics."

    } else if (input$topicChoice == "grants") {
      url <- "https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum-and-resettlement"
      second_instruction <- "2. In the 'Asylum applications, decisions and resettlement' section, click the 'Asylum applications, initial decisions and resettlement' link to download the most recent data."

    } else if (input$topicChoice == "backlog") {
      url <- "https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum-and-resettlement"
      second_instruction <- "2. In the 'Asylum applications, decisions and resettlement' section, click the 'Asylum applications awaiting a decision' link to download the most recent data."

    } else if (input$topicChoice == "sap") {
      url <- "https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum-and-resettlement"
      second_instruction <- "2. In the 'Asylum applications, decisions and resettlement' section, click the 'Asylum applications, initial decisions and resettlement' link to download the most recent data."
    }

    div(
      p("1. Go to ", a(url, href = url, target = "_blank")),
      p(second_instruction)
    )
  })

  output$results <- renderUI({
    # print(input$file1)
    # if (is.null(ho_file())) return(h2("4. Results will appear here"))

    output_data <- NULL

    if (length(input$topicChoice) > 0) {
      if (input$topicChoice == "channel") {
        output_data <- calc_irregular_migration()

      } else if (input$topicChoice == "grants") {
        output_data <- calc_grant_rates()

      } else if (input$topicChoice == "backlog") {
        output_data <- calc_backlog()

      } else if (input$topicChoice == "sap") {
        output_data <- calc_SAP()

      } else if (input$topicChoice == "support") {
        output_data <- calc_support()

      }

      # lapply(1:ncol(output_data), function(i) {
      #   p(paste(names(output_data)[i], output_data[,i], sep = ": "))
      # })
      div(
        h2("Results"),
        output_data
      )
    }
  })

  # ---- Channel crossings stats ----
  calc_irregular_migration <- reactive({
    data_file <- download_stats("https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables", "Detailed datasets")

    irregular_migration <-
      read_excel(data_file, sheet = "Data - Irr_D01", skip = 1)

    # DEBUG:
    # irregular_migration <-
    #   read_excel("c:/users/040026704/Downloads/irregular-migration-to-the-UK-data-tables-year-ending-june-2023.xlsx", sheet = "Data - Irr_D01", skip = 1)

    # Wrangling
    irregular_migration <-
      irregular_migration |>
      # mutate(Date = yq(Quarter)) |>
      mutate(Date = zoo::as.Date(as.yearqtr(Quarter, format = "%Y Q%q"), frac = 1)) |>
      mutate(Quarter = quarter(Date)) |>
      relocate(Date) |>
      drop_na()

    # Number of small boat arrivals, year to date
    small_boat_arrivals_ytd <-
      irregular_migration |>
      filter(Year == max(Year)) |>
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

    # Top five nationalities crossing, year to date
    nationalities_last_quarter <-
      irregular_migration |>
      filter(Year == max(Year)) |>
      filter(`Method of entry` == "Small boat arrivals") |>
      group_by(Nationality) |>
      summarise(Detections = sum(`Number of detections`)) |>
      ungroup() |>
      slice_max(Detections, n = 5) |>
      pull(Nationality)

    nationalities_last_quarter <- paste(nationalities_last_quarter, collapse = ", ")

    # Top five nationalities crossing in last 12 months
    nationalities_last_year <-
      irregular_migration |>
      filter(Date >= max(Date) - dmonths(11)) |>
      filter(`Method of entry` == "Small boat arrivals") |>
      group_by(Nationality) |>
      summarise(Detections = sum(`Number of detections`)) |>
      ungroup() |>
      slice_max(Detections, n = 5) |>
      pull(Nationality)

    nationalities_last_year <- paste(nationalities_last_year, collapse = ", ")

    # Calculate date ranges
    date_recent_quarter <- max(irregular_migration$Date)
    date_recent_quarter_txt <- date_formatter(date_recent_quarter)

    date_prior_year <-
      irregular_migration |>
      filter((Date >= max(Date) - dmonths(22)) & (Date <= max(Date) - dmonths(11))) |>
      distinct(Date)
    date_prior_year_txt <- date_formatter(max(date_prior_year$Date))

    # channel_data <-
    #   tibble(
    #     `Number of small boat arrivals in most recent quarter` = scales::comma(small_boat_arrivals_ytd),
    #     `Number of small boat arrivals over last 12 months` = scales::comma(small_boat_arrivals_last_12_months),
    #     `% change in number of people crossing - compared to previous year to date` = percent_change
    #   )

    channel_data <-
      div(
        p(tags$b("Number of small boat arrivals, year to date:"), scales::comma(small_boat_arrivals_ytd)),
        p(tags$b("Number of small boat arrivals over last 12 months (year ending", date_recent_quarter_txt, "): "), scales::comma(small_boat_arrivals_last_12_months)),
        p(tags$b("Number of small boat arrivals over the 12 months prior (year ending", date_prior_year_txt, "): "), scales::comma(small_boat_arrivals_year_before)),
        p(tags$b("% change in number of people crossing (year ending", date_prior_year_txt, " to year ending ", date_recent_quarter_txt, "): "), percent_change),
        p(tags$b("Top five nationalities arriving via small boats, year to date:"), nationalities_last_quarter),
        p(tags$b("Top five nationalities arriving via small boats over the last 12 months (year ending", date_recent_quarter_txt, "): "), nationalities_last_year)
      )

    return(channel_data)
  })

  # ---- Grant rates stats ----
  calc_grant_rates <- reactive({
    data_file <- download_stats("https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables", "Asylum applications, initial decisions and resettlement detailed datasets")

    decisions_resettlement <-
      read_excel(data_file, sheet = "Data - Asy_D02", skip = 1)

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
      filter(`Case type` == "Asylum Case") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`, `Applicant type`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    recent_quarter_people_withdrawing <- sum(recent_quarter$Withdrawn)

    recent_quarter <-
      recent_quarter |>
      filter(`Applicant type` == "Main applicant")

    recent_quarter_decisions <- recent_quarter$Grant + recent_quarter$Refused
    recent_quarter_grant_rate <- recent_quarter$`Initial grant rate`
    recent_quarter_refusals <- recent_quarter$Refused / (recent_quarter$Grant + recent_quarter$Refused)
    recent_quarter_withdrawals_proportion <- recent_quarter$Withdrawn / (recent_quarter$Grant + recent_quarter$Refused + recent_quarter$Withdrawn)

    # Grants, refusals, withdrawals, and grant rates over last 12 months
    recent_year <-
      decisions_resettlement |>
      filter(Date >= max(Date) - dmonths(11)) |>
      filter(`Case type` == "Asylum Case") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`, `Applicant type`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    recent_year_people_withdrawing <- sum(recent_year$Withdrawn)

    recent_year <-
      recent_year |>
      filter(`Applicant type` == "Main applicant")

    recent_year_decisions <- recent_year$Grant + recent_year$Refused
    recent_year_grant_rate <- recent_year$`Initial grant rate`
    recent_year_refusals <- recent_year$Refused / (recent_year$Grant + recent_year$Refused)
    recent_year_withdrawals_proportion <- recent_year$Withdrawn / (recent_year$Grant + recent_year$Refused + recent_year$Withdrawn)

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

    # Top five nationalities receiving initial decisions (grants and refusals) in last year
    nationalities_last_year <-
      decisions_resettlement |>
      filter(Date >= max(Date) - dmonths(11)) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>
      filter(`Case outcome group` %in% c("Grant", "Refused")) |>

      group_by(Nationality) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>
      slice_max(Decisions, n = 5) |>
      pull(Nationality)

    nationalities_last_year <- paste(nationalities_last_year, collapse = ", ")

    # Top five nationalities granted status in last year
    nationalities_granted_last_year <-
      decisions_resettlement |>
      filter(Date >= max(Date) - dmonths(11)) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>
      filter(`Case outcome group` %in% c("Grant")) |>

      group_by(Nationality) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>
      slice_max(Decisions, n = 5) |>
      pull(Nationality)

    nationalities_granted_last_year <- paste(nationalities_granted_last_year, collapse = ", ")

    # Calculate date ranges
    date_recent_quarter <- max(decisions_resettlement$Date)
    date_recent_quarter_txt <- date_formatter(date_recent_quarter)

    date_prior_year <-
      decisions_resettlement |>
      filter((Date >= max(Date) - dmonths(22)) & (Date <= max(Date) - dmonths(11))) |>
      distinct(Date)
    date_prior_year_txt <- date_formatter(max(date_prior_year$Date))

    html_output <-
      div(
        p(tags$b("Number of initial decisions made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_decisions)),
        p(tags$b("Initial grant rate during last quarter, as of", date_recent_quarter_txt, ": "), scales::percent(recent_quarter_grant_rate, accuracy = 0.1)),
        p(tags$b("Percentage of refusals (compared to total initial decisions), as of", date_recent_quarter_txt, ": "), scales::percent(recent_quarter_refusals, accuracy = 0.1)),
        p(),
        p(tags$b("Number of initial decisions made over the last 12 months (year ending", date_recent_quarter_txt, "): "), scales::comma(recent_year_decisions)),
        p(tags$b("Initial grant rate over the last 12 months (year ending", date_recent_quarter_txt, "): "), scales::percent(recent_year_grant_rate, accuracy = 0.1)),
        p(tags$b("Percentage of refusals over the last 12 months (year ending", date_recent_quarter_txt, "): "), scales::percent(recent_year_refusals, accuracy = 0.1)),

        br(),
        h4("Withdrawals"),
        p(tags$b("Number of claims withdrawn in last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter$Withdrawn)),
        p(tags$b("Number of people withdrawing in last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_people_withdrawing)),
        p(tags$b("Proportion of withdrawals, compared to total grants + refusals + withdrawals in last quarter, as of", date_recent_quarter_txt, ": "), scales::percent(recent_quarter_withdrawals_proportion, accuracy = 0.1)),
        p(),
        p(tags$b("Number of claims withdrawn over the last 12 months (year ending", date_recent_quarter_txt, "): "), scales::comma(recent_year$Withdrawn)),
        p(tags$b("Number of people withdrawing over the last 12 months (year ending", date_recent_quarter_txt, "): "), scales::comma(recent_year_people_withdrawing)),
        p(tags$b("Proportion of withdrawals, compared to total grants + refusals + withdrawals (year ending", date_recent_quarter_txt, "): "), scales::percent(recent_year_withdrawals_proportion, accuracy = 0.1)),
        p(),
        p(tags$b("Number of claims withdrawn in the prior year (year ending", date_prior_year_txt, "): "), scales::comma(prior_year$Withdrawn)),
        p(tags$b("Percentage change in withdrawals (year ending", date_prior_year_txt, " to year ending ", date_recent_quarter_txt, "): "), scales::percent(withdrawals_change, accuracy = 0.1)),

        br(),
        h4("Nationalities"),
        p(tags$b("Top five nationalities receiving initial decisions (grants and refusals) over the last 12 months (year ending", date_recent_quarter_txt, "): "), nationalities_last_year),
        p(tags$b("Top five nationalities granted status over the last 12 months (year ending", date_recent_quarter_txt, "): "), nationalities_granted_last_year),
        p(),
        p("Initial decisions referrs to grants and refusals for main applicants only; withdrawals do not count as decisions. Figures do not include resettlement.")
      )

    # return(grants_data)
    return(html_output)
  })

  # ---- Backlog stats ----
  calc_backlog <- reactive({
    data_file <- download_stats("https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables", "Asylum applications awaiting a decision detailed datasets")

    awaiting_decision <-
      read_excel(data_file, sheet = "Data - Asy_D03", skip = 1)

    data_file <- download_stats("https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables", "Asylum applications, initial decisions and resettlement detailed datasets")

    applications <-
      read_excel(data_file, sheet = "Data - Asy_D01", skip = 1)

    # DEBUG:
    # awaiting_decision <-
    #   read_excel("C:\\Users/040026704/Downloads/asylum-applications-awaiting-decision-datasets-jun-2023.xlsx", sheet = "Data - Asy_D03", skip = 1)

    # Wrangling
    awaiting_decision <-
      awaiting_decision |>
      rename_with(~"Date", starts_with("Date")) |>
      # filter(Date != "End of table") |>
      filter(toupper(Date) != "END OF TABLE") |>
      mutate(Date = dmy(Date)) |>

      mutate(
        `Applicant type` = if_else(`Applicant type` == "Dependant", "Dependant", "Main applicant")
      )

    applications <-
      applications |>
      mutate(Date = zoo::as.Date(as.yearqtr(Quarter, format = "%Y Q%q"), frac = 1)) |>
      relocate(Date) |>
      mutate(`Applicant type` = if_else(`Applicant type` == "Dependant", "Dependant", "Main applicant")) |>
      drop_na()

    # Number of people claiming asylum in the last 12 months
    people_claiming_asylum <-
      applications |>
      filter(Date >= max(Date) - dmonths(11)) |>
      summarise(Applications = sum(Applications)) |>
      pull(Applications)

    # Number of people waiting for an initial decision, as of current year-end
    backlog_total <-
      awaiting_decision |>
      filter(Date == max(Date)) |>
      summarise(Backlog = sum(Applications)) |>
      pull(Backlog)

    # % change in people waiting for initial decisions, quarter-on-quarter
    backlog_change <-
      awaiting_decision |>
      filter(Date >= max(Date) - dmonths(4)) |>
      group_by(Date) |>
      summarise(Backlog = sum(Applications)) |>
      ungroup() |>
      mutate(delta = (Backlog - lag(Backlog)) / lag(Backlog)) |>
      slice_tail(n = 1) |>
      pull(delta)

    # % change in people waiting for initial decisions, compared to same period last year
    backlog_change_year <-
      awaiting_decision |>
      filter(Date >= max(Date) - dmonths(13)) |>
      filter(Date == min(Date) | Date == max(Date)) |>
      group_by(Date) |>
      summarise(Backlog = sum(Applications)) |>
      ungroup() |>
      mutate(delta = (Backlog - lag(Backlog)) / lag(Backlog)) |>
      slice_tail(n = 1) |>
      pull(delta)

    # Top five nationalities waiting for an initial decision
    backlog_nationality <-
      awaiting_decision |>
      filter(Date == max(Date)) |>
      group_by(Nationality) |>
      summarise(Backlog = sum(Applications)) |>
      ungroup() |>
      slice_max(Backlog, n = 5) |>
      pull(Nationality)

    backlog_nationality <- paste(backlog_nationality, collapse = ", ")

    # Calculate date ranges
    date_recent_quarter <- max(awaiting_decision$Date)
    date_recent_quarter_txt <- date_formatter(date_recent_quarter)

    date_previous_quarter <-
      awaiting_decision |>
      filter(Date >= max(Date) - dmonths(4)) |>
      distinct(Date) |>
      filter(Date == min(Date))
    date_previous_quarter_txt <- date_formatter(date_previous_quarter$Date)

    date_previous_year <-
      awaiting_decision |>
      filter(Date >= max(Date) - dmonths(13)) |>
      distinct(Date) |>
      filter(Date == min(Date))
    date_previous_year_txt <- date_formatter(date_previous_year$Date)

    # Output results
    html_output <-
      div(
        p(tags$b("Number of people claiming asylum in the 12 months to", date_recent_quarter_txt, ": "), scales::comma(people_claiming_asylum)),
        p(tags$b("Number of people waiting for an initial decision, as of", date_recent_quarter_txt, ": "), scales::comma(backlog_total)),
        p(tags$b("Change in people waiting for initial decisions - from", date_previous_quarter_txt, "to", date_recent_quarter_txt, ": "), scales::percent(backlog_change, accuracy = 0.1)),
        p(tags$b("% change in people waiting for initial decisions, compared to same period last year - between", date_previous_year_txt, "and", date_recent_quarter_txt, ": "), scales::percent(backlog_change_year, accuracy = 0.1)),
        p(tags$b("Top five nationalities waiting for initial decisions, as of", date_recent_quarter_txt, ": "), backlog_nationality)
      )

    return(html_output)
  })

  # ---- Streamlined Asylum Processing (SAP) ----
  calc_SAP <- reactive({
    data_file <- download_stats("https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables", "Asylum applications, initial decisions and resettlement detailed datasets")

    decisions_resettlement <-
      read_excel(data_file, sheet = "Data - Asy_D02", skip = 1)

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

    # - SAP nationalities -
    sap_decisions <-
      decisions_resettlement |>
      filter(Nationality %in% c("Afghanistan", "Eritrea", "Libya", "Syria", "Yemen"))

    # Grants, refusals, withdrawals, and grant rates for most recent quarter
    sap_recent_quarter <-
      sap_decisions |>
      filter(Date == max(Date)) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    sap_recent_quarter_decisions <- sap_recent_quarter$Grant + sap_recent_quarter$Refused
    sap_recent_quarter_grant_rate <- sap_recent_quarter$`Initial grant rate`
    sap_recent_quarter_refusals <- sap_recent_quarter$Refused / (sap_recent_quarter$Grant + sap_recent_quarter$Refused)

    # Grants, refusals, withdrawals, and grant rates since end of March 2023
    sap_recent_year <-
      sap_decisions |>
      filter(Date > ymd("2023-03-31")) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    sap_recent_year_decisions <- sap_recent_year$Grant + sap_recent_year$Refused
    sap_recent_year_grant_rate <- sap_recent_year$`Initial grant rate`
    sap_recent_year_refusals <- sap_recent_year$Refused / (sap_recent_year$Grant + sap_recent_year$Refused)

    # - Iran and Iraq -
    iran_iraq_decisions <-
      decisions_resettlement |>
      filter(Nationality %in% c("Iran", "Iraq"))

    # Grants, refusals, withdrawals, and grant rates for most recent quarter
    iran_iraq_recent_quarter <-
      iran_iraq_decisions |>
      filter(Date == max(Date)) |>
      filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
      mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>

      group_by(`Case outcome group`) |>
      summarise(Decisions = sum(Decisions)) |>
      ungroup() |>

      pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
      mutate(`Initial grant rate` = Grant / (Grant + Refused))

    iran_iraq_recent_quarter_decisions <- iran_iraq_recent_quarter$Grant + iran_iraq_recent_quarter$Refused
    iran_iraq_recent_quarter_grant_rate <- iran_iraq_recent_quarter$`Initial grant rate`
    iran_iraq_recent_quarter_refusals <- iran_iraq_recent_quarter$Refused / (iran_iraq_recent_quarter$Grant + iran_iraq_recent_quarter$Refused)

    # Grants, refusals, withdrawals, and grant rates since Q3 2023
    # TODO: Uncomment these lines when newest stats are published
    # iran_iraq_recent_year <-
    #   iran_iraq_decisions |>
    #   filter(Date > ymd("2023-09-30")) |>
    #   filter(`Case type` == "Asylum Case", `Applicant type` == "Main applicant") |>
    #   mutate(`Case outcome group` = if_else(str_detect(`Case outcome group`, "Grant"), "Grant", `Case outcome group`)) |>
    #
    #   group_by(`Case outcome group`) |>
    #   summarise(Decisions = sum(Decisions)) |>
    #   ungroup() |>
    #
    #   pivot_wider(names_from = `Case outcome group`, values_from = Decisions) |>
    #   mutate(`Initial grant rate` = Grant / (Grant + Refused))
    #
    # iran_iraq_recent_year_decisions <- iran_iraq_recent_year$Grant + iran_iraq_recent_year$Refused
    # iran_iraq_recent_year_grant_rate <- iran_iraq_recent_year$`Initial grant rate`
    # iran_iraq_recent_year_refusals <- iran_iraq_recent_year$Refused / (iran_iraq_recent_year$Grant + iran_iraq_recent_year$Refused)

    # Calculate date ranges
    date_recent_quarter <- max(sap_decisions$Date)
    date_recent_quarter_txt <- date_formatter(date_recent_quarter)

    html_output <-
      div(
        h4("People from Streamlined Asylum Process (SAP) nationalities: most recent quarter"),
        p(tags$b("Number of initial decisions (grants and refusals) for main applicant adults from SAP nationalities made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(sap_recent_quarter_decisions)),
        p(tags$b("Number of grants for main applicant adults from SAP nationalities made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(sap_recent_quarter$Grant)),
        p(tags$b("Number of refusals for main applicant adults from SAP nationalities made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(sap_recent_quarter$Refused)),
        p(tags$b("Number of withdrawals for main applicant adults from SAP nationalities made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(sap_recent_quarter$Withdrawn)),
        p(tags$b("Initial grant rate for main applicant adults from SAP nationalities during last quarter, as of", date_recent_quarter_txt, ": "), scales::percent(sap_recent_quarter_grant_rate, accuracy = 0.1)),

        br(),
        h4("People from Streamlined Asylum Process (SAP) nationalities: since March 2023"),
        p(tags$b("Number of initial decisions (grants and refusals) for main applicant adults from SAP nationalities since end of March 2023"), scales::comma(sap_recent_year_decisions)),
        p(tags$b("Number of grants for main applicant adults from SAP nationalities since end of March 2023"), scales::comma(sap_recent_year$Grant)),
        p(tags$b("Number of refusals for main applicant adults from SAP nationalities since end of March 2023"), scales::comma(sap_recent_year$Refused)),
        p(tags$b("Number of withdrawals for main applicant adults from SAP nationalities since end of March 2023"), scales::comma(sap_recent_year$Withdrawn)),
        p(tags$b("Initial grant rate for main applicant adults from SAP nationalities since end of March 2023"), scales::percent(sap_recent_year_grant_rate, accuracy = 0.1)),
        p(),

        br(),
        h4("People from Iran and Iraq: most recent quarter"),
        p(tags$b("Number of initial decisions (grants and refusals) for main applicant adults from Iran and Iraq made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(iran_iraq_recent_quarter_decisions)),
        p(tags$b("Number of grants for main applicant adults from Iran and Iraq made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(iran_iraq_recent_quarter$Grant)),
        p(tags$b("Number of refusals for main applicant adults from Iran and Iraq made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(iran_iraq_recent_quarter$Refused)),
        p(tags$b("Number of withdrawals for main applicant adults from Iran and Iraq made over last quarter, as of", date_recent_quarter_txt, ": "), scales::comma(iran_iraq_recent_quarter$Withdrawn)),
        p(tags$b("Initial grant rate for main applicant adults from Iran and Iraq during last quarter, as of", date_recent_quarter_txt, ": "), scales::percent(iran_iraq_recent_quarter_grant_rate, accuracy = 0.1)),

        # br(),
        # h4("People from Iran and Iraq: since Q3 (September) 2023"),
        # p(tags$b("Number of initial decisions (grants and refusals) for main applicant adults from Iran and Iraq since Q3 2023"), scales::comma(iran_iraq_recent_year_decisions)),
        # p(tags$b("Number of grants for main applicant adults from Iran and Iraq since Q3 2023"), scales::comma(iran_iraq_recent_year$Grant)),
        # p(tags$b("Number of refusals for main applicant adults from Iran and Iraq since Q3 2023"), scales::comma(iran_iraq_recent_year$Refused)),
        # p(tags$b("Number of withdrawals for main applicant adults from Iran and Iraq since Q3 2023"), scales::comma(iran_iraq_recent_year$Withdrawn)),
        # p(tags$b("Initial grant rate for main applicant adults from Iran and Iraq since Q3 2023"), scales::percent(iran_iraq_recent_year_grant_rate, accuracy = 0.1)),

        p(),
        p("Initial decisions referrs to grants and refusals for main applicants only; withdrawals do not count as decisions. Figures do not include resettlement. SAP nationalities are: Afghanistan, Eritrea, Libya, Syria, and Yemen.")
      )

    return(html_output)
  })

  # ---- Asylum support ----
  calc_support <- reactive({
    data_file <- download_stats("https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables", "Asylum seekers in receipt of support detailed datasets")

    support_received <-
      read_excel(data_file, sheet = "Data - Asy_D09", skip = 1)

    # Wrangling
    support_received <-
      support_received |>
      rename_with(~"Date", starts_with("Date")) |>
      filter(toupper(Date) != "END OF TABLE") |>
      mutate(Date = dmy(Date)) |>
      select(-starts_with("..."))

    # Number of people in receipt of asylum support – total and by Section – compared with previous quarter and compared with same quarter 12 months ago
    support_by_date_and_section <-
      support_received |>
      filter(Date >= max(Date) - dmonths(12)) |>
      group_by(Date, `Support Type`) |>
      summarise(People = sum(People)) |>
      ungroup()

    recent_quarter_support <-
      support_by_date_and_section |>
      filter(Date == max(Date))

    recent_quarter_total <- sum(recent_quarter_support$People)
    recent_quarter_section4 <- recent_quarter_support |> filter(`Support Type` == "Section 4") |> pull(People)
    recent_quarter_section95 <- recent_quarter_support |> filter(`Support Type` == "Section 95") |> pull(People)
    recent_quarter_section98 <- recent_quarter_support |> filter(`Support Type` == "Section 98") |> pull(People)

    previous_quarter_support <-
      support_by_date_and_section |>
      filter(Date < max(Date)) |>
      filter(Date == max(Date))

    previous_quarter_total <- sum(previous_quarter_support$People)
    previous_quarter_section4 <- previous_quarter_support |> filter(`Support Type` == "Section 4") |> pull(People)
    previous_quarter_section95 <- previous_quarter_support |> filter(`Support Type` == "Section 95") |> pull(People)
    previous_quarter_section98 <- previous_quarter_support |> filter(`Support Type` == "Section 98") |> pull(People)

    previous_year_support <-
      support_by_date_and_section |>
      filter(Date == min(Date))

    previous_year_total <- sum(previous_year_support$People)
    previous_year_section4 <- previous_year_support |> filter(`Support Type` == "Section 4") |> pull(People)
    previous_year_section95 <- previous_year_support |> filter(`Support Type` == "Section 95") |> pull(People)
    previous_year_section98 <- previous_year_support |> filter(`Support Type` == "Section 98") |> pull(People)

    # % changes
    change_quarter_on_quarter <- (recent_quarter_total - previous_quarter_total) / previous_quarter_total
    change_year <- (recent_quarter_total - previous_year_total) / previous_year_total

    # Accommodation
    accommodation <-
      support_received |>
      filter(Date >= max(Date) - dmonths(12)) |>
      group_by(Date, `Accommodation Type`) |>
      summarise(People = sum(People)) |>
      ungroup()

    recent_quarter_accomm <-
      accommodation |>
      filter(Date == max(Date))

    recent_quarter_accomm_dispersed <- recent_quarter_accomm |> filter(str_detect(`Accommodation Type`, "Dispersed")) |> pull(People)
    recent_quarter_accomm_hotels <- recent_quarter_accomm |> filter(str_detect(`Accommodation Type`, "Hotel")) |> pull(People)
    recent_quarter_accomm_other <- recent_quarter_accomm |> filter(str_detect(`Accommodation Type`, "Contingency.*Other")) |> pull(People)

    previous_quarter_accomm <-
      accommodation |>
      filter(Date < max(Date)) |>
      filter(Date == max(Date))

    previous_quarter_accomm_dispersed <- previous_quarter_accomm |> filter(str_detect(`Accommodation Type`, "Dispersed")) |> pull(People)
    previous_quarter_accomm_hotels <- previous_quarter_accomm |> filter(str_detect(`Accommodation Type`, "Hotel")) |> pull(People)
    previous_quarter_accomm_other <- previous_quarter_accomm |> filter(str_detect(`Accommodation Type`, "Contingency.*Other")) |> pull(People)

    previous_year_accomm <-
      accommodation |>
      filter(Date == min(Date))

    previous_year_accomm_dispersed <- previous_year_accomm |> filter(str_detect(`Accommodation Type`, "Dispersed")) |> pull(People)
    previous_year_accomm_hotels <- previous_year_accomm |> filter(str_detect(`Accommodation Type`, "Hotel")) |> pull(People)
    previous_year_accomm_other <- previous_year_accomm |> filter(str_detect(`Accommodation Type`, "Contingency.*Other")) |> pull(People)

    # % change in number of people in hotels compared with previous quarter
    change_hotels <- (recent_quarter_accomm_hotels - previous_quarter_accomm_hotels) / previous_quarter_accomm_hotels

    # % change in number of people in other contingency accommodation compared with previous quarter
    change_contingency <- (recent_quarter_accomm_other - previous_quarter_accomm_other) / previous_quarter_accomm_other

    # Calculate date ranges
    date_recent_quarter_txt <- date_formatter(max(recent_quarter_support$Date))
    date_previous_quarter_txt <- date_formatter(max(previous_quarter_support$Date))
    date_previous_year_txt <- date_formatter(max(previous_year_support$Date))

    html_output <-
      div(
        p(tags$b("Number of people receiving asylum support, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_total)),
        p(tags$b("Number of people receiving asylum support in previous quarter (", date_previous_quarter_txt, "): "), scales::comma(previous_quarter_total)),
        p(tags$b("Number of people receiving asylum support in same quarter previous year (", date_previous_year_txt, "): "), scales::comma(previous_year_total)),

        p(tags$b("% change in people receiving asylum support between ", date_previous_quarter_txt, "and", date_recent_quarter_txt, ": "), scales::percent(change_quarter_on_quarter, accuracy = 0.1)),
        p(tags$b("% change in people receiving asylum support between ", date_previous_year_txt, "and", date_recent_quarter_txt, ": "), scales::percent(change_year, accuracy = 0.1)),

        br(),
        h4("Section 4"),
        p(tags$b("Number of people receiving Section 4 support, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_section4)),
        p(tags$b("Number of people receiving Section 4 support in previous quarter (", date_previous_quarter_txt, "): "), scales::comma(previous_quarter_section4)),
        p(tags$b("Number of people receiving Section 4 support in same quarter previous year (", date_previous_year_txt, "): "), scales::comma(previous_year_section4)),

        br(),
        h4("Section 95"),
        p(tags$b("Number of people receiving Section 95 support, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_section95)),
        p(tags$b("Number of people receiving Section 95 support in previous quarter (", date_previous_quarter_txt, "): "), scales::comma(previous_quarter_section95)),
        p(tags$b("Number of people receiving Section 95 support in same quarter previous year (", date_previous_year_txt, "): "), scales::comma(previous_year_section95)),

        br(),
        h4("Section 98"),
        p(tags$b("Number of people receiving Section 98 support, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_section98)),
        p(tags$b("Number of people receiving Section 98 support in previous quarter (", date_previous_quarter_txt, "): "), scales::comma(previous_quarter_section98)),
        p(tags$b("Number of people receiving Section 98 support in same quarter previous year (", date_previous_year_txt, "): "), scales::comma(previous_year_section98)),

        br(),
        h4("Asylum accommodation"),
        p(tags$b("Number of people in dispersed accommodation, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_accomm_dispersed)),
        p(tags$b("Number of people in contingency hotels, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_accomm_hotels)),
        p(tags$b("Number of people in other contingency accommodation, as of", date_recent_quarter_txt, ": "), scales::comma(recent_quarter_accomm_other)),
        br(),
        p(tags$b("Number of people in dispersed accommodation, as of", date_previous_year_txt, ": "), scales::comma(previous_year_accomm_dispersed)),
        p(tags$b("Number of people in contingency hotels, as of", date_previous_year_txt, ": "), scales::comma(previous_year_accomm_hotels)),
        p(tags$b("Number of people in other contingency accommodation, as of", date_previous_year_txt, ": "), scales::comma(previous_year_accomm_other)),
        br(),
        p(tags$b("% change in people in contingency hotels between", date_previous_quarter_txt, "and", date_recent_quarter_txt, ": "), scales::percent(change_hotels, accuracy = 0.1)),
        p(tags$b("% change in people in other contingency accommodation between", date_previous_quarter_txt, "and", date_recent_quarter_txt, ": "), scales::percent(change_contingency, accuracy = 0.1))
      )

    return(html_output)
  })

  # ---- Calculate and download statistics ----
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     if (input$topicChoice == "channel") {
  #       paste("channel-crossings", ".csv", sep = "")
  #     } else if (input$topicChoice == "grants") {
  #       paste("grants", ".csv", sep = "")
  #     }
  #   },
  #
  #   content = function(file) {
  #
  #     output_data <- NULL
  #
  #     if (input$topicChoice == "channel") {
  #       output_data <- calc_irregular_migration()
  #     }
  #
  #     write_csv(output_data, file)
  #   }
  # )

}
