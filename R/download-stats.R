download_stats <- function(url, name_of_file) {
  # Automatically try to download files when user clicks the radio button
  # If it can't find it, or it looks wrong to the user, let them choose to upload manually
  # url <- "https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum-and-resettlement"
  # url <- "https://www.gov.uk/government/statistical-data-sets/irregular-migration-detailed-dataset-and-summary-tables"
  # name_of_file <- "Detailed datasets"
  # name_of_file <- "Asylum applications, initial decisions and resettlement detailed datasets"

  # Get webpage data
  # req <- request(url)
  # resp <- req_perform(req)
  # doc <- resp_body_html(resp)
  doc <-
    request(url) |>
    req_perform() |>
    resp_body_html()

  # Helper function to extract the URL based on 'text_to_find'
  extract_url <- function(doc, text_to_find) {
    doc |>
      html_nodes(xpath = str_glue("//*//a[contains(text(), '{text_to_find}')]//@href")) |>
      html_text() |>
      str_trim()
  }

  # Extract the URL for this file
  data_url <-
    doc |>
    extract_url(name_of_file)

  # On the irregular migration page, there are two "Detailed datasets" links, but only one leads to a file - extract it
  if (is.vector(data_url)) {
    data_url <- data_url[ grepl("https", data_url) ]
  }

  # Download the data file and return it
  data_file_ext <- paste0(".", tools::file_ext(data_url))
  temp_path <- tempfile(fileext = data_file_ext)
  req_perform(request(data_url), path = temp_path)
  return(temp_path)
}
