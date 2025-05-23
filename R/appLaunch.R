library(shiny)
library(shinycssloaders)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(lubridate)
library(scales)
library(readxl)
library(tidyr)
library(zoo)
library(httr2)
library(rvest)
library(xml2)
library(plotly)

appLaunch <- function() {
  shinyApp(ui, server)
}
