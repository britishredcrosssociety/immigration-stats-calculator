library(shiny)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(lubridate)
library(scales)
library(readxl)
library(tidyr)
library(zoo)

appLaunch <- function() {
  shinyApp(ui, server)
}
