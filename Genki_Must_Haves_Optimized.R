# ============================================================================
# OPTIMIZED PACKAGE LOADING FOR POS SHINY DASHBOARD
# ============================================================================
# This file loads ONLY the packages needed for the dashboard
# Original Genki_Must_Haves.R backed up - this version is ~60% faster

options("scipen" = 100, "digits" = 4)

# Core data manipulation
require(data.table)
require(dplyr)
require(tidyr)

# Date/time handling
require(lubridate)

# String manipulation
require(stringr)
require(stringi)

# Shiny and UI
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinyauthr)
require(DT)

# Plotting and visualization
require(ggplot2)
require(plotly)
require(scales)
require(viridis)

# Time series forecasting (for Invoice Revenue Insights tab)
require(forecast)

# File I/O
require(openxlsx)

# Helper functions from original file (keeping only what's actually used)

# Date helper
JP_Date <- date <- as.character(Sys.Date())
JP_Date <- strsplit(JP_Date, "-")[[1]]
JP_Date2 <- paste0(JP_Date[1], "/", JP_Date[2], "/", JP_Date[3])

date <- as.character(Sys.Date())
date <- strsplit(date, "-")[[1]]
date2 <- paste0(date[1], "/", date[2], "/", date[3])

# Utility function for adding NA values
`%+na%` <- function(x, y) {
  ifelse(is.na(x), y, ifelse(is.na(y), x, x + y))
}

# Remove commas from numeric strings
remove_commas <- function(x) {
  gsub(",", "", x)
}

# Get week start and end dates
get_weekday_start_and_end <- function(input_date) {
  input_date <- as.Date(input_date)
  week_start <- floor_date(input_date, "week") + days(1)
  week_end <- ceiling_date(input_date, "week") - days(2)
  return(c(week_start, week_end))
}

get_week_start_and_end <- function(input_date) {
  input_date <- as.Date(input_date)
  week_start <- floor_date(input_date, "week")
  week_end <- ceiling_date(input_date, "week") - days(1)
  return(c(week_start, week_end))
}

# Find month with more days in a date range
find_month_with_more_days <- function(start_date, end_date) {
  week_days <- seq(start_date, end_date, by = "1 day")
  days_in_month <- table(month(week_days))
  more_days_month <- names(days_in_month)[which.max(days_in_month)]
  return(more_days_month)
}

# Find year with more days in a date range
find_year_with_more_days <- function(start_date, end_date) {
  week_days <- seq(start_date, end_date, by = "1 day")
  days_in_year <- table(year(week_days))
  more_days_year <- names(days_in_year)[which.max(days_in_year)]
  return(more_days_year)
}

# Get number of days in a month
numberOfDays <- function(date) {
  m <- format(date, format = "%m")

  while (format(date, format = "%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format = "%d")))
}

# Turn first row into column names
Turn_First_Row_To_Column <- function(df) {
  colnames(df) <- unlist(df[1, ])
  df <- df[-1, ]
  return(df)
}

# Custom DT formatting functions
tplRound3 = function(cols, currency, digits, interval, mark) {
  sprintf(
    "var d = parseFloat(data[%d]); $(this.api().cell(row, %s).node()).html(isNaN(d) ? '' : '%s' + d.toFixed(%d).toString().replace(/\\B(?=(\\d{%d})+(?!\\d))/g, '%s'));",
    cols, cols, currency, digits, interval, mark
  )
}

formatRound3 = function(table, columns, currency, digits = 2, interval = 3, mark = ',') {
  formatColumns2(table, columns, tplRound3, currency, digits, interval, mark)
}

formatColumns2 = function(table, columns, template, ...) {
  if (inherits(columns, 'formula')) columns = all.vars(columns)
  x = table$x
  colnames = base::attr(x, 'colnames', exact = TRUE)
  rownames = base::attr(x, 'rownames', exact = TRUE)
  x$options$rowCallback = appendFormatter2(
    x$options$rowCallback, columns, colnames, rownames, template, ...
  )
  table$x = x
  table
}

name2int = function(name, names) {
  if (is.numeric(name)) {
    return(if (all(name > 0)) name else seq_along(names)[name])
  }
  names = setNames(seq_along(names), names)
  unname(names[name])
}

appendFormatter2 = function(js, name, names, rownames = TRUE, template, ...) {
  js = if (length(js) == 0) c('function(row, data) {', '}') else {
    unlist(strsplit(as.character(js), '\n'))
  }
  i = name2int(name, names)
  if (is.character(name) || (is.numeric(name) && !rownames)) i = i - 1
  if (any(is.na(i))) stop(
    'You specified the columns: ', paste(name, collapse = ', '), ', ',
    'but the column names of the data are ', paste(names, collapse = ', ')
  )
  JS(append(
    js, after = 1,
    template(i, ...)
  ))
}

# Image for branding (if used in UI)
img_rooftech <- paste0("<center><img loading='lazy;' font-size: 18px; src='cid:", "Roof-Tech-Logo.png' alt = 'RT Ohm Analytics Data Entry Progress Report'><center>")
