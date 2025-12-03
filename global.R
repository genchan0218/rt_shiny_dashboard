# ============================================================================
# GLOBAL.R - Initialization and Helper Functions
# ============================================================================
# Last Updated: November 5, 2025
# Purpose: Load all data, configure authentication, and define helper functions
# Used by: server.R and ui.R
# Use RStudio Document Outline (Ctrl/Cmd + Shift + O) to navigate sections

# SETUP: Package Loading ----
# Load required packages and custom functions
# Using optimized version - reduces startup time by ~60%
source("Genki_Must_Haves_Optimized.R")


# DATA: Core POS and Sales Data ----
# Loads primary POS transaction data and sales territory mappings
# Used by: POS Quarterly Rev, POS Row by Row tabs

Part_Number_List <- fread("Part_Number_List.csv")
Sales_Map <- fread("Sales_Map.csv")

# Load POS_Agg once (was loaded twice before - line 21 and 51)
POS_Agg <- fread("POS_Agg.csv")
POS_Agg[, Price_ea := as.numeric(gsub("\\$", "", Price_ea))]
POS_Agg[, Revenue := Price_ea * Qty]
setDT(POS_Agg)

# Create All_POS reference for backward compatibility
All_POS <- POS_Agg

# Drop original Description and join with Part_Number_List
All_POS2 <- All_POS[, !"Description"]
All_POS2 <- left_join(All_POS2, Part_Number_List[, .(P_N, Description)], by = c("Item" = "P_N")) |> setDT()

NewDT3 <- fread("NewDT3.csv")
NewDT3 <- left_join(NewDT3, Sales_Map)

All_POS_Ohm_Qtr <- fread("POS_vs_Ohm.csv")


# DATA: Branch and Contact Information ----
# Loads PC, Cooper, and Wesco branch data with contact information
# Used by: GT PC# Table, Cooper Branch Table, Wesco Branch Table tabs

PCdata <- fread("CED_GT_Manager_Map.csv")
PCdata <- left_join(PCdata, Sales_Map)

Cooperdata <- fread("Cooper_Map.csv")
colnames(Cooperdata) <- gsub("\\.", " ", colnames(Cooperdata))
Cooperdata <- left_join(Cooperdata, Sales_Map)


# DATA: Invoice and Forecasting Data ----
# Loads invoice data for revenue analysis and forecasting
# Used by: Invoice Revenue Insights tab

Invoice_data <- fread("Invoice_data.csv")

# POS_Agg already loaded above - no need to reload


# DATA: Ohm Analytics and Installer Data ----
# Loads installer capacity data from Ohm Analytics for prospecting
# Used by: POS and Ohm Analytics Screener tab

ohm_data <- fread("Ohm_All_Installers_Kw_PC.csv")
ohm_data_qtrly_avg <- fread("Ohm_All_Installers_Kw_PC_Qtrly.csv")
# Join on common keys to add quarterly averages
ohm_data <- left_join(ohm_data, ohm_data_qtrly_avg,
                       by = c("Contractor_Name", "State", "RT_POS_Customer"))
ohm_data_qtrly_avg <- NULL
setDT(ohm_data)

contacts_data <- fread("Contact_List.csv")
pos_data <- fread("POS_vs_Ohm.csv")

wesco_branches <- fread("Wesco_Branches.csv")
wesco_branches <- rbind(
  left_join(wesco_branches[mail_country == "US", ], Sales_Map, by = c("mail_state" = "State")),
  wesco_branches[mail_country != "US"][, Rep := "Guy"]
)
setnames(wesco_branches, colnames(wesco_branches),
         c("Branch ID", "Branch Name", "Address", "City", "State/Province", "Zip Code", "Country", "Rep"))


# DATA: Inventory Tracking Data ----
# Loads distributor inventory data for stock level monitoring
# Used by: Distributor Inventory Tracker tab

GT_Inv <- fread("GT_Inventory.csv")
setDT(GT_Inv)
GT_Inv[, Price_ea := as.numeric(gsub("\\$", "", Price_ea))]
GT_Inv[, `On Hand Value` := as.numeric(gsub("\\$", "", `On Hand Value`))]

inv_dt <- fread("Distributor_Inventory.csv")
inv_dt[, Price_ea := as.numeric(gsub("\\$", "", Price_ea))]
inv_dt[, `On Hand Value` := as.numeric(gsub("\\$", "", `On Hand Value`))]
inv_dt[, Date := as.Date(Date)]


# CONFIG: Distributor Code Mapping ----
# Maps distributor codes (1-8) to full distributor names
# Used across all tabs for consistent distributor identification

# Distributor code mapping
dist_map <- data.table(
  Dist_Code   = c("1", "2", "3", "4", "5", "6", "7", "8"),
  Distributor = c(
    "CED Greentech",
    "BayWa r.e. USA",
    "Krannich",
    "US Renewable Energy",
    "Cooper",
    "Soligent",
    "Wesco USA",
    "Wesco Canada"
  )
)


# CONFIG: User Authentication ----
# Defines authorized users with encrypted passwords
# All users currently have admin permissions
# Used by: shinyauthr authentication system

# User authentication setup
user_base <- tibble::tibble(
  user = c("Genki", "Guy", "Admin", "Ryan", "RyanW"),
  password = sapply(c("rt1234", "rt1121g", "rt0000", "rt7181", "rt5161"), sodium::password_store),
  permissions = c("admin", "admin", "admin", "admin", "admin"),
  name = c("Genki", "Guy", "Admin", "Ryan", "RyanW")
)


# HELPER FUNCTIONS ----
# Reusable functions called by server.R for data processing and filtering


# FUNCTION: load_aggregated_data() ----
# Loads aggregated quarterly revenue data based on category, year, and user permissions
# Handles multi-year selection by loading appropriate pre-aggregated CSV files
# Used by: POS Quarterly Rev tab
load_aggregated_data <- function(category_val, year_val, permissions, user_name) {
  # Handle multiple years or single year
  if (length(year_val) > 1) {
    # Multiple years selected - load the "All" file
    if (category_val != "" && category_val != "All") {
      filename <- paste0("NewDT4_", category_val, "_All.csv")
    } else {
      filename <- "NewDT4_All_All.csv"
    }
  } else if (length(year_val) == 1) {
    # Single year selected
    if (category_val != "" && category_val != "All") {
      filename <- paste0("NewDT4_", category_val, "_", year_val, ".csv")
    } else {
      filename <- paste0("NewDT4_All_", year_val, ".csv")
    }
  } else {
    # No year selected - load all
    if (category_val != "" && category_val != "All") {
      filename <- paste0("NewDT4_", category_val, "_All.csv")
    } else {
      filename <- "NewDT4_All_All.csv"
    }
  }

  # Load data
  data2 <- fread(filename)
  setnames(data2, "State", "Ship To State")
  data2[, `PC #` := as.character(`PC #`)]
  data2[, `PC #2` := `PC #`]

  # Filter by user if not admin
  if (permissions != "admin") {
    data2 <- data2[Rep == user_name]
  }

  return(data2)
}


# FUNCTION: apply_base_filters() ----
# Applies common UI filters (Rep, State, Dist Code, PC#) to any data table
# Used by: Multiple tabs for consistent filter application
apply_base_filters <- function(dt, rep_val = NULL, state_val = NULL,
                                dist_code_val = NULL, pc_val = NULL) {
  if (!is.null(rep_val) && rep_val != "All") {
    dt <- dt[Rep == rep_val]
  }
  # Handle multiple state selection
  if (!is.null(state_val) && length(state_val) > 0 && !("All" %in% state_val)) {
    dt <- dt[`Ship To State` %in% state_val]
  }
  if (!is.null(dist_code_val) && dist_code_val != "All") {
    dt <- dt[Dist_Code == dist_code_val]
  }
  if (!is.null(pc_val) && pc_val != "All") {
    dt <- dt[`PC #2` == pc_val]
  }
  return(dt)
}


# FUNCTION: create_action_links() ----
# Creates clickable action links for Customer and PC# columns in tables
# Passes Customer ID and PC# to Shiny inputs when clicked for modal popups
# Used by: POS Quarterly Rev tab for customer detail modals
create_action_links <- function(dt) {
  # Return empty data table immediately if no rows
  if (nrow(dt) == 0) {
    return(dt)
  }

  # Only create PC #2 if it doesn't already exist
  # PC #2 stores the raw PC # values for filtering and action link creation
  if (!"PC #2" %in% colnames(dt)) {
    # Save raw PC # values to PC #2 (converting to character first)
    dt[, `PC #2` := as.character(`PC #`)]
  }

  # Create Customer action links using by to process one row at a time
  dt[, Customer_raw := Customer]
  dt[, inputId_cust := paste0("customer_input_", seq_len(.N))]
  dt[, Customer := as.character(actionLink(
    inputId = inputId_cust,
    label = Customer_raw,
    onclick = sprintf("Shiny.setInputValue('customer_click', %s, {priority: 'event'});", Cust_ID)
  )), by = inputId_cust]
  dt[, inputId_cust := NULL]

  # Create PC # action links using PC #2 as the source (which has raw values)
  dt[, inputId_pc := paste0("pc_input_", seq_len(.N))]
  dt[, `PC #` := as.character(actionLink(
    inputId = inputId_pc,
    label = `PC #2`,
    onclick = sprintf("Shiny.setInputValue('pc_click', %s, {priority: 'event'});", `PC #2`)
  )), by = inputId_pc]
  dt[, inputId_pc := NULL]

  return(dt)
}


# FUNCTION: get_newdt3_data() ----
# Returns NewDT3 data filtered by user permissions
# Admin users see all data; regular users see only their assigned accounts
# Used by: POS tabs for permission-based data filtering
get_newdt3_data <- function(permissions, user_name) {
  if (permissions == "admin") {
    return(NewDT3)
  } else {
    return(NewDT3[Rep == user_name])
  }
}
