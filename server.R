# ============================================================================
# SERVER.R - Server Logic
# ============================================================================
# Note: Data loading and helper functions are now in global.R
#
# SESSION NOTES (Nov 5, 2025):
# - Fixed Windows IDate/year() error by adding lubridate:: namespace
# - Fixed NULL checks in POS and Ohm Analytics Screener tab
# - Rewrote complete About section with all 8 tabs documented
# - Fixed customer click modal to use customer ID instead of row index
# - Intelligent lost business sorting by relevant quarter
#
# Use RStudio Document Outline (Ctrl/Cmd + Shift + O) to navigate sections
# ============================================================================

server <- function(input, output, session) {
  
  # --- NEW: keeps the exact data visible in the main table (pre-HTML) for modal lookups
  main_table_data <- reactiveVal(NULL)
  
  # --- NEW: final safety filter that enforces UI filters on already-wide tables
  .final_table_filter <- function(dt, rep, states, dist, pc) {
    dt <- data.table::as.data.table(dt)
    if (!is.null(rep)    && rep    != "All") dt <- dt[Rep == rep]
    if (!is.null(states) && length(states) > 0 && !("All" %in% states)) dt <- dt[`Ship To State` %in% states]
    if (!is.null(dist)   && dist   != "All") dt <- dt[`Dist Code` == dist]
    if (!is.null(pc)     && pc     != "All") dt <- dt[`PC #` == as.character(pc)]
    dt
  }
  
  # Authentication ----
  # Login Auth
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # OBSERVER: Ship To State Filter - All vs Specific States ----
  # Handles mutual exclusivity between "All" and specific state selections
  # Case 1: If All is already selected and user picks a specific state, remove "All"
  # Case 2: If specific states are selected and user selects "All", remove all specific states
  
  # Track previous state selection to determine which item was just added
  state_previous <- reactiveVal(c("All"))
  
  observeEvent(input$ship_to_state, {
    current_selection <- input$ship_to_state
    previous_selection <- state_previous()
    
    # Only proceed if there are multiple selections
    if (length(current_selection) > 1) {
      # If "All" is in the selection along with other states
      if ("All" %in% current_selection) {
        # Determine which item(s) were just added
        new_items <- setdiff(current_selection, previous_selection)
        
        if ("All" %in% new_items) {
          # Case 2: "All" was just added - keep only "All"
          updateSelectizeInput(session, "ship_to_state", selected = "All")
        } else {
          # Case 1: A specific state was just added - remove "All"
          new_selection <- setdiff(current_selection, "All")
          updateSelectizeInput(session, "ship_to_state", selected = new_selection)
        }
      }
    }
    
    # Update previous selection for next comparison
    state_previous(current_selection)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # OBSERVER: Smart Year Selection for Quarterly/Monthly Toggle ----
  # Automatically adjusts year selection when toggling between views
  # Monthly view: Optimized for recent data (1-2 years)
  # Quarterly view: Broader analysis (3 most recent years)
  observeEvent(input$view_toggle, {
    if (!is.null(input$view_toggle)) {
      # Find most recent date in the data
      most_recent_date <- max(POS_Agg$Date, na.rm = TRUE)
      most_recent_month <- lubridate::month(most_recent_date)
      most_recent_year <- lubridate::year(most_recent_date)

      # Get all available years
      all_years <- sort(unique(lubridate::year(POS_Agg$Monthly)), decreasing = TRUE)

      if (input$view_toggle == TRUE) {
        # MONTHLY VIEW - Smart filtering for efficiency
        # Select years based on month
        if (most_recent_month < 7) {
          # Before July: Select 2 most recent years
          selected_years <- head(all_years, 2)
        } else {
          # July or after: Select only the most recent year
          selected_years <- most_recent_year
        }
      } else {
        # QUARTERLY VIEW - Broader trend analysis
        # Select 3 most recent years for better quarterly comparison
        selected_years <- head(all_years, 3)
      }

      # Update the year selectInput
      updateSelectizeInput(session, "year", selected = selected_years)
    }
  }, ignoreInit = FALSE)

  output$loggedinUI0 <- renderUI({
    req(credentials()$user_auth)
    
    titlePanel(fluidRow(
      img(src = 'Roof-Tech-Logo.png', align = "left", width = "100px", height = "auto")
    ))
  })
  
  # Loggedin UI
  output$loggedinUI <- renderUI({
    req(credentials()$user_auth)
    
    # Navigation Bar
    navbarPage(
      "POS Deep Dive",
      tags$head(
        tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:10px !important;
                            padding-bottom:0 !important;
                            height: 35px;
                            }
                           .navbar {min-height:40px !important;}'))
      ),
      id = "main_navbar",
      div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
      tabPanel(
        "POS Quarterly Rev",
        mainPanel(
          
          div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
          
          fluidRow(
            column(2,
                   selectInput("rep", "Sales Person:",
                               c("All", sort(unique(as.character(NewDT3$Rep)))))
            ),
            column(2,
                   selectizeInput("ship_to_state", "Ship To State:",
                                  choices = c("All", sort(unique(as.character(NewDT3$State)))),
                                  selected = "All",
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select state(s)'))
            ),
            column(2,
                   selectInput("dist_code", "Dist Code:",
                               c("All", sort(unique(as.character(All_POS$Dist_Code)))))
            ),
            column(2,
                   selectizeInput("year", "Year:",
                                  choices = sort(unique(as.integer(lubridate::year(NewDT3$Monthly)))),
                                  selected = {
                                    # Smart default selection based on most recent date
                                    most_recent_date <- max(POS_Agg$Date, na.rm = TRUE)
                                    most_recent_month <- lubridate::month(most_recent_date)
                                    all_years <- sort(unique(lubridate::year(POS_Agg$Monthly)), decreasing = TRUE)
                                    if (most_recent_month < 7) {
                                      head(all_years, 2)  # Before July: 2 most recent years
                                    } else {
                                      head(all_years, 1)  # July or after: only current year
                                    }
                                  },
                                  multiple = TRUE)
            ),
            column(2,
                   selectInput("PC", "PC #:",
                               c("All", sort(unique(as.character(NewDT3$`PC #`)))))
            ),
            column(2,
                   selectInput("category", "Product Category:",
                               c("All", sort(unique(as.character(Part_Number_List$Category)))))
            ),
          ),
          
          fluidRow(
            column(2,
                   selectInput("lost_business_filter", "Lost Business Filter (Months):",
                               c(0:length(unique(All_POS$Monthly))))
            ),
            column(3,
                   dateRangeInput("last_purchase_range", "Last Purchase Date Range:",
                                  start = min(All_POS$Date),
                                  end = max(All_POS$Date),
                                  format = "yyyy-mm-dd")
            ),
            column(3,
                   dateRangeInput("first_purchase_range", "First Purchase Date Range:",
                                  start = min(All_POS$Date),
                                  end = max(All_POS$Date),
                                  format = "yyyy-mm-dd")
            ),
            column(3, br(), actionButton("reset_purchase_range", "Reset Date Ranges", class = "btn-primary", width = "100%")),
            column(1, br(),
                   shinyWidgets::switchInput(
                     inputId = "view_toggle",
                     label   = "View Type:",
                     onLabel = "Monthly",
                     offLabel = "Quarterly",
                     value   = TRUE,
                     onStatus = "success",
                     offStatus = "primary"
                   )
            )
          ),
          
          fluidRow(
            column(3, shinyWidgets::prettySwitch(inputId = "posq_show_category", label = "Show Viz: by Product Category", value = FALSE, status = "primary", bigger = TRUE)),
            column(3, shinyWidgets::prettySwitch(inputId = "posq_show_distributor", label = "Show Viz: by Distributor", value = FALSE, status = "primary", bigger = TRUE)),
            column(3, shinyWidgets::prettySwitch(inputId = "posq_show_rep", label = "Show Viz: by Rep", value = FALSE, status = "primary", bigger = TRUE)),
            column(3, shinyWidgets::prettySwitch(inputId = "posq_show_pc", label = "Show Viz: by PC #", value = FALSE, status = "primary", bigger = TRUE))
          ),

          fluidRow(
            column(3, shinyWidgets::prettySwitch(inputId = "posq_show_guidance", label = "Show Suggested Actions", value = TRUE, status = "success", bigger = TRUE))
          ),
          
          # optional: how many PCs to visualize (top-N by revenue)
          conditionalPanel(
            condition = "input.posq_show_pc == true",
            fluidRow(
              column(3, sliderInput("posq_pc_topn", "Top PC# (by Revenue):", min = 5, max = 500, value = 10, step = 1))
            )
          ),

          # --- Analysis Guidance Notes ---
          conditionalPanel(
            condition = "input.posq_show_guidance == true",
            tags$hr(),
            fluidRow(
              column(12,
                tags$div(
                  style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin-bottom: 20px;",
                  tags$h4("Suggested Filter Actions", style = "margin-top: 0; color: #007bff;"),
                  tags$ul(
                    tags$li(tags$strong("For a narrower view:"), " Select a smaller year range (e.g., single year instead of multiple years)"),
                    tags$li(tags$strong("Focus your analysis:"), " Filter by Sales Person, Product Category, or PC# as needed"),
                    tags$li(tags$strong("Lookup PC#:"), " PC# for CED Greentech can be looked up in the GT PC# Table tab"),
                    tags$li(tags$strong("Find lost business:"), " Set the Lost Business filter to identify customers who recently stopped buying (", tags$em("suggested: 1-3 months"), ")"),
                    tags$li(tags$strong("Find new installers:"), " Set the First Purchase Date range to identify recently acquired customers")
                  )
                )
              )
            )
          ),

          # --- one conditional block per plot ---
          conditionalPanel(
            condition = "input.posq_show_category == true",
            fluidRow(
              column(12, h4("Monthly Revenue by Product Category")),
              column(12, plotly::plotlyOutput("posq_plot_category", height = "360px"))
            ),
            tags$hr()
          ),
          conditionalPanel(
            condition = "input.posq_show_distributor == true",
            fluidRow(
              column(12, h4("Monthly Revenue by Distributor")),
              column(12, plotly::plotlyOutput("posq_plot_distributor", height = "360px"))
            ),
            tags$hr()
          ),
          conditionalPanel(
            condition = "input.posq_show_rep == true",
            fluidRow(
              column(12, h4("Monthly Revenue by Rep")),
              column(12, plotly::plotlyOutput("posq_plot_rep", height = "360px"))
            ),
            tags$hr()
          ),
          conditionalPanel(
            condition = "input.posq_show_pc == true",
            fluidRow(
              column(12, h4(textOutput("pc_top_n_title"))),
              column(12, plotly::plotlyOutput("posq_plot_pc", height = "360px"))
            ),
            tags$hr()
          ),
          br(),
          shinycssloaders::withSpinner(DT::dataTableOutput("table"))
        )
      ),
      tabPanel(
        "POS Row by Row",
        mainPanel(
          div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
          fluidRow(
            column(2,
                   selectInput("rep_all", "Sales Person:",
                               c("All", sort(unique(as.character(NewDT3$Rep)))))
            ),
            column(2,
                   selectInput("ship_to_state_all", "Ship To State:",
                               c("All", sort(unique(as.character(NewDT3$State)))))
            ),
            column(2,
                   selectInput("dist_code_all", "Dist Code:",
                               c("All", sort(unique(as.character(All_POS$Dist_Code)))))
            ),
            column(2,
                   selectInput("year_all", "Year:",
                               c("All", sort(unique(as.integer(lubridate::year(NewDT3$Monthly))))))
            ),
            column(2,
                   selectInput("PC_all", "PC #:",
                               c("All", sort(unique(as.character(NewDT3$`PC #`)))))
            ),
            column(2,
                   selectInput("category_all", "Product Category:",
                               c("All", sort(unique(as.character(Part_Number_List$Category)))))
            ),
            column(2,
                   selectInput("item_all", "Item:",
                               selected = "All",
                               c("All", sort(unique(as.character(All_POS$Item)))),multiple = T)
            ),
            column(2,
                   selectInput("description_all", "Description:",
                               selected = "All",
                               c("All", sort(unique(as.character(All_POS2$Description)))),multiple = T)
            )
          ),
          fluidRow(
            column(12,
                   shinycssloaders::withSpinner(DT::dataTableOutput("all_table")))
          )
        )
      ),
      tabPanel(
        "GT PC# Table",
        mainPanel(
          fluidRow(
            column(4,
                   selectInput("rep2",
                               "Sales Person:",
                               c("All",
                                 unique(as.character(PCdata$Rep))[order(unique(as.character(PCdata$Rep)))]))
            ),
            column(4,
                   selectInput("state",
                               "State:",
                               c("All",
                                 unique(as.character(PCdata$State))[order(unique(as.character(PCdata$State)))]))
            ),
            column(4,
                   selectInput("division",
                               "Division:",
                               c("All",
                                 unique(as.character(PCdata$Division))[order(unique(as.character(PCdata$Division)))]))
            ),
            column(4,
                   selectInput("type",
                               "Type:",
                               c("All",
                                 unique(as.character(PCdata$Type))[order(unique(as.character(PCdata$Type)))]))
            )
          ),
          DT::dataTableOutput("PCtable")
        )
      ),
      tabPanel(
        "Distributor Inventory Tracker",
        sidebarLayout(
          sidebarPanel(
            selectInput("di_display", "Display By:", choices = c("Category", "Item")),
            selectInput("di_metric", "Metric:", choices = c( "On Hand Value","On Hand Qty")),
            selectInput("di_distributor", "Distributor:", choices = c("All", unique(inv_dt$Distributor))),
            selectInput("di_rep", "Rep:", choices = c("All", unique(inv_dt$Rep))),
            uiOutput("dynamic_di_state"),
            uiOutput("dynamic_di_pc_name"),
            uiOutput("dynamic_di_pc"),
            uiOutput("dynamic_di_item"),
            dateRangeInput("di_date_range", "Date Range:", start = min(inv_dt$Date), end = max(inv_dt$Date))
          ),
          mainPanel(
            plotlyOutput("di_inventory_plot"),
            DT::dataTableOutput("di_inventory_table")
          )
        )
      ),
      tabPanel(
        "Cooper Branch Table",
        mainPanel(
          fluidRow(
            column(4,
                   selectInput("rep3",
                               "Sales Person:",
                               c("All",
                                 unique(as.character(Cooperdata$Rep))[order(unique(as.character(Cooperdata$Rep)))]))
            ),
            column(4,
                   selectInput("state3",
                               "State:",
                               c("All",
                                 unique(as.character(Cooperdata$State))[order(unique(as.character(Cooperdata$State)))]))
            )
          ),
          DT::dataTableOutput("CPtable")
        )
      ),
      tabPanel(
        "Wesco Branch Table",
        mainPanel(
          fluidRow(
            column(4,
                   selectInput("rep5",
                               "Sales Person:",
                               c("All",
                                 unique(as.character(wesco_branches$Rep))[order(unique(as.character(wesco_branches$Rep)))]))
            ),
            column(4,
                   selectInput("state5",
                               "State/Province:",
                               c("All",
                                 unique(as.character(wesco_branches$`State/Province`))[order(unique(as.character(wesco_branches$`State/Province`)))]))
            ),
            column(4,
                   selectInput("country5",
                               "Country:",
                               c("All",
                                 unique(as.character(wesco_branches$Country))[order(unique(as.character(wesco_branches$Country)))]))
            ),
            column(4,
                   selectInput("branch5",
                               "Branch ID:",
                               c("All", unique(as.character(wesco_branches$`Branch ID`))[order(unique(as.character(wesco_branches$`Branch ID`)))]))
            )
          ),
          DT::dataTableOutput("Wescotable")
        )
      ),
      tabPanel(
        "POS and Ohm Analytics Screener",
        sidebarLayout(
          sidebarPanel(
            selectInput("state_installer", "State Filter:", choices = c("All", unique(ohm_data$State)), selected = "All"),
            selectInput("filter_metric", "Installation Volume in:",
                        choices = list("KW" = "Kw", "Project Count (PC)" = "PC"),
                        selected = "Kw"),
            uiOutput("dynamic_slider"),
            checkboxInput("rt_customer", "RT POS Customer", value = FALSE),
            selectInput("pos_ohm_input_category", "Revenue Product Category:",
                        choices = c(unique(pos_data$Category)),
                        selected = "All"),
            uiOutput("installer_list_ui")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Installation Trends", plotlyOutput("install_trend_plot"),DT::dataTableOutput("ohm_installer_quarterly_avg_table")),
              tabPanel("Installer Contacts", tableOutput("installer_contacts_table"))
            )
          )
        )
      ),
      tabPanel(
        "Invoice Revenue Insights",
        mainPanel(
          fluidRow(
            column(4,
                   selectInput("rep4",
                               "Sales Person:",
                               c("All",
                                 unique(as.character(Invoice_data$Rep))))
            ),
            column(4,
                   selectInput("state4",
                               "State:",
                               c("All",
                                 unique(as.character(Invoice_data$State))))
            ),
            column(4,
                   selectInput("category4",
                               "Product Category:",
                               c("All",
                                 unique(as.character(Part_Number_List$Category))))
            ),
            column(4,
                   selectInput("time_aggregation",
                               "Date Agg Freq",
                               choices = c("Monthly", "Weekly")),
            ),
            column(4,
                   selectInput("forecast_method",
                               "Forecast Method",
                               choices = c("ARIMA", "TBATS", "ETS", "None")),
            ),
            column(4,
                   selectInput("customer_name_inv",
                               "Customer Name",
                               selected ="All",
                               choices = c("All",sort(unique(Invoice_data$Customer_Name)))),
            )
          ),
          fluidRow(
            column(12, shinycssloaders::withSpinner(plotlyOutput("sales_forecast_viz"))),
            fluidRow(
              column(6,
                     strong(h3("Previous Comp and Forecasts")),
                     tableOutput("sales_forecast_table")
              ),
              column(6,
                     strong(h3("Most Recent Invoices")),
                     shinycssloaders::withSpinner(DT::dataTableOutput("invoice_data_viz2"))
              )
            )
          )
        )
      ),
      !!!admin_tab(),  # Note the triple bang
      
      # UI TAB: About Section ----
      tabPanel(
        "About",
        mainPanel(
          h3("RT POS Analytics Dashboard"),
          br(),
          h4("Distributor Code Reference:"),
          p(
            strong("Dist_Code:"), br(),
            "1 - CED Greentech", br(),
            "2 - BayWa r.e. USA", br(),
            "3 - Krannich", br(),
            "4 - US Renewable Energy", br(),
            "5 - Cooper", br(),
            "6 - Soligent", br(),
            "7 - Wesco USA", br(),
            "8 - Wesco Canada"
          ),
          br(),
          h4("Dashboard Overview:"),
          p(
            "This dashboard provides comprehensive POS (Point of Sale) analytics for RT products across multiple distributors and regions.",
            "Use the various tabs to analyze sales data, customer trends, inventory levels, and installer metrics."
          ),
          br(),
          h3("Tab Guides & Filter Options"),
          br(),
          h4("1. POS Quarterly Rev Tab"),
          p(
            strong("Purpose:"), " Analyze quarterly revenue trends by customer, product category, distributor, and territory."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("Year:"), " Select one or multiple years to analyze. Choose multiple years to see trends across time periods."),
            tags$li(strong("Category:"), " Filter by product category (MINI, APEX, C-MOUNT, Accessories, Tools, Bolts, Screws, etc.) or select 'All' for comprehensive view."),
            tags$li(strong("Rep:"), " Filter by sales representative to view their assigned accounts and performance."),
            tags$li(strong("Ship To State:"), " Filter by customer ship-to state to analyze regional performance."),
            tags$li(strong("Distributor (Dist_Code):"), " Filter by specific distributor to see sales through that channel."),
            tags$li(strong("PC #:"), " Filter by project coordinator number for detailed project tracking."),
            tags$li(strong("Lost Business Filter:"), " Set number of months (0-12) to identify customers who haven't purchased recently. At 0, shows all customers. At 3, shows customers with no purchases in the last 3 months.")
          ),
          p(
            strong("Example Use Case 1:"), " To identify at-risk customers in California for CED Greentech:"
          ),
          tags$ul(
            tags$li("Set Ship To State: California"),
            tags$li("Set Distributor: 1 (CED Greentech)"),
            tags$li("Set Lost Business Filter: 3 months"),
            tags$li("Result: View customers in CA buying through CED who haven't purchased in 3+ months, sorted by their Q2 2025 revenue to prioritize outreach")
          ),
          p(
            strong("Example Use Case 2:"), " To analyze APEX product performance across all years for a specific rep:"
          ),
          tags$ul(
            tags$li("Set Year: Select multiple years (2022, 2023, 2024, 2025)"),
            tags$li("Set Category: APEX"),
            tags$li("Set Rep: Your rep name"),
            tags$li("Result: View year-over-year APEX sales trends for all your customers with quarterly breakdowns")
          ),
          p(
            strong("Interactive Feature:"), " Click on any customer name in the table to open a detailed modal showing their monthly revenue by product category."
          ),
          br(),
          h4("2. POS Row by Row Tab"),
          p(
            strong("Purpose:"), " View granular, transaction-level POS data for detailed analysis of individual sales records."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("Sales Person:"), " Filter by sales rep to view their transaction details."),
            tags$li(strong("Ship To State:"), " Filter by customer state."),
            tags$li(strong("Dist Code:"), " Filter by distributor code."),
            tags$li(strong("Year:"), " Select specific year to analyze."),
            tags$li(strong("PC #:"), " Filter by project coordinator number (can copy PC# from GT PC# Table)."),
            tags$li(strong("Product Category:"), " Filter by product category."),
            tags$li(strong("Item:"), " Filter by specific part numbers (supports multiple selections)."),
            tags$li(strong("Description:"), " Filter by product description (supports multiple selections).")
          ),
          p(
            strong("Example Use Case:"), " To analyze all MINI product sales in California for a specific PC:"
          ),
          tags$ul(
            tags$li("Set Ship To State: California"),
            tags$li("Set Product Category: MINI"),
            tags$li("Copy a PC# from the GT PC# Table"),
            tags$li("Paste the PC# in the PC # filter"),
            tags$li("Result: See every transaction for that PC's MINI product purchases in California, including dates, quantities, and pricing")
          ),
          br(),
          h4("3. GT PC# Table Tab"),
          p(
            strong("Purpose:"), " Reference table of CED Greentech Project Coordinators with their territories and contact information."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("Sales Person:"), " Filter by RT sales rep assigned to the territory."),
            tags$li(strong("State:"), " Filter by state coverage."),
            tags$li(strong("Division:"), " Filter by CED Greentech division."),
            tags$li(strong("Type:"), " Filter by PC type or category.")
          ),
          p(
            strong("Workflow Tip:"), " Use this table to find relevant PC numbers, then copy those PC# values into the POS Quarterly Rev or POS Row by Row tabs to deep dive into revenue and transaction-level data for specific project coordinators."
          ),
          p(
            strong("Example Use Case:"), " To identify PCs in Texas and analyze their revenue:"
          ),
          tags$ul(
            tags$li("Set State: Texas in the GT PC# Table"),
            tags$li("Note the PC# values from the table (e.g., 12345)"),
            tags$li("Navigate to POS Quarterly Rev tab"),
            tags$li("Set PC #: 12345"),
            tags$li("Result: View quarterly revenue breakdown for that specific PC, identifying top customers and product categories")
          ),
          br(),
          h4("4. Cooper Branch Table Tab"),
          p(
            strong("Purpose:"), " Reference table of Cooper Electric branch locations with contact information for follow-up based on revenue analytics."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("Sales Person:"), " Filter by assigned RT sales rep."),
            tags$li(strong("State:"), " Filter branches by state.")
          ),
          p(
            strong("Workflow Tip:"), " After identifying opportunities in POS Quarterly Rev or POS Row by Row tabs (e.g., declining sales at a specific branch), use this table to find the branch contact information for targeted outreach."
          ),
          p(
            strong("Example Use Case:"), " To follow up with Cooper branches showing declining sales:"
          ),
          tags$ul(
            tags$li("In POS Quarterly Rev, set Distributor: 5 (Cooper) and Lost Business Filter: 3 months"),
            tags$li("Identify Cooper branches/customers with declining sales"),
            tags$li("Navigate to Cooper Branch Table"),
            tags$li("Search for the branch by state or name"),
            tags$li("Result: Access branch contact details to reach out and address the sales decline")
          ),
          br(),
          h4("5. Wesco Branch Table Tab"),
          p(
            strong("Purpose:"), " Reference table of Wesco branch locations (USA and Canada) with contact information for follow-up based on revenue analytics."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("Sales Person:"), " Filter by assigned RT sales rep."),
            tags$li(strong("State/Province:"), " Filter branches by state or province."),
            tags$li(strong("Country:"), " Filter by USA or Canada."),
            tags$li(strong("Branch ID:"), " Filter by specific Wesco branch ID.")
          ),
          p(
            strong("Workflow Tip:"), " After identifying opportunities in POS Quarterly Rev or POS Row by Row tabs (e.g., high-performing branches or branches needing attention), use this table to find branch contact information for follow-up."
          ),
          p(
            strong("Example Use Case:"), " To follow up with top-performing Wesco branches:"
          ),
          tags$ul(
            tags$li("In POS Quarterly Rev, set Distributor: 7 (Wesco USA)"),
            tags$li("Sort by highest quarterly revenue to identify top branches"),
            tags$li("Navigate to Wesco Branch Table"),
            tags$li("Search for the branch by Branch ID or State"),
            tags$li("Result: Access branch contact details to strengthen relationships with high-performers")
          ),
          br(),
          h4("6. POS and Ohm Analytics Screener Tab"),
          p(
            strong("Purpose:"), " Screen and analyze solar installers based on their installation capacity and identify potential new customers."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("State:"), " Filter installers by state to focus on your territory."),
            tags$li(strong("RT POS Customer:"), " Toggle to show only installers who are already RT customers or only those who aren't."),
            tags$li(strong("Filter Metric:"), " Choose between 'Kw' (kilowatts installed) or 'PC' (project count) to screen installers."),
            tags$li(strong("Install Kw Slider:"), " When Kw metric selected, set minimum and maximum quarterly average kilowatts to filter installer size."),
            tags$li(strong("Install PC Slider:"), " When PC metric selected, set minimum and maximum quarterly average project count to filter installer activity level."),
            tags$li(strong("Installer to Screen:"), " After filtering, select a specific installer from the dropdown to view their detailed analytics and trends.")
          ),
          p(
            strong("Example Use Case 1:"), " To find medium-sized non-customer installers in Texas for prospecting:"
          ),
          tags$ul(
            tags$li("Set State: Texas"),
            tags$li("Set RT POS Customer: FALSE (unchecked)"),
            tags$li("Set Filter Metric: Kw"),
            tags$li("Adjust Install Kw slider: 100-500 Kw (targets medium-sized installers)"),
            tags$li("Review the filtered list and select an installer to view their installation trends"),
            tags$li("Result: Identify active medium-sized installers in Texas who aren't yet RT customers for targeted outreach")
          ),
          p(
            strong("Example Use Case 2:"), " To analyze your existing high-volume customer installers:"
          ),
          tags$ul(
            tags$li("Set RT POS Customer: TRUE (checked)"),
            tags$li("Set Filter Metric: PC"),
            tags$li("Adjust Install PC slider: 20+ projects per quarter"),
            tags$li("Select State: All (to see across all territories)"),
            tags$li("Select specific installers to view their quarterly trends and seasonal patterns"),
            tags$li("Result: Monitor your top-performing customer installers and anticipate their purchasing patterns")
          ),
          br(),
          h4("7. Invoice Revenue Insights Tab"),
          p(
            strong("Purpose:"), " Forecast future revenue trends and analyze recent invoice data with advanced time-series forecasting."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("Sales Person:"), " Filter by sales rep to view their revenue forecasts."),
            tags$li(strong("State:"), " Filter by customer state."),
            tags$li(strong("Product Category:"), " Filter by product category."),
            tags$li(strong("Date Agg Freq:"), " Choose between Monthly or Weekly aggregation for trend analysis."),
            tags$li(strong("Forecast Method:"), " Select forecasting algorithm (ARIMA, TBATS, ETS, or None)."),
            tags$li(strong("Customer Name:"), " Filter by specific customer for detailed forecasting.")
          ),
          p(
            strong("Example Use Case:"), " To forecast next quarter's revenue for APEX products in your territory:"
          ),
          tags$ul(
            tags$li("Set Sales Person: Your name"),
            tags$li("Set Product Category: APEX"),
            tags$li("Set Date Agg Freq: Monthly"),
            tags$li("Set Forecast Method: ARIMA (for seasonal data)"),
            tags$li("Result: View historical trends with projected revenue for upcoming months, plus recent invoice details")
          ),
          br(),
          h4("8. Distributor Inventory Tracker Tab"),
          p(
            strong("Purpose:"), " Monitor inventory levels and on-hand values across all distributor locations."
          ),
          p(
            strong("Available Filters:")
          ),
          tags$ul(
            tags$li(strong("Display By:"), " Choose between Category or Item view."),
            tags$li(strong("Metric:"), " Select 'On Hand Value' (dollar amount) or 'On Hand Qty' (unit count)."),
            tags$li(strong("Distributor:"), " Filter by specific distributor."),
            tags$li(strong("Rep:"), " Filter by sales rep territory."),
            tags$li(strong("State:"), " Filter by state (dynamic based on other selections)."),
            tags$li(strong("PC Name:"), " Filter by project coordinator name."),
            tags$li(strong("PC #:"), " Filter by project coordinator number."),
            tags$li(strong("Item:"), " Filter by specific part numbers."),
            tags$li(strong("Date Range:"), " View inventory snapshots across different time periods.")
          ),
          p(
            strong("Example Use Case:"), " To track MINI product inventory trends at CED Greentech:"
          ),
          tags$ul(
            tags$li("Set Display By: Category"),
            tags$li("Set Metric: On Hand Qty"),
            tags$li("Set Distributor: 1 (CED Greentech)"),
            tags$li("Filter to MINI category in the table or item filter"),
            tags$li("Result: View historical inventory trends and identify if stock levels are increasing or decreasing")
          ),
          br(),
          h4("Recommended Workflows:"),
          tags$ul(
            tags$li(strong("Deep Dive Workflow:"), " Start in GT PC# Table → Copy PC# → Paste into POS Quarterly Rev or POS Row by Row → Analyze revenue/transactions for that PC."),
            tags$li(strong("Follow-up Workflow:"), " Identify opportunities in POS Quarterly Rev (e.g., declining customers) → Go to Cooper/Wesco Branch Tables → Find branch contact info → Reach out for follow-up."),
            tags$li(strong("Prospecting Workflow:"), " Use POS and Ohm Analytics Screener to find non-customer installers → Cross-reference with branch tables for distributor contacts → Coordinate outreach with distributor partners.")
          ),
          br(),
          h4("General Tips:"),
          tags$ul(
            tags$li("Start with broad filters and gradually narrow down to identify specific insights."),
            tags$li("Click on customer names in POS Quarterly Rev to see detailed monthly product breakdowns."),
            tags$li("When using Lost Business Filter, the table automatically sorts by the relevant quarter to highlight accounts that need attention."),
            tags$li("Combine State and Rep filters to analyze regional performance within your territory."),
            tags$li("Use multiple year selection to spot growth trends and seasonal patterns."),
            tags$li("Copy PC# values from GT PC# Table to use as filters in other tabs for focused analysis."),
            tags$li("Use Branch Tables to find contact information after identifying opportunities in revenue analytics."),
            tags$li("In the Ohm Analytics tab, if visualizations don't appear, make sure you've selected an Installer to Screen from the dropdown.")
          ),
          br(),
          h4("Admin Features:"),
          p(
            strong("Note:"), " CSV download functionality is available for Admin Team access only."
          ),
          br(),
          h4("Contact & Support:"),
          p(
            "Please contact Genki, Milton, or Yukiko if you have any questions or need assistance with the dashboard."
          )
        )
      )
    )
  })
  
  # --- UPDATED: unify resets to use All_POS$Date consistently
  observeEvent(input$reset_purchase_range, {
    min_date <- min(All_POS$Date, na.rm = TRUE)
    max_date <- max(All_POS$Date, na.rm = TRUE)
    
    updateDateRangeInput(session, "last_purchase_range",  start = min_date, end = max_date)
    updateDateRangeInput(session, "first_purchase_range", start = min_date, end = max_date)
  })
  
  # REACTIVE: POS Quarterly/Monthly Rev Table ----
  # UPDATED: Nov 7, 2025
  output$table <- DT::renderDataTable(
    DT::datatable({
      req(credentials()$user_auth)
      
      if (!is.null(input$view_toggle) && input$view_toggle == TRUE) {
        #==============================#
        # MONTHLY VIEW                 #
        #==============================#
        
        monthly_data <- copy(POS_Agg)
        
        if (!is.null(input$category) && input$category != "All") {
          monthly_data <- monthly_data[Category == input$category]
        }
        if (!is.null(input$rep) && input$rep != "All") {
          valid_custs <- unique(NewDT3[Rep == input$rep]$Cust_ID)
          monthly_data <- monthly_data[Cust_ID %in% valid_custs]
        }
        if (!is.null(input$ship_to_state) && length(input$ship_to_state) > 0 && !("All" %in% input$ship_to_state)) {
          valid_custs <- unique(NewDT3[State %in% input$ship_to_state]$Cust_ID)
          monthly_data <- monthly_data[Cust_ID %in% valid_custs]
        }
        if (!is.null(input$dist_code) && input$dist_code != "All") {
          monthly_data <- monthly_data[Dist_Code == input$dist_code]
        }
        if (!is.null(input$`PC`) && input$`PC` != "All") {
          valid_custs <- unique(NewDT3[`PC #` == input$`PC`]$Cust_ID)
          monthly_data <- monthly_data[Cust_ID %in% valid_custs]
        }
        if (!"Revenue" %in% colnames(monthly_data)) {
          monthly_data[, Revenue := as.numeric(gsub("\\$", "", Price_ea)) * Qty]
        }

        # Get unique metadata for each Cust_ID from NewDT3
        # Since Cust_ID is unique, each should have only one combination of these fields
        cust_metadata <- unique(NewDT3[, .(Cust_ID, Customer, `PC #`, State, Rep, Dist_Code)])

        # Aggregate revenue by Cust_ID and Monthly ONLY
        monthly_agg <- monthly_data[, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(Cust_ID, Monthly)]

        # Merge metadata back in (one row per Cust_ID)
        monthly_agg <- merge(monthly_agg, cust_metadata, by = "Cust_ID", all.x = TRUE)
        
        if (nrow(monthly_agg) == 0) {
          # No rows at this point — return the custom message
          validate(need(FALSE, "No sale under the filter settings applied"))
        } else {
          
          #==============================#
          # BACKFILL missing Cust_ID×Month
          #==============================#
          unique_cust_ids <- unique(monthly_agg$Cust_ID)
          all_months <- sort(unique(POS_Agg$Monthly))
          
          complete_skeleton <- CJ(Cust_ID = unique_cust_ids, Monthly = all_months)
          existing_combos <- unique(monthly_agg[, .(Cust_ID, Monthly)])
          setkey(complete_skeleton, Cust_ID, Monthly)
          setkey(existing_combos, Cust_ID, Monthly)
          missing_combos <- complete_skeleton[!existing_combos]
          
          # Reuse the same cust_metadata from above (already unique per Cust_ID)
          missing_combos <- merge(missing_combos, cust_metadata, by = "Cust_ID", all.x = TRUE)
          missing_combos[, Revenue := 0]
          
          monthly_agg <- rbindlist(list(monthly_agg, missing_combos), use.names = TRUE)
          
          # Apply year filter now (post-backfill)
          if (!is.null(input$year) && length(input$year) > 0) {
            monthly_agg <- monthly_agg[lubridate::year(Monthly) %in% input$year]
          }
          
          data2 <- dcast(monthly_agg, Cust_ID + Customer + Dist_Code + `PC #` + State + Rep ~ Monthly, value.var = "Revenue", fill = 0)
          
          last_months <- monthly_data[
            !is.na(Qty) & Qty > 0,
            .(`Last Purchase Date` = max(Date), `First Purchase Date` = min(Date)),
            by = .(Cust_ID, Customer)
          ]
          data2 <- merge(data2, last_months[, .(Cust_ID, `Last Purchase Date`, `First Purchase Date`)], by = "Cust_ID", all.x = TRUE)
          
          month_cols <- sort(grep("^20[0-9]{2}-", colnames(data2), value = TRUE))
          data2[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = month_cols]

          # Remove rows where Total = 0
          data2 <- data2[Total != 0]

          base_cols <- c("Cust_ID", "Customer", "State", "Rep", "Dist_Code", "PC #", "Last Purchase Date", "First Purchase Date")
          data2 <- data2[, c(base_cols[base_cols %in% colnames(data2)], month_cols, "Total"), with = FALSE]
          
          setnames(data2, "State", "Ship To State")
          data2[, `PC #` := as.character(`PC #`)]
          data2[, `PC #2` := `PC #`]
          
          data2 <- data2[order(data2$Total, decreasing = TRUE)]
          
          most_recent_month <- max(monthly_data$Monthly)
          if (input$lost_business_filter > 0) {
            cutoff_date <- seq(most_recent_month, length = 2, by = paste0("-", input$lost_business_filter, " months"))[2]
            before_counts <- monthly_data[Date <= cutoff_date, .(Before_Count = .N), by = Customer]
            after_counts  <- monthly_data[Date > cutoff_date, .(After_Count  = .N), by = Customer]
            lost_customers_info <- merge(before_counts, after_counts, by = "Customer", all.x = TRUE)
            lost_customers_info[is.na(After_Count), After_Count := 0]
            lost_customers_info[, Lost_Business := Before_Count > 0 & After_Count == 0]
            lost_customers <- lost_customers_info[Lost_Business == TRUE]
            data2 <- data2[Customer %in% lost_customers$Customer]
          }
          
          # --- NEW: enforce safety filter on wide table
          data2 <- .final_table_filter(
            data2,
            input$rep,
            input$ship_to_state,
            input$dist_code,
            input$`PC`
          )
          
          # --- NEW: store pre-HTML wide table for modal lookups
          main_table_data(data.table::copy(data2))
          
          # --- NEW: zero row -> custom message
          if (nrow(data2) == 0) {
            validate(need(FALSE, "No sale under the filter settings applied"))
          }
          
          # Apply date range filters (after storing main_table_data)
          last_range  <- as.Date(input$last_purchase_range)
          first_range <- as.Date(input$first_purchase_range)
          data2 <- data2[
            `Last Purchase Date`  >= last_range[1]  & `Last Purchase Date`  <= lubridate::ceiling_date(last_range[2]) - 1 &
              `First Purchase Date` >= first_range[1] & `First Purchase Date` <= lubridate::ceiling_date(first_range[2]) - 1
          ]
          
          # If date filter knocks everything out, still return the message
          if (nrow(data2) == 0) {
            validate(need(FALSE, "No sale under the filter settings applied"))
          }
          
          if ("PC #2" %in% colnames(data2)) data2[, `PC #2` := NULL]
          if ("Customer_raw" %in% colnames(data2)) data2[, Customer_raw := NULL]
          colnames(data2) <- gsub("_", " ", colnames(data2))
          
          if (!is.null(input$year) && length(input$year) > 0) {
            all_cols <- colnames(data2)
            base_cols <- c("Cust ID", "Customer", "Ship To State", "Rep", "Dist Code", "PC #",
                           "Last Purchase Date", "First Purchase Date")
            cols_to_keep <- base_cols[base_cols %in% all_cols]
            year_cols <- c()
            for (yr in sort(input$year)) {
              year_pattern <- paste0("^", yr, "-")
              matching_cols <- grep(year_pattern, all_cols, value = TRUE)
              year_cols <- c(year_cols, matching_cols)
            }
            if ("Total" %in% all_cols) year_cols <- c(year_cols, "Total")
            cols_to_keep <- c(cols_to_keep, year_cols)
            data2 <- data2[, cols_to_keep, with = FALSE]
          }
          
          month_cols <- grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", colnames(data2), value = TRUE)

          # Recalculate Total based on remaining month columns (after year filtering)
          if (length(month_cols) > 0 && "Total" %in% colnames(data2)) {
            data2[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = month_cols]
          }

          if (length(month_cols)) {
            if (input$lost_business_filter == 0) {
              most_recent_col <- month_cols[length(month_cols)]
              if (most_recent_col %in% colnames(data2)) {
                data2 <- data2[order(-data2[[most_recent_col]])]
              }
            } else {
              cutoff_date <- seq(most_recent_month, length = 2, by = paste0("-", input$lost_business_filter, " months"))[2]
              target_month_col <- format(cutoff_date, "%Y-%m-01")
              if (target_month_col %in% colnames(data2)) {
                data2 <- data2[order(-data2[[target_month_col]])]
              } else {
                total_col <- grep("Total$", colnames(data2), value = TRUE)
                if (length(total_col)) {
                  data2 <- data2[order(-data2[[total_col[1]]])]
                }
              }
            }
          }

          # Clickable action links (after all filters)
          data2 <- create_action_links(data2)

          # Final filter: Remove any rows where Total = 0 (after link creation)
          if ("Total" %in% colnames(data2)) {
            data2 <- data2[Total != 0]
          }
        }
        
      } else {
        #==============================#
        # QUARTERLY VIEW               #
        #==============================#
        
        data2 <- load_aggregated_data(
          input$category,
          input$year,
          credentials()$info[["permissions"]],
          credentials()$info[["user"]]
        )
        data4 <- get_newdt3_data(credentials()$info[["permissions"]], credentials()$info[["user"]])
        
        data2 <- apply_base_filters(data2, input$rep, input$ship_to_state, input$dist_code, input$`PC`)
        data2 <- data2[order(data2[[colnames(data2)[(grep("Total", colnames(data2)) - 1)]]], decreasing = TRUE)]
        
        most_recent_month <- max(POS_Agg$Monthly)
        if(input$category != "All"){
          filtered_pos <- POS_Agg[Category %in% input$category]
        }else{
          filtered_pos <- POS_Agg
        }
        
        last_months <- filtered_pos[
          !is.na(Qty) & Qty > 0,
          .(`Last Purchase Date` = max(Date), `First Purchase Date` = min(Date)),
          by = .(Cust_ID, Customer)
        ]
        data2 <- merge(data2, last_months[, .(Cust_ID, `Last Purchase Date`, `First Purchase Date`)], by = "Cust_ID", all.x = TRUE)
        
        if (input$lost_business_filter > 0) {
          cutoff_date <- seq(most_recent_month, length = 2, by = paste0("-", input$lost_business_filter, " months"))[2]
          before_counts <- filtered_pos[Date <= cutoff_date, .(Before_Count = .N), by = Customer]
          after_counts  <- filtered_pos[Date > cutoff_date, .(After_Count  = .N), by = Customer]
          lost_customers_info <- merge(before_counts, after_counts, by = "Customer", all.x = TRUE)
          lost_customers_info <- merge(lost_customers_info, last_months[, .(Customer, `Last Purchase Date`)], by = "Customer", all.x = TRUE)
          lost_customers_info[is.na(After_Count), After_Count := 0]
          lost_customers_info[, Lost_Business := Before_Count > 0 & After_Count == 0]
          lost_customers <- lost_customers_info[Lost_Business == TRUE]
          data2 <- data2[Customer %in% lost_customers$Customer]
        }
        
        # --- NEW: enforce safety filter on wide table
        data2 <- .final_table_filter(
          data2,
          input$rep,
          input$ship_to_state,
          input$dist_code,
          input$`PC`
        )
        
        # --- NEW: store pre-HTML wide table for modal lookups
        main_table_data(data.table::copy(data2))
        
        # --- NEW: zero row -> custom message
        if (nrow(data2) == 0) {
          validate(need(FALSE, "No sale under the filter settings applied"))
        }
        
        last_range  <- as.Date(input$last_purchase_range)
        first_range <- as.Date(input$first_purchase_range)
        data2 <- data2[
          `Last Purchase Date`  >= last_range[1]  & `Last Purchase Date`  <= lubridate::ceiling_date(last_range[2]) - 1 &
            `First Purchase Date` >= first_range[1] & `First Purchase Date` <= lubridate::ceiling_date(first_range[2]) - 1
        ]
        
        if (nrow(data2) == 0) {
          validate(need(FALSE, "No sale under the filter settings applied"))
        }
        
        if ("PC #2" %in% colnames(data2)) data2[, `PC #2` := NULL]
        if ("Customer_raw" %in% colnames(data2)) data2[, Customer_raw := NULL]
        
        colnames(data2) <- gsub("_", " ", colnames(data2))
        
        if (!is.null(input$year) && length(input$year) > 0) {
          all_cols <- colnames(data2)
          base_cols <- c("Cust ID", "Customer", "Ship To State", "Rep", "Dist Code", "PC #",
                         "Last Purchase Date", "First Purchase Date")
          cols_to_keep <- base_cols[base_cols %in% all_cols]
          
          year_cols <- c()
          for (yr in sort(input$year)) {
            year_pattern <- paste0("^", yr, " ")
            matching_cols <- grep(year_pattern, all_cols, value = TRUE)
            year_cols <- c(year_cols, matching_cols)
          }
          
          if ("Total" %in% all_cols) {
            if (length(input$year) == 1) {
              new_total_name <- paste0(input$year[1], " Total")
              setnames(data2, "Total", new_total_name)
              year_cols <- c(year_cols, new_total_name)
            } else {
              setnames(data2, "Total", "All Total")
              year_cols <- c(year_cols, "All Total")
            }
          }
          
          cols_to_keep <- c(cols_to_keep, year_cols)
          data2 <- data2[, cols_to_keep, with = FALSE]
        }
        
        q_cols <- grep("Q[1-4]", colnames(data2), value = TRUE)
        if (length(q_cols)) {
          if (input$lost_business_filter == 0) {
            latest_q_col <- max(q_cols)
            data2 <- data2[order(-data2[[latest_q_col]])]
          } else {
            cutoff_date <- seq(most_recent_month, length = 2, by = paste0("-", input$lost_business_filter, " months"))[2]
            cutoff_year <- lubridate::year(cutoff_date)
            cutoff_quarter <- lubridate::quarter(cutoff_date)
            target_q_col <- paste0(cutoff_year, " Q", cutoff_quarter)
            if (target_q_col %in% colnames(data2)) {
              data2 <- data2[order(-data2[[target_q_col]])]
            } else {
              total_col <- grep(" Total$", colnames(data2), value = TRUE)
              if (length(total_col)) {
                data2 <- data2[order(-data2[[total_col[1]]])]
              }
            }
          }
        }
        
        # Clickable action links (after all filters)
        data2 <- create_action_links(data2)

        # Final filter: Remove any rows where Total = 0 (after link creation)
        if ("Total" %in% colnames(data2)) {
          data2 <- data2[Total != 0]
        }
      }
      
      # Validate non-empty once more (covers both views uniformly)
      validate(
        need(nrow(data2) > 0, "No sale under the filter settings applied")
      )
      
      data2
    },
    escape = FALSE,
    options = list(pageLength = 100)
    ) %>%
      {
        existing_cols <- colnames(.[["x"]][["data"]])
        q_cols_to_format <- grep("Q[1-4]", existing_cols, value = TRUE)
        total_col <- grep(" Total$|^Total$", existing_cols, value = TRUE)
        month_cols_to_format <- grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", existing_cols, value = TRUE)
        cols_to_format <- c(q_cols_to_format, total_col, month_cols_to_format)
        
        if (length(cols_to_format) > 0) {
          formatRound3(., cols_to_format, "$", digits = 2)
        } else {
          .
        }
      }
  )
  
  output$table <- DT::renderDataTable({
    req(credentials()$user_auth)
    
    # ---------------------- MONTHLY VIEW ----------------------
    if (!is.null(input$view_toggle) && input$view_toggle == TRUE) {
      # 1. Start from POS_Agg and apply all filters EXCEPT year
      monthly_data <- data.table::copy(POS_Agg)
      data.table::setDT(monthly_data)
      
      # Ensure Monthly exists and is a Date
      if (!"Monthly" %in% names(monthly_data)) {
        monthly_data[, Monthly := lubridate::floor_date(as.Date(Date), "month")]
      } else {
        monthly_data[, Monthly := as.Date(Monthly)]
      }
      
      # ---------- FILTERS ON POS_Agg (PRODUCT + REP/STATE/DIST) ----------
      
      # Category (product) filter
      if (!is.null(input$category) && input$category != "All") {
        monthly_data <- monthly_data[Category == input$category]
      }
      
      # Rep filter
      if (!is.null(input$rep) && input$rep != "All") {
        valid_custs <- unique(NewDT3[Rep == input$rep]$Cust_ID)
        monthly_data <- monthly_data[Cust_ID %in% valid_custs]
      }
      
      # Ship to state filter (multi)
      if (!is.null(input$ship_to_state) &&
          length(input$ship_to_state) > 0 &&
          !("All" %in% input$ship_to_state)) {
        valid_custs <- unique(NewDT3[State %in% input$ship_to_state]$Cust_ID)
        monthly_data <- monthly_data[Cust_ID %in% valid_custs]
      }
      
      # Dist code filter
      if (!is.null(input$dist_code) && input$dist_code != "All") {
        monthly_data <- monthly_data[Dist_Code == input$dist_code]
      }
      
      # PC filter: **MOVED** to wide table to avoid mismatch.
      # Here we do NOT filter by PC yet, only at the final wide table level.
      # if (!is.null(input$`PC`) && input$`PC` != "All") {
      #   valid_custs <- unique(NewDT3[`PC #` == input$`PC`]$Cust_ID)
      #   monthly_data <- monthly_data[Cust_ID %in% valid_custs]
      # }
      
      # If everything is filtered away already
      validate(
        need(nrow(monthly_data) > 0, "No sale under the filter settings applied")
      )
      
      # Make sure Revenue exists
      if (!"Revenue" %in% names(monthly_data)) {
        monthly_data[, Price_ea := as.numeric(gsub("[^0-9.-]", "", Price_ea))]
        monthly_data[, Qty      := as.numeric(Qty)]
        monthly_data[, Revenue  := Price_ea * Qty]
      }
      
      # 2. Unique customers (after filters, before year)
      cust_ids <- unique(monthly_data$Cust_ID)

      # 3. All unique months - FILTERED BY YEAR for memory optimization (shinyapps.io)
      # MEMORY OPTIMIZATION: Only create skeleton for selected years to reduce memory footprint
      # This prevents creating skeleton with ALL historical months (which can be 100-500MB)
      if (!is.null(input$year) && length(input$year) > 0) {
        # Filter to selected year(s) only
        all_months_full <- sort(unique(as.Date(POS_Agg$Monthly)))
        month_years <- lubridate::year(all_months_full)
        all_months <- all_months_full[month_years %in% as.integer(input$year)]
      } else {
        # If no year selected, use all months (default behavior)
        all_months <- sort(unique(as.Date(POS_Agg$Monthly)))
      }
      
      # 4. Skeleton: Cust_ID × Monthly
      skeleton <- data.table::CJ(
        Cust_ID = cust_ids,
        Monthly = all_months
      )
      
      # 5. Aggregate filtered POS_Agg to monthly (revenues)
      # Get metadata from monthly_data (POS_Agg), including Dist_Code which is not in NewDT3
      monthly_agg <- monthly_data[
        ,
        .(Revenue = sum(Revenue, na.rm = TRUE)),
        by = .(Cust_ID, Customer, Monthly, Dist_Code, `PC #`, State, Rep)
      ]
      
      # 6. Backfill: find skeleton combos that DON'T exist in monthly_agg
      existing_pairs <- unique(monthly_agg[, .(Cust_ID, Monthly)])
      data.table::setkey(skeleton, Cust_ID, Monthly)
      data.table::setkey(existing_pairs, Cust_ID, Monthly)
      missing_pairs <- skeleton[!existing_pairs]

      if (nrow(missing_pairs) > 0) {
        # Create metadata from monthly_agg (use first value per customer)
        meta_from_agg <- monthly_agg[
          ,
          .(
            Customer = first(Customer),
            State = first(State),
            Rep = first(Rep),
            Dist_Code = first(Dist_Code),
            `PC #` = first(`PC #`)
          ),
          by = Cust_ID
        ]

        missing_pairs <- merge(
          missing_pairs,
          meta_from_agg,
          by = "Cust_ID",
          all.x = TRUE
        )
        missing_pairs[, Revenue := 0]
        monthly_full <- data.table::rbindlist(
          list(monthly_agg, missing_pairs),
          use.names = TRUE,
          fill = TRUE
        )
      } else {
        monthly_full <- monthly_agg
      }
      monthly_full[is.na(`PC #`), `PC #` := 0]
      
      validate(
        need(nrow(monthly_full) > 0, "No sale under the filter settings applied")
      )
      
      # 8. Pivot wide: each month becomes a column
      data2 <- data.table::dcast(
        monthly_full,
        Cust_ID + Customer + State + Rep + Dist_Code + `PC #` ~ Monthly,
        value.var = "Revenue",
        fill = 0
      )
      
      # Last/first purchase dates (use full filtered monthly_data – not year-limited)
      last_first <- monthly_data[
        !is.na(Qty) & Qty > 0,
        .(
          `Last Purchase Date`  = max(Date),
          `First Purchase Date` = min(Date)
        ),
        by = .(Cust_ID, Customer)
      ]
      
      data2 <- merge(
        data2,
        last_first[, .(Cust_ID, `Last Purchase Date`, `First Purchase Date`)],
        by = "Cust_ID",
        all.x = TRUE
      )
      
      # ---------- Month columns & year-based column filter ----------
      month_cols <- grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", colnames(data2), value = TRUE)
      month_cols <- sort(month_cols)
      
      # Keep only month columns that match selected year(s)
      if (!is.null(input$year) && length(input$year) > 0) {
        month_years     <- lubridate::year(as.Date(month_cols))
        keep_month_cols <- month_cols[month_years %in% as.integer(input$year)]
      } else {
        keep_month_cols <- month_cols
      }
      
      # If no month columns remain after year filter → no data
      validate(
        need(length(keep_month_cols) > 0, "No sale under the filter settings applied")
      )
      
      # Total column (from *kept* month columns only)
      if (length(keep_month_cols) > 0) {
        data2[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = keep_month_cols]
      } else {
        data2[, Total := 0]
      }

      # Dynamic Total column naming based on year selection
      # Same logic as quarterly view for consistency
      total_col_name <- "Total"
      if (!is.null(input$year) && length(input$year) > 0) {
        if (length(input$year) == 1) {
          # Single year selected: "{year} Total"
          total_col_name <- paste0(input$year[1], " Total")
          data.table::setnames(data2, "Total", total_col_name)
        } else {
          # Multiple years selected: "All Total"
          total_col_name <- "All Total"
          data.table::setnames(data2, "Total", total_col_name)
        }
      }

      # Reorder columns: base → kept months → Total
      base_cols <- c(
        "Cust_ID", "Customer", "State", "Rep", "Dist_Code",
        "PC #", "Last Purchase Date", "First Purchase Date"
      )
      base_cols <- base_cols[base_cols %in% names(data2)]
      data2 <- data2[, c(base_cols, keep_month_cols, total_col_name), with = FALSE]

      # Replace $0 with empty cells in monthly columns (convert 0 to NA)
      # This makes the table cleaner - only showing actual revenue, not zeros
      if (length(keep_month_cols) > 0) {
        for (col in keep_month_cols) {
          data2[get(col) == 0, (col) := NA_real_]
        }
      }

      # Filter to keep only rows where Total for selected year(s) > 0
      # This removes customers with no revenue in the filtered year range
      data2 <- data2[get(total_col_name) > 0]

      # Rename State column for UI
      data.table::setnames(data2, "State", "Ship To State", skip_absent = TRUE)
      
      # ---------- FINAL PC FILTER ON WIDE TABLE (fixes PC mismatch) ----------
      if (!is.null(input$`PC`) && input$`PC` != "All" && "PC #" %in% names(data2)) {
        data2 <- data2[as.character(`PC #`) == as.character(input$`PC`)]
      }
      
      validate(
        need(nrow(data2) > 0, "No sale under the filter settings applied")
      )
      
      # 9. Lost business filter (based on filtered monthly_data)
      most_recent_month <- max(monthly_data$Monthly, na.rm = TRUE)
      
      if (!is.null(input$lost_business_filter) &&
          input$lost_business_filter > 0 &&
          nrow(monthly_data) > 0) {
        
        cutoff_date <- seq(
          most_recent_month,
          length = 2,
          by = paste0("-", input$lost_business_filter, " months")
        )[2]
        
        before_counts <- monthly_data[
          Date <= cutoff_date,
          .(Before_Count = .N),
          by = Customer
        ]
        after_counts <- monthly_data[
          Date > cutoff_date,
          .(After_Count = .N),
          by = Customer
        ]
        
        lost_customers_info <- merge(
          before_counts,
          after_counts,
          by = "Customer",
          all.x = TRUE
        )
        lost_customers_info[is.na(After_Count), After_Count := 0]
        lost_customers_info[, Lost_Business := Before_Count > 0 & After_Count == 0]
        
        lost_customers <- lost_customers_info[Lost_Business == TRUE, Customer]
        data2 <- data2[Customer %in% lost_customers]
      }
      
      # Date range filters
      last_range  <- as.Date(input$last_purchase_range)
      first_range <- as.Date(input$first_purchase_range)
      
      data2 <- data2[
        `Last Purchase Date`  >= last_range[1] &
          `Last Purchase Date`  <= lubridate::ceiling_date(last_range[2], "day") - 1 &
          `First Purchase Date` >= first_range[1] &
          `First Purchase Date` <= lubridate::ceiling_date(first_range[2], "day") - 1
      ]
      
      validate(
        need(nrow(data2) > 0, "No sale under the filter settings applied")
      )
      
      # Pretty column names (turn Cust_ID → Cust ID, Dist_Code → Dist Code, etc.)
      colnames(data2) <- gsub("_", " ", colnames(data2))
      
      # Sorting for monthly view:
      month_cols_now <- grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", colnames(data2), value = TRUE)
      month_cols_now <- sort(month_cols_now)
      
      if (length(month_cols_now) > 0) {
        if (is.null(input$lost_business_filter) || input$lost_business_filter == 0) {
          most_recent_col <- tail(month_cols_now, 1)
          data2 <- data2[order(-get(most_recent_col))]
        } else {
          cutoff_date <- seq(
            most_recent_month,
            length = 2,
            by = paste0("-", input$lost_business_filter, " months")
          )[2]
          target_month_col <- format(cutoff_date, "%Y-%m-01")
          
          if (target_month_col %in% colnames(data2)) {
            data2 <- data2[order(-get(target_month_col))]
          } else if ("Total" %in% colnames(data2)) {
            data2 <- data2[order(-Total)]
          }
        }
      }
      
    } else {
      # ---------------------- QUARTERLY VIEW ----------------------
      data2 <- load_aggregated_data(
        input$category,
        input$year,
        credentials()$info[["permissions"]],
        credentials()$info[["user"]]
      )
      
      data2 <- apply_base_filters(
        data2,
        input$rep,
        input$ship_to_state,
        input$dist_code,
        input$`PC`
      )
      
      # Sort by "latest" quarter column
      total_idx <- grep("Total", colnames(data2))[1]
      data2 <- data2[order(
        -data2[[colnames(data2)[total_idx - 1]]]
      )]
      
      most_recent_month <- max(POS_Agg$Monthly)
      
      if (input$category != "All") {
        filtered_pos <- POS_Agg[Category %in% input$category]
      } else {
        filtered_pos <- POS_Agg
      }
      
      last_months <- filtered_pos[
        !is.na(Qty) & Qty > 0,
        .(
          `Last Purchase Date`  = max(Date),
          `First Purchase Date` = min(Date)
        ),
        by = .(Cust_ID, Customer)
      ]
      
      data2 <- merge(
        data2,
        last_months[, .(Cust_ID, `Last Purchase Date`, `First Purchase Date`)],
        by = "Cust_ID",
        all.x = TRUE
      )
      
      # Lost business filter (same as before)
      if (!is.null(input$lost_business_filter) && input$lost_business_filter > 0) {
        cutoff_date <- seq(
          most_recent_month,
          length = 2,
          by = paste0("-", input$lost_business_filter, " months")
        )[2]
        
        before_counts <- filtered_pos[
          Date <= cutoff_date,
          .(Before_Count = .N),
          by = Customer
        ]
        after_counts <- filtered_pos[
          Date > cutoff_date,
          .(After_Count = .N),
          by = Customer
        ]
        
        lost_customers_info <- merge(before_counts, after_counts, by = "Customer", all.x = TRUE)
        lost_customers_info <- merge(
          lost_customers_info,
          last_months[, .(Customer, `Last Purchase Date`)],
          by = "Customer",
          all.x = TRUE
        )
        lost_customers_info[is.na(After_Count), After_Count := 0]
        lost_customers_info[, Lost_Business := Before_Count > 0 & After_Count == 0]
        lost_customers <- lost_customers_info[Lost_Business == TRUE, Customer]
        
        data2 <- data2[Customer %in% lost_customers]
      }
      
      # Date range filters
      last_range  <- as.Date(input$last_purchase_range)
      first_range <- as.Date(input$first_purchase_range)
      
      data2 <- data2[
        `Last Purchase Date`  >= last_range[1] &
          `Last Purchase Date`  <= lubridate::ceiling_date(last_range[2], "day") - 1 &
          `First Purchase Date` >= first_range[1] &
          `First Purchase Date` <= lubridate::ceiling_date(first_range[2], "day") - 1
      ]
      
      validate(
        need(nrow(data2) > 0, "No sale under the filter settings applied")
      )
      
      if ("PC #2" %in% colnames(data2)) data2[, `PC #2` := NULL]
      if ("Customer_raw" %in% colnames(data2)) data2[, Customer_raw := NULL]
      
      colnames(data2) <- gsub("_", " ", colnames(data2))
      
      # Year-specific column pruning (quarterly)
      if (!is.null(input$year) && length(input$year) > 0) {
        all_cols <- colnames(data2)
        base_cols <- c(
          "Cust ID", "Customer", "Ship To State", "Rep", "Dist Code",
          "PC #", "Last Purchase Date", "First Purchase Date"
        )
        base_keep <- base_cols[base_cols %in% all_cols]
        
        year_cols <- c()
        for (yr in sort(input$year)) {
          year_pattern <- paste0("^", yr, " ")
          year_cols <- c(year_cols, grep(year_pattern, all_cols, value = TRUE))
        }
        
        if ("Total" %in% all_cols) {
          if (length(input$year) == 1) {
            new_total_name <- paste0(input$year[1], " Total")
            data.table::setnames(data2, "Total", new_total_name)
            year_cols <- c(year_cols, new_total_name)
          } else {
            data.table::setnames(data2, "Total", "All Total")
            year_cols <- c(year_cols, "All Total")
          }
        }
        
        keep <- unique(c(base_keep, year_cols))
        data2 <- data2[, keep, with = FALSE]
      }
      
      # Sort by latest quarter or by quarter before cutoff if lost business used
      q_cols <- grep("Q[1-4]", colnames(data2), value = TRUE)
      if (length(q_cols)) {
        if (is.null(input$lost_business_filter) || input$lost_business_filter == 0) {
          latest_q_col <- max(q_cols)
          data2 <- data2[order(-get(latest_q_col))]
        } else {
          cutoff_date <- seq(
            most_recent_month,
            length = 2,
            by = paste0("-", input$lost_business_filter, " months")
          )[2]
          cutoff_year    <- lubridate::year(cutoff_date)
          cutoff_quarter <- lubridate::quarter(cutoff_date)
          target_q_col   <- paste0(cutoff_year, " Q", cutoff_quarter)
          
          if (target_q_col %in% colnames(data2)) {
            data2 <- data2[order(-get(target_q_col))]
          } else {
            total_col <- grep(" Total$", colnames(data2), value = TRUE)
            if (length(total_col)) {
              data2 <- data2[order(-get(total_col[1]))]
            }
          }
        }
      }
    }
    
    # ----- At this point data2 is the clean, fully-filtered wide table -----
    
    # Store a *clean* copy for use in modals (titles will match filters)
    main_table_data(data.table::copy(data2))
    
    # ----- Make Customer clickable (fires customer_click with Cust_ID / Cust ID) -----
    {
      # Handle both "Cust ID" and "Cust_ID" just in case
      cust_id_col <- NULL
      if ("Cust ID" %in% names(data2)) {
        cust_id_col <- "Cust ID"
      } else if ("Cust_ID" %in% names(data2)) {
        cust_id_col <- "Cust_ID"
      }
      
      if (!is.null(cust_id_col) && "Customer" %in% names(data2)) {
        cust_ids    <- as.character(data2[[cust_id_col]])
        cust_labels <- as.character(data2[["Customer"]])
        
        customer_links <- vapply(
          seq_along(cust_ids),
          function(i) {
            if (is.na(cust_ids[i]) || cust_ids[i] == "" || is.na(cust_labels[i])) {
              return(cust_labels[i])
            }
            as.character(
              actionLink(
                inputId = paste0("cust_", i),
                label   = cust_labels[i],
                onclick = sprintf(
                  "Shiny.setInputValue('customer_click', '%s', {priority: 'event'});",
                  cust_ids[i]
                )
              )
            )
          },
          FUN.VALUE = character(1L)
        )
        
        data2[, Customer := customer_links]
      }
    }
    
    # ----- Make PC # clickable (fires pc_click with PC #) -----
    if ("PC #" %in% colnames(data2)) {
      pc_raw <- as.character(data2[["PC #"]])
      
      pc_links <- vapply(
        seq_along(pc_raw),
        function(i) {
          val <- pc_raw[i]
          if (is.na(val) || val == "") return("")
          as.character(
            actionLink(
              inputId = paste0("pc_", i),
              label   = val,
              onclick = sprintf(
                "Shiny.setInputValue('pc_click', '%s', {priority: 'event'});",
                val
              )
            )
          )
        },
        FUN.VALUE = character(1L)
      )
      
      data2[, `PC #` := pc_links]
    }
    
    # Final safety check
    validate(
      need(nrow(data2) > 0, "No sale under the filter settings applied")
    )
    
    # Build DT and apply currency formatting
    dt <- DT::datatable(
      data2,
      escape  = FALSE,
      options = list(
        pageLength = 100,
        scrollX = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = '150px', targets = "_all")
        )
      ),
      class = 'display cell-border'
    )
    
    existing_cols        <- colnames(data2)
    q_cols_to_format     <- grep("Q[1-4]", existing_cols, value = TRUE)
    total_cols_to_format <- grep(" Total$|^Total$", existing_cols, value = TRUE)
    month_cols_to_format <- grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", existing_cols, value = TRUE)
    
    cols_to_format <- unique(c(q_cols_to_format, total_cols_to_format, month_cols_to_format))
    
    if (length(cols_to_format) > 0) {
      dt <- DT::formatCurrency(dt, columns = cols_to_format, currency = "$", digits = 2)
    }
    
    dt
  })
  
  
  # REACTIVE: Customer Click Modal ----
  # UPDATED: Uses main_table_data() to guarantee titles match visible rows
  # === CUSTOMER CLICK MODAL (ALL HISTORICAL, ALL CATEGORIES) ===
  
  observeEvent(input$customer_click, {
    req(credentials()$user_auth)
    req(input$customer_click)
    
    cust_id <- as.character(input$customer_click)
    
    # Prefer NewDT3 for title (has State / clean Customer), fallback to POS_Agg
    cust_info <- unique(NewDT3[Cust_ID == cust_id, .(Customer, State)])
    if (nrow(cust_info) == 0) {
      cust_info <- unique(POS_Agg[Cust_ID == cust_id, .(Customer, State)])
    }
    
    cust_name  <- if (nrow(cust_info) > 0) cust_info$Customer[1] else cust_id
    cust_state <- if (nrow(cust_info) > 0) cust_info$State[1]    else ""
    
    showModal(modalDialog(
      title = paste0(
        "Revenue by Product (All History): ",
        cust_name,
        if (!is.na(cust_state) && cust_state != "") paste0(" - ", cust_state) else "",
        " (", cust_id, ")"
      ),
      tableOutput("customer_history_table"),
      size      = "l",
      easyClose = TRUE
    ))
  })
  
  
  output$customer_history_table <- renderTable({
    req(credentials()$user_auth)
    req(input$customer_click)

    cust_id <- as.character(input$customer_click)

    # Use NewDT3 which has ALL date rows and ALL product columns (including zeros)
    cust_data <- data.table::copy(NewDT3)[Cust_ID == cust_id]

    if (nrow(cust_data) == 0) {
      return(data.frame(Message = "No sales history found for this customer"))
    }

    # NewDT3 has monthly data with all product category columns
    # Columns: Monthly, MINI, Bolts, Screws, APEX, Accessories, Unknown, REB, Tools, C-MOUNT, Others, PLUS, Customer, State, Cust_ID, PC #
    all_cols <- colnames(cust_data)
    exclude_cols <- c("Cust_ID", "Customer", "State", "Rep", "PC #", "Dist_Code", "Monthly")
    product_cols <- setdiff(all_cols, exclude_cols)

    # Select only date and product columns
    if ("Monthly" %in% all_cols) {
      tbl <- cust_data[, c("Monthly", product_cols), with = FALSE]
      # Sort most recent month first
      data.table::setorder(tbl, -Monthly)

      # Convert NA to 0 for all product columns
      for (col in product_cols) {
        tbl[is.na(get(col)), (col) := 0]
      }

      # Format date as character (YYYY-MM-DD or similar readable format)
      tbl[, Monthly := as.character(Monthly)]

      # Rename Monthly to Date
      data.table::setnames(tbl, "Monthly", "Date")

      # Reorder columns: Date first, then MINI, APEX, C-MOUNT, PLUS, then Accessories, Tools, then remaining
      priority_cols <- c("MINI", "APEX", "C-MOUNT", "PLUS", "Accessories", "Tools")
      # Get remaining columns (excluding Date and priority columns)
      remaining_cols <- setdiff(colnames(tbl), c("Date", priority_cols))
      # Filter priority_cols to only include those that exist in the table
      available_priority <- intersect(priority_cols, colnames(tbl))
      # Reorder: Date + available priority columns + remaining columns
      tbl <- tbl[, c("Date", available_priority, remaining_cols), with = FALSE]
    } else {
      # Fallback if structure is different
      return(data.frame(Message = "Data structure not recognized"))
    }

    # Currency formatting for all product category columns
    for (col in product_cols) {
      if (is.numeric(tbl[[col]])) {
        tbl[, (col) := paste0(
          "$",
          formatC(get(col), format = "f", digits = 2, big.mark = ",")
        )]
      }
    }

    tbl
  })

    
  # === FILTERED TABLE (MODAL POPUP) ===
  output$filtered_table <- renderTable({
    req(credentials()$user_auth)
    req(input$customer_click)
    
    tryCatch({
      cust_id <- input$customer_click
      pos_all_data <- copy(POS_Agg)[Cust_ID == cust_id]
      if (nrow(pos_all_data) == 0) {
        return(data.frame(Message = "No sales history found for this customer"))
      }
      if (!is.null(input$category) && input$category != "" && input$category != "All") {
        pos_all_data <- pos_all_data[Category == input$category]
        if (nrow(pos_all_data) == 0) {
          return(data.frame(Message = "No sales history under this filter condition"))
        }
      }
      if (!"Revenue" %in% colnames(pos_all_data)) {
        pos_all_data[, Revenue := as.numeric(gsub("\\$", "", Price_ea)) * Qty]
      }
      monthly_agg_all <- pos_all_data[, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(Monthly, Category)]
      min_date <- as.Date(paste0(min(lubridate::year(POS_Agg$Monthly)), "-01-01"))
      max_date <- as.Date(format(max(POS_Agg$Monthly), "%Y-%m-01"))
      all_months <- seq.Date(from = min_date, to = max_date, by = "month")
      all_categories <- unique(monthly_agg_all$Category)
      
      month_skeleton <- data.table(
        Monthly = rep(all_months, each = length(all_categories)),
        Category = rep(all_categories, times = length(all_months)),
        Revenue = 0
      )
      
      monthly_agg_final <- merge(month_skeleton, monthly_agg_all, by = c("Monthly", "Category"), all.x = TRUE, suffixes = c("_skeleton", "_actual"))
      monthly_agg_final[, Revenue := ifelse(is.na(Revenue_actual), Revenue_skeleton, Revenue_actual)]
      monthly_agg_final[, c("Revenue_skeleton", "Revenue_actual") := NULL]
      
      if (nrow(monthly_agg_final) == 0) {
        return(data.frame(Message = "No sales data available"))
      }
      
      tbl <- dcast(monthly_agg_final, Monthly ~ Category, value.var = "Revenue", fill = 0)
      expected_cols <- c("MINI", "APEX", "C-MOUNT", "Accessories", "Tools", "Bolts", "Screws", "Unknown")
      for (col in expected_cols) if (!col %in% colnames(tbl)) tbl[, (col) := 0]
      available_cols <- intersect(expected_cols, colnames(tbl))
      tbl <- tbl[, c("Monthly", available_cols), with = FALSE]
      setorder(tbl, -Monthly)
      setnames(tbl, "Monthly", "Date")
      tbl[, Date := format(as.Date(Date), "%Y-%m")]
      for (col in available_cols) {
        if (col %in% colnames(tbl)) {
          tbl[, (col) := paste0("$", formatC(get(col), format = "f", digits = 2, big.mark = ","))]
        }
      }
      tbl
    }, error = function(e) {
      data.frame(Error = paste("Error:", e$message))
    })
  })
  
  # === DEPRECATED: OLD MODAL TOGGLE LOGIC ===
  view_mode <- reactiveVal("monthly")  # Default to monthly view
  
  observeEvent(input$toggle_view, {
    if (view_mode() == "monthly") {
      view_mode("quarterly")
      updateActionButton(session, "toggle_view", label = "Switch to Monthly View")
    } else {
      view_mode("monthly")
      updateActionButton(session, "toggle_view", label = "Switch to Quarterly View")
    }
  })
  
  # === REVENUE TABLE (UNIFIED - INSIDE MODAL) ===
  output$revenue_table <- renderTable({
    req(credentials()$user_auth)
    req(input$customer_click)
    
    if (view_mode() == "monthly") {
      cust_id <- input$customer_click
      pos_filtered <- copy(POS_Agg)[Cust_ID == cust_id]
      if (!is.null(input$year) && length(input$year) > 0) {
        pos_filtered <- pos_filtered[lubridate::year(Monthly) %in% input$year]
      }
      if (!"Revenue" %in% colnames(pos_filtered)) {
        pos_filtered[, Revenue := as.numeric(gsub("\\$", "", Price_ea)) * Qty]
      }
      monthly_agg <- pos_filtered[, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(Monthly, Category)]
      tbl <- dcast(monthly_agg, Monthly ~ Category, value.var = "Revenue", fill = 0)
      expected_cols <- c("MINI", "APEX", "C-MOUNT", "Accessories", "Tools", "Bolts", "Screws", "Unknown")
      for (col in expected_cols) if (!col %in% colnames(tbl)) tbl[, (col) := 0]
      available_cols <- intersect(expected_cols, colnames(tbl))
      tbl <- tbl[, c("Monthly", available_cols), with = FALSE]
      setorder(tbl, -Monthly)
      setnames(tbl, "Monthly", "Date")
      tbl[, Date := format(as.Date(Date), "%Y-%m")]
      revenue_cols <- c("MINI", "APEX", "C-MOUNT", "Accessories", "Tools", "Bolts", "Screws", "Unknown")
      for (col in revenue_cols) if (col %in% colnames(tbl)) {
        tbl[, (col) := paste0("$", formatC(get(col), format = "f", digits = 2, big.mark = ","))]
      }
      tbl
    } else {
      data2 <- load_aggregated_data(
        input$category,
        input$year,
        credentials()$info[["permissions"]],
        credentials()$info[["user"]]
      )
      data2 <- apply_base_filters(data2, input$rep, input$ship_to_state, input$dist_code, input$`PC`)
      cust_id <- input$customer_click
      tbl <- data2[Cust_ID == cust_id]
      quarter_cols <- grep("Q[1-4]", colnames(tbl), value = TRUE)
      if (length(quarter_cols) > 0 && nrow(tbl) > 0) {
        tbl <- tbl[, ..quarter_cols]
        tbl_long <- melt(tbl, measure.vars = colnames(tbl), variable.name = "Date", value.name = "Revenue")
        tbl_long[, Date := as.character(Date)]
        setorder(tbl_long, -Date)
        tbl_long[, Revenue := paste0("$", formatC(Revenue, format = "f", digits = 2, big.mark = ","))]
        tbl_long
      } else {
        data.table(Date = character(), Revenue = character())
      }
    }
  })
  
  # === MONTHLY TABLE (INSIDE MODAL) - DEPRECATED ===
  output$monthly_table <- renderTable({
    req(credentials()$user_auth)
    cust_id <- input$customer_click
    pos_filtered <- copy(POS_Agg)[Cust_ID == cust_id]
    if (!is.null(input$year) && length(input$year) > 0) {
      pos_filtered <- pos_filtered[lubridate::year(Monthly) %in% input$year]
    }
    if (!"Revenue" %in% colnames(pos_filtered)) {
      pos_filtered[, Revenue := as.numeric(gsub("\\$", "", Price_ea)) * Qty]
    }
    monthly_agg <- pos_filtered[, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(Monthly, Category)]
    tbl <- dcast(monthly_agg, Monthly ~ Category, value.var = "Revenue", fill = 0)
    expected_cols <- c("MINI", "APEX", "C-MOUNT", "Accessories", "Tools", "Bolts", "Screws", "Unknown")
    for (col in expected_cols) if (!col %in% colnames(tbl)) tbl[, (col) := 0]
    available_cols <- intersect(expected_cols, colnames(tbl))
    tbl <- tbl[, c("Monthly", available_cols), with = FALSE]
    setorder(tbl, -Monthly)
    setnames(tbl, "Monthly", "Date")
    tbl[, Date := format(as.Date(Date), "%Y-%m")]
    revenue_cols <- c("MINI", "APEX", "C-MOUNT", "Accessories", "Tools", "Bolts", "Screws", "Unknown")
    for (col in revenue_cols) if (col %in% colnames(tbl)) {
      tbl[, (col) := paste0("$", formatC(get(col), format = "f", digits = 2, big.mark = ","))]
    }
    tbl
  })
  
  # === PC CLICK EVENT ===
  # UPDATED: Titles come from current visible table to ensure correctness
  observeEvent(input$pc_click, {
    req(credentials()$user_auth)
    req(input$pc_click)
    
    # Use visible table
    visible <- main_table_data()
    req(!is.null(visible))
    
    pc_num <- as.character(input$pc_click)
    req(pc_num %in% as.character(visible$`PC #`))
    
    row_now <- visible[`PC #` == pc_num][1]
    cust_name  <- row_now$Customer
    cust_state <- row_now$`Ship To State`
    
    if (nrow(row_now) > 0 && row_now$`Dist Code` == 1) {
      showModal(modalDialog(
        title = paste0("CED_MGR_Contacts: ", cust_name, " - ", cust_state, " (", pc_num, ")"),
        tableOutput("CED_MGR_Contacts"),
        size = "l"
      ))
    } else if (nrow(row_now) > 0) {
      showModal(modalDialog(
        title = paste0("Branch Manager Contacts: ", cust_name, " - ", cust_state, " (", pc_num, ")"),
        tableOutput("Non_CED_MGR_Contacts"),
        size = "l"
      ))
    }
  })
  
  # REACTIVE: GT PC# Table - CED Manager Contacts ----
  output$CED_MGR_Contacts <- renderTable({
    req(credentials()$user_auth)
    PC <- fread("CED_GT_Manager_Map.csv")
    if (!is.null(input$pc_click) && input$pc_click != 0 && nrow(PC[`PC Number` == input$pc_click, ]) != 0) {
      row <- PC[`PC Number` == input$pc_click, ]
      mv <- colnames(PC)
      res <- data.table::melt(
        row[1,], id.vars = NULL, measure.vars = mv,
        variable.name = "Field", value.name = "Value", variable.factor = FALSE
      )[, .(Field, Value)]
      res
    } else {
      "PC# not found in CED Branch List"
    }
  })
  
  output$Non_CED_MGR_Contacts <- renderTable({
    req(credentials()$user_auth)
    "Dist Code != 1 (Non CED Sale)"
  })
  
  # TAB OUTPUT: POS Row by Row Table ----
  output$all_table <- DT::renderDataTable({
    req(credentials()$user_auth)
    
    dt <- data.table::copy(All_POS2)
    data.table::setDT(dt)
    
    dt[, Date := as.Date(Date)]
    dt[, `PC #` := as.character(`PC #`)]
    dt[, Dist_Code := as.character(Dist_Code)]
    
    if (is.character(dt$Price_ea)) {
      dt[, Price_ea := as.numeric(gsub("[^0-9.-]", "", Price_ea))]
    }
    dt[, Qty := as.numeric(Qty)]
    dt[, Revenue := Price_ea * Qty]
    
    if (!is.null(input$rep_all) && input$rep_all != "All")
      dt <- dt[Rep == input$rep_all]
    if (!is.null(input$ship_to_state_all) && input$ship_to_state_all != "All")
      dt <- dt[State == input$ship_to_state_all]
    if (!is.null(input$dist_code_all) && input$dist_code_all != "All")
      dt <- dt[Dist_Code == as.character(input$dist_code_all)]
    if (!is.null(input$year_all) && input$year_all != "All")
      dt <- dt[lubridate::year(Date) == as.integer(input$year_all)]
    if (!is.null(input$PC_all) && input$PC_all != "All")
      dt <- dt[`PC #` == as.character(input$PC_all)]
    if (!is.null(input$category_all) && input$category_all != "All")
      dt <- dt[Category == input$category_all]
    if (!is.null(input$item_all) && length(input$item_all) && !("All" %in% input$item_all))
      dt <- dt[Item %in% input$item_all]
    if (!is.null(input$description_all) && length(input$description_all) && !("All" %in% input$description_all))
      dt <- dt[Description %in% input$description_all]
    
    cols <- c("Date","Customer","Category","Description","State","PC #",
              "Dist_Code","Rep","Cust_ID","Item","Price_ea","Qty","Revenue")
    cols <- intersect(cols, names(dt))
    dt <- dt[, ..cols]
    data.table::setorder(dt, -Date)
    
    tbl <- DT::datatable(
      dt,
      options = list(pageLength = 100, autoWidth = TRUE),
      rownames = FALSE
    )
    tbl <- DT::formatCurrency(tbl, c("Price_ea","Revenue"), currency = "$", digits = 2)
    tbl
  }, escape = FALSE)
  
  # ---------- Shared helper: filtered monthly POS for the plots ----------
  posq_filtered_pos <- reactive({
    req(POS_Agg)
    pos <- data.table::copy(POS_Agg)
    data.table::setDT(pos)
    if (!"Revenue" %in% names(pos)) {
      pos[, Price_ea := as.numeric(gsub("\\$", "", Price_ea))]
      pos[, Revenue := Price_ea * Qty]
    }
    if (!"Monthly" %in% names(pos)) {
      pos[, Monthly := lubridate::floor_date(as.Date(Date), "month")]
    } else {
      pos[, Monthly := as.Date(Monthly)]
    }
    if ("Dist_Code" %in% names(pos)) pos[, Dist_Code := as.character(Dist_Code)]
    if ("Distributor" %in% names(pos)) pos[, Distributor := as.character(Distributor)]
    
    if (!is.null(input$rep) && input$rep != "All") {
      pos <- pos[Rep == input$rep]
    }
    if (!is.null(input$ship_to_state) && length(input$ship_to_state) > 0 && !("All" %in% input$ship_to_state)) {
      pos <- pos[State %in% input$ship_to_state]
    }
    if (!is.null(input$dist_code) && input$dist_code != "All") {
      pos <- pos[Dist_Code == input$dist_code]
    }
    if (!is.null(input$`PC`) && input$`PC` != "All") {
      pos <- pos[`PC #` == input$`PC`]
    }
    if (!is.null(input$category) && input$category != "All") {
      pos <- pos[Category == input$category]
    }
    if (!is.null(input$year) && length(input$year) > 0 && !("All" %in% input$year)) {
      pos <- pos[lubridate::year(Monthly) %in% as.integer(input$year)]
    }
    
    pos <- pos[!is.na(Revenue) & Revenue != 0]
    
    if ("Distributor" %in% names(pos) && any(nzchar(pos$Distributor))) {
      pos[, dist_label := Distributor]
    } else {
      pos[, dist_label := Dist_Code]
    }
    pos[is.na(dist_label) | dist_label == "", dist_label := "Unknown"]
    pos[, dist_label := factor(dist_label, levels = sort(unique(dist_label)))]
    
    if (!"Rep" %in% names(pos)) pos[, Rep := "Unknown"]
    pos[is.na(Rep) | Rep == "", Rep := "Unknown"]
    pos[, Rep := factor(Rep, levels = sort(unique(Rep)))]
    
    pos
  })
  
  output$pc_top_n_title <- renderText({
    yr  <- if(is.null(input$year)) {
      "All Years"
    } else if (length(input$year) > 1) {
      paste(input$year, collapse = ", ")
    } else {
      input$year[1]
    }
    rep <- ifelse(is.null(input$rep)  || input$rep  == "All", "All Reps", input$rep)
    n   <- ifelse(is.null(input$top_n), "N", input$top_n)
    paste0("Monthly Revenue by PC # (Top-", n, ") — ", rep, " — ", yr)
  })
  
  # Category
  output$posq_plot_category <- plotly::renderPlotly({
    req(input$posq_show_category)
    pos <- posq_filtered_pos()
    if (!"Category" %in% names(pos)) pos[, Category := "Unknown"]
    pos[is.na(Category) | Category == "", Category := "Unknown"]
    
    agg <- pos[, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(Monthly, Category)][order(Monthly)]
    g <- ggplot2::ggplot(agg, ggplot2::aes(Monthly, Revenue, fill = Category)) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::labs(x = NULL, y = "Revenue ($)", fill = "Category") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    plotly::ggplotly(g)
  })
  
  # Distributor
  output$posq_plot_distributor <- plotly::renderPlotly({
    req(input$posq_show_distributor)
    pos <- posq_filtered_pos()
    pos[, Dist_Code := as.character(Dist_Code)]
    pos <- merge(pos, dist_map, by = "Dist_Code", all.x = TRUE)
    pos[is.na(Distributor) | Distributor == "",
        Distributor := paste0("Unknown (Code ", Dist_Code, ")")]
    
    agg <- pos[, .(Revenue = sum(Revenue, na.rm = TRUE)),
               by = .(Monthly, Distributor, Dist_Code)][order(Monthly)]
    agg[, hover_text := paste0(
      "Month: ", format(Monthly, "%b %Y"),
      "<br>Distributor: ", Distributor,
      "<br>Dist Code: ", Dist_Code,
      "<br>Revenue: ", scales::dollar(Revenue)
    )]
    g <- ggplot2::ggplot(
      agg,
      ggplot2::aes(x = Monthly, y = Revenue, fill = Distributor, text = hover_text)
    ) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::labs(x = NULL, y = "Revenue ($)", fill = "Distributor") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    plotly::ggplotly(g, tooltip = "text")
  })
  
  # Rep
  output$posq_plot_rep <- plotly::renderPlotly({
    req(input$posq_show_rep)
    pos <- posq_filtered_pos()
    agg <- pos[, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(Monthly, Rep)][order(Monthly)]
    g <- ggplot2::ggplot(agg, ggplot2::aes(Monthly, Revenue, fill = Rep)) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::labs(x = NULL, y = "Revenue ($)", fill = "Rep") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    plotly::ggplotly(g)
  })
  
  # PC#
  output$posq_plot_pc <- plotly::renderPlotly({
    req(input$posq_show_pc)
    pos <- posq_filtered_pos()
    if (!"PC #" %in% names(pos)) pos[, `PC #` := NA_character_]
    pos[, `PC #` := as.character(`PC #`)]
    pos[is.na(`PC #`) | `PC #` == "" , `PC #` := "Unknown"]
    
    top_pc <- pos[, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(`PC #`)][
      order(-Revenue)
    ][seq_len(min(input$posq_pc_topn, .N)), `PC #`]
    
    plot_df <- pos[`PC #` %in% top_pc, .(Revenue = sum(Revenue, na.rm = TRUE)), by = .(Monthly, `PC #`)][order(Monthly)]
    
    g <- ggplot2::ggplot(plot_df, ggplot2::aes(Monthly, Revenue, fill = `PC #`)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::labs(x = NULL, y = "Revenue ($)", fill = "PC #") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    plotly::ggplotly(g)
  })
  
  # TAB SECTION: Distributor Inventory Tracker ----
  filtered_gt_inventory <- reactive({
    req(credentials()$user_auth)
    data <- copy(GT_Inv)
    setDT(data)
    
    if (input$di_rep != "All") data <- data[Rep == input$di_rep]
    if (input$di_state != "All") data <- data[State == input$di_state]
    
    # --- FIXED BUG: this was State == input$di_distributor
    if (input$di_distributor != "All") data <- data[Distributor == input$di_distributor]
    
    if (input$di_pc_name != "All") data <- data[`PC Name` == input$di_pc_name]
    if (input$di_pc != "All") data <- data[`PC #` == input$di_pc]
    
    if (input$di_display == "Item" && !is.null(input$di_item) && length(input$di_item) > 0) {
      data <- data[Item %in% input$di_item]
    }
    
    data <- data[Date >= input$di_date_range[1] & Date <= input$di_date_range[2]]
    if (nrow(data) == 0) return(NULL)
    
    data[, `PC #_raw` := `PC #`]
    data[, `PC Name_raw` := `PC Name`]
    data
  })
  
  observeEvent(input$di_rep, {
    req(inv_dt)
    filtered_states <- if (input$di_rep == "All") unique(inv_dt$State) else unique(inv_dt[Rep == input$di_rep, State])
    output$dynamic_di_state <- renderUI({
      selectInput("di_state", "State:", choices = c("All", filtered_states))
    })
    filtered_pc_names <- if (input$di_rep == "All") unique(inv_dt$`Branch Name`) else unique(inv_dt[Rep == input$di_rep, `Branch Name`])
    output$dynamic_di_pc_name <- renderUI({
      selectInput("di_pc_name", "Branch Name:", choices = c("All", filtered_pc_names))
    })
    filtered_pc <- if (input$di_rep == "All") unique(inv_dt$`PC #`) else unique(inv_dt[Rep == input$di_rep, `PC #`])
    output$dynamic_di_pc <- renderUI({
      selectInput("di_pc", "PC #:", choices = c("All", filtered_pc))
    })
  })
  
  observeEvent(input$di_state, {
    req(inv_dt)
    filtered_pc_names <- unique(inv_dt[
      (input$di_rep == "All" | Rep == input$di_rep) &
        (input$di_state == "All" | State == input$di_state),
      `Branch Name`
    ])
    output$dynamic_di_pc_name <- renderUI({
      selectInput("di_pc_name", "Branch Name:", choices = c("All", filtered_pc_names))
    })
    filtered_pc <- unique(inv_dt[
      (input$di_rep == "All" | Rep == input$di_rep) &
        (input$di_state == "All" | State == input$di_state),
      `PC #`
    ])
    output$dynamic_di_pc <- renderUI({
      selectInput("di_pc", "PC #:", choices = c("All", filtered_pc))
    })
  })
  
  observeEvent(input$di_pc_name, {
    req(inv_dt)
    filtered_pc <- unique(inv_dt[
      (input$di_rep == "All" | Rep == input$di_rep) &
        (input$di_state == "All" | State == input$di_state) &
        (input$di_pc_name == "All" | `Branch Name` == input$di_pc_name),
      `PC #`
    ])
    output$dynamic_di_pc <- renderUI({
      selectInput("di_pc", "PC #:", choices = c("All", filtered_pc))
    })
  })
  
  output$dynamic_di_item <- renderUI({
    req(credentials()$user_auth)
    req(inv_dt, input$di_display)
    
    if (input$di_display == "Item") {
      items <- unique(inv_dt$Item)
      pickerInput(
        inputId = "di_item",
        label = "Select Item(s):",
        choices = items,
        selected = items,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    }
  })
  
  filtered_di_inventory <- reactive({
    req(credentials()$user_auth)
    data <- copy(inv_dt)
    setDT(data)
    
    if (!is.null(input$di_rep) && length(input$di_rep) > 0 && input$di_rep != "All") data <- data[Rep == input$di_rep]
    if (!is.null(input$di_state) && length(input$di_state) > 0 && input$di_state != "All") data <- data[State == input$di_state]
    if (!is.null(input$di_distributor) && length(input$di_distributor) > 0 && input$di_distributor != "All") data <- data[Distributor == input$di_distributor]
    if (!is.null(input$di_pc_name) && length(input$di_pc_name) > 0 && input$di_pc_name != "All") data <- data[`Branch Name` == input$di_pc_name]
    if (!is.null(input$di_pc) && length(input$di_pc) > 0 && input$di_pc != "All") data <- data[`PC #` == input$di_pc]
    
    if (input$di_display == "Item" && !is.null(input$di_item) && length(input$di_item) > 0) {
      data <- data[Item %in% input$di_item]
    }
    
    data <- data[Date >= input$di_date_range[1] & Date <= input$di_date_range[2]]
    if (nrow(data) == 0) return(NULL)
    
    data[, `PC #_raw` := `PC #`]
    data[, `Branch Name_raw` := `Branch Name`]
    data
  })
  
  output$di_inventory_table <- DT::renderDataTable({
    req(credentials()$user_auth)
    data <- filtered_di_inventory()
    req(data)
    data <- data[lubridate::year(Date) == lubridate::year(max(Date)) & lubridate::month(Date) == lubridate::month(max(Date))]
    data[, safe_pc_num := gsub("[^A-Za-z0-9]", "_", `PC #`)]
    
    data[, `Branch Name_raw` := `Branch Name`]
    data[, `PC #_raw` := `PC #`]
    
    data[, `Branch Name` := mapply(function(pc_name, pc_num) {
      sprintf('<span href="#" class="action-button" onclick="Shiny.setInputValue(\'pc_name_click\', \'%s\', {priority: \'event\'});">%s</span>',
              pc_num, pc_name)
    }, data$`Branch Name_raw`, data$safe_pc_num)]
    
    data[, `PC #` := mapply(function(pc_num, safe_num) {
      sprintf('<span href="#" class="action-button" onclick="Shiny.setInputValue(\'pc_num_click\', \'%s\', {priority: \'event\'});">%s</span>',
              pc_num, pc_num)
    }, data$`PC #_raw`, data$safe_pc_num)]
    
    data[[input$di_metric]] <- as.numeric(data[[input$di_metric]])
    
    summary <- data %>%
      group_by(`Branch Name`, `PC #`, Rep, .data[[input$di_display]], State) %>%
      summarise(Total = sum(.data[[input$di_metric]], na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = .data[[input$di_display]], values_from = Total, values_fill = 0) %>%
      as.data.table()
    
    cols_to_format <- setdiff(names(summary), c("Branch Name", "PC #", "Rep", "State"))
    summary[, (cols_to_format) := lapply(.SD, as.numeric), .SDcols = cols_to_format]
    if ("MINI" %in% colnames(summary)) setorder(summary, -"MINI")
    
    dt <- DT::datatable(summary, escape = FALSE, options = list(pageLength = 100))
    if (input$di_metric == "On Hand Value") {
      dt <- formatRound3(dt, cols_to_format, "$", digits = 2)
    } else {
      dt <- formatRound3(dt, cols_to_format, "", digits = 0)
    }
    dt
  })
  
  output$di_inventory_plot <- renderPlotly({
    data <- filtered_di_inventory()
    req(data)
    
    data[, Month := lubridate::floor_date(Date, "month")]
    data[[input$di_metric]] <- as.numeric(data[[input$di_metric]])
    
    plot_df <- data %>%
      group_by(Month, .group = .data[[input$di_display]]) %>%
      summarise(Total = sum(.data[[input$di_metric]], na.rm = TRUE), .groups = "drop")
    setnames(plot_df, ".group", input$di_display)
    
    ggplotly(
      ggplot(plot_df, aes(x = Month, y = Total, fill = .data[[input$di_display]])) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = scales::hue_pal()(length(unique(plot_df[[input$di_display]])))) +
        labs(
          title = paste("Monthly Inventory Volume by", str_to_title(input$di_display)),
          y = ifelse(input$di_metric == "On Hand Value", "Total Inventory Value ($)", "Total Units"),
          x = "Month", fill = str_to_title(input$di_display)
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # ✅ Show Historical Data when clicking on PC Name
  observeEvent(input$pc_name_click, {
    data <- filtered_di_inventory()
    req(data)
    
    historical_data <- data[`PC #_raw` == input$pc_name_click]
    
    historical_summary <- setDT(historical_data %>%
                                  group_by(Date, .data[[input$di_display]]) %>%
                                  summarise(Total = sum(.data[[input$di_metric]], na.rm = TRUE), .groups = "drop") %>%
                                  pivot_wider(names_from = .data[[input$di_display]], values_from = Total, values_fill = 0))
    
    setorder(historical_summary, -"Date")
    historical_summary[, Date := as.character(as.Date(Date))]
    
    historical_summary[, (setdiff(names(historical_summary), "Date")) := lapply(.SD, function(x) {
      if (input$gt_metric == "On Hand Value") {
        scales::dollar(x)
      } else {
        format(x, big.mark = ",")
      }
    }), .SDcols = setdiff(names(historical_summary), "Date")]
    
    showModal(modalDialog(
      title = paste("Historical Data for", PCdata[`PC Number` == input$pc_name_click]$`PC Name`, "(", input$pc_name_click, ")"),
      tableOutput("historical_inventory_table"),
      size = "l"
    ))
    
    output$historical_inventory_table <- renderTable({
      historical_summary
    })
  })
  
  # ✅ Show Branch Info when clicking on PC #
  observeEvent(input$pc_num_click, {
    data <- filtered_gt_inventory()
    req(data)
    branch_info <- PCdata[`PC Number` == input$pc_num_click]
    
    showModal(modalDialog(
      title = paste("Branch Info for PC #", input$pc_num_click),
      tableOutput("branch_info_table"),
      size = "l"
    ))
    
    output$branch_info_table <- renderTable({
      branch_info %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = everything(), names_to = "Info Name", values_to = "Info")
    })
  })
  
  # TAB SECTION: Invoice Revenue Insights ----
  
  output$invoice_data_viz <-renderPlotly({
    req(credentials()$user_auth)
    
    filtered_data <- reactive({
      data <- Invoice_data[State != "Aichi", ]
      if (input$rep4 != "All")  data <- data[data$Rep == input$rep4, ]
      if (input$state4 != "All") data <- data[data$State == input$state4, ]
      if (input$category4 != "All") data <- data[data$Category == input$category4, ]
      if (input$customer_name_inv != "All") data <- data[data$Customer_Name %in% input$customer_name_inv, ]
      data
    })
    data <- filtered_data()
    aggregated_data <- data[, .(total_revenue = sum(Revenue, na.rm = TRUE)), by = .(Monthly_Date, Year)]
    aggregated_data[, month := lubridate::month(Monthly_Date)]
    
    custom_colors <- c("2020" = "#1b9e77", "2021" = "#d95f02", "2022" = "#7570b3", "2023" = "#e7298a","2024" = "blue")
    
    p <- plot_ly(aggregated_data,
                 x = ~as.factor(month),
                 y = ~total_revenue,
                 color = ~as.factor(Year),
                 colors = custom_colors,
                 type = 'scatter',
                 mode = 'lines') %>%
      layout(title = "Annual Revenue Trend Comp",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Revenue", tickformat = "$,.2f"),
             autosize = TRUE)
  })
  
  output$invoice_data_viz2 <- DT::renderDataTable({
    req(credentials()$user_auth)
    
    filtered_data <- reactive({
      data <- Invoice_data[State != "Aichi", ]
      if (input$rep4 != "All")  data <- data[data$Rep == input$rep4, ]
      if (input$state4 != "All") data <- data[data$State == input$state4, ]
      if (input$category4 != "All") data <- data[data$Category == input$category4, ]
      if (input$customer_name_inv != "All") data <- data[data$Customer_Name %in% input$customer_name_inv, ]
      data
    })
    
    data <- filtered_data()
    data <- data[Date %in% head(unique(data$Date)[order(unique(data$Date), decreasing = TRUE)], 10) & Revenue != 0, ]
    
    print_table <- setnames(
      data[, sum(Revenue), by = c("Invoice_Number", "Item", "Category", "Rep", "Customer_Name", "Date")][order(-Date, -Invoice_Number)],
      "V1", "Revenue"
    )
    
    print_table[, Revenue := scales::dollar(Revenue)]
    print_table[, Date := as.character(Date)]
    
    DT::datatable(print_table, options = list(pageLength = 60, autoWidth = TRUE))
  })
  
  # TAB OUTPUT: Sales Forecast Visualization ----
  output$sales_forecast_viz <- renderPlotly({
    req(credentials()$user_auth)
    
    filtered_data <- reactive({
      data <- Invoice_data[State != "Aichi", ]
      if (input$rep4 != "All")  data <- data[data$Rep == input$rep4, ]
      if (input$state4 != "All") data <- data[data$State == input$state4, ]
      if (input$category4 != "All") data <- data[data$Category == input$category4, ]
      if (input$customer_name_inv != "All") data <- data[data$Customer_Name %in% input$customer_name_inv, ]
      data
    })
    
    data <- filtered_data()
    data[, Date := as.Date(Date)]
    
    if (input$time_aggregation == "Weekly") {
      data[, Time := floor_date(Date, "week")]
      data[, Period := as.numeric(format(Time, "%U"))]
    } else {
      data[, Time := floor_date(Date, "month")]
      data[, Period := lubridate::month(Time)]
    }
    data[, Year := lubridate::year(Time)]
    
    aggregated_data <- data[, .(total_revenue = sum(Revenue, na.rm = TRUE)), by = .(Time, Year, Period)]
    
    if (input$forecast_method != "None") {
      actual_frequency <- if (input$time_aggregation == "Weekly") length(unique(aggregated_data$Period)) else 12
      ts_data <- ts(aggregated_data$total_revenue, frequency = actual_frequency, start = c(min(aggregated_data$Year), min(aggregated_data$Period)))
      forecasted_values <- switch(input$forecast_method,
                                  "ARIMA" = forecast(auto.arima(ts_data), h = ifelse(input$time_aggregation == "Weekly", 8, 6)),
                                  "TBATS" = forecast(tbats(ts_data), h = ifelse(input$time_aggregation == "Weekly", 8, 6)),
                                  "ETS"   = forecast(ets(ts_data),  h = ifelse(input$time_aggregation == "Weekly", 8, 6)))
      
      forecast_time <- seq.Date(from = max(aggregated_data$Time) + (ifelse(input$time_aggregation == "Weekly", 7, 1)),
                                by = ifelse(input$time_aggregation == "Weekly", "week", "month"),
                                length.out = ifelse(input$time_aggregation == "Weekly", 8, 6))
      
      forecast_time_period <- if (input$time_aggregation == "Weekly") {
        as.numeric(format(forecast_time, "%U"))
      } else {
        lubridate::month(forecast_time)
      }
      
      forecast_df <- data.table(
        Time = forecast_time,
        total_revenue = as.numeric(forecasted_values$mean),
        Year = lubridate::year(forecast_time),
        Period = forecast_time_period,
        Type = "Forecast"
      )
      plot_data <- rbind(aggregated_data[, .(Time, Year, Period, total_revenue, Type = "Actual")], forecast_df)
    } else {
      plot_data <- aggregated_data[, .(Time, Year, Period, total_revenue, Type = "Actual")]
    }
    
    plot_title <- paste(input$time_aggregation, "Sales Forecast using", if (input$forecast_method == "None") "No Forecast" else input$forecast_method,
                        if (input$rep4 != "All") paste(" - Rep:", input$rep4) else "",
                        if (input$state4 != "All") paste(" - State:", input$state4) else "",
                        if (input$category4 != "All") paste(" - Category:", input$category4) else "")
    
    p <- plot_ly() %>%
      add_lines(data = plot_data[Type == "Actual"],
                x = ~Period, y = ~total_revenue,
                color = ~as.factor(Year),
                colors = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
                text = ~paste("Type:", Type, "<br>Week Start:", format(Time, "%B %d, %Y"), "<br>Revenue:", scales::dollar(total_revenue)),
                hoverinfo = "text",
                line = list(width = 2)) %>%
      add_lines(data = plot_data[Type == "Forecast"],
                x = ~Period, y = ~total_revenue,
                color = ~as.factor(Year),
                colors = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
                text = ~paste("Type:", Type, "<br>Week Start:", format(Time, "%B %d, %Y"), "<br>Revenue:", scales::dollar(total_revenue)),
                hoverinfo = "text",
                line = list(width = 2, dash = 'dot', opacity = 0.4)) %>%
      layout(title = plot_title,
             xaxis = list(title = ifelse(input$time_aggregation == "Weekly", "Week Number", "Month")),
             yaxis = list(title = "Revenue", tickformat = "$,.2f"),
             autosize = TRUE,
             legend = list(title = list(text = "Year")))
    p
  })
  
  output$sales_forecast_table <- renderTable({
    req(credentials()$user_auth)
    
    filtered_data <- reactive({
      data <- Invoice_data[State != "Aichi", ]
      if (input$rep4 != "All")  data <- data[data$Rep == input$rep4, ]
      if (input$state4 != "All") data <- data[data$State == input$state4, ]
      if (input$category4 != "All") data <- data[data$Category == input$category4, ]
      data
    })
    
    data <- filtered_data()
    data[, Date := as.Date(Date)]
    
    h <- if (input$time_aggregation == "Weekly") 8 else 6
    if (input$time_aggregation == "Weekly") {
      data[, Time := floor_date(Date, "week")]
      data[, Period := as.numeric(format(Time, "%U"))]
    } else {
      data[, Time := floor_date(Date, "month")]
      data[, Period := lubridate::month(Time)]
    }
    data[, Year := lubridate::year(Time)]
    
    aggregated_data <- data[, .(total_revenue = sum(Revenue, na.rm = TRUE)), by = .(Time, Year, Period)]
    ts_data <- ts(aggregated_data$total_revenue, frequency = ifelse(input$time_aggregation == "Weekly", 52, 12))
    
    forecasted_values <- switch(input$forecast_method,
                                "ARIMA" = forecast(auto.arima(ts_data), h = h),
                                "TBATS" = forecast(tbats(ts_data), h = h),
                                "ETS"   = forecast(ets(ts_data), h = h))
    
    forecast_time <- seq(max(aggregated_data$Time) + (ifelse(input$time_aggregation == "Weekly", weeks(1), months(1))),
                         length.out = h, by = ifelse(input$time_aggregation == "Weekly", "week", "month"))
    
    forecast_df <- data.table(Time = forecast_time,
                              total_revenue = as.numeric(forecasted_values$mean),
                              Year = lubridate::year(forecast_time),
                              Period = ifelse(input$time_aggregation == "Weekly", lubridate::week(forecast_time), lubridate::month(forecast_time)),
                              Label = "Forecast")
    
    first_forecast_date <- forecast_df$Time[1]
    first_forecast_year <- forecast_df$Year[1]
    first_forecast_period <- forecast_df$Period[1]
    max_previous_year_period <- max(aggregated_data[Year == (first_forecast_year - 1), Period])
    
    previous_year <- aggregated_data[Period == first_forecast_period & Year == (first_forecast_year - 1), .(Time, Year, total_revenue, Label = "Previous Year")]
    
    if(input$time_aggregation == "Weekly"){
      previous_month <- aggregated_data[
        Period == ifelse(first_forecast_period < 5, max_previous_year_period - (4 - first_forecast_period), first_forecast_period - 4) &
          Year == ifelse(lubridate::month(first_forecast_date) == 1, first_forecast_year - 1, first_forecast_year),
        .(Time, Year, total_revenue, Label = "Previous Month")
      ]
    } else {
      previous_month <- aggregated_data[
        Period == ifelse(first_forecast_period < 2, max_previous_year_period, first_forecast_period-1) &
          Year == ifelse(lubridate::month(first_forecast_date) == 1, first_forecast_year - 1, first_forecast_year),
        .(Time, Year, total_revenue, Label = "Previous Month")
      ]
    }
    
    prior_week <- aggregated_data[
      Period == ifelse(lubridate::week(first_forecast_date) == 1, max_previous_year_period, lubridate::week(first_forecast_date) - 1) &
        Year == ifelse(lubridate::week(first_forecast_date) == 1, first_forecast_year - 1, first_forecast_year),
      .(Time, Year, total_revenue, Label = "Prior Week")
    ]
    
    combined_table <- rbindlist(list(
      previous_year,
      previous_month,
      prior_week,
      forecast_df[, .(Time, Year, total_revenue, Label)]
    ), fill = TRUE)
    combined_table[,Time := as.character(as.Date(Time))][,Year := NULL]
    setnames(combined_table, "total_revenue", "Total Revenue")
    combined_table[, `Total Revenue` := scales::dollar(round(`Total Revenue`, 2))]
  })
  
  output$dynamic_slider <- renderUI({
    req(input$filter_metric)
    
    if (input$filter_metric == "Kw") {
      sliderInput("install_kw", "Qtrly Installation Volume Avg. (KW):",
                  min = 0,
                  max = round(max(ohm_data$Qtrly_Avg_Kw, na.rm = TRUE) + 10,0),
                  value = c(0, round(max(ohm_data$Qtrly_Avg_Kw, na.rm = TRUE), 0)))
    } else {
      sliderInput("install_pc", "Qtrly Installation Volume Avg. (PC):",
                  min = 0,
                  max = round(max(ohm_data$Qtrly_Avg_PC, na.rm = TRUE) + 10, 0),
                  value = c(0, round(max(ohm_data$Qtrly_Avg_PC, na.rm = TRUE), 0)))
    }
  })
  
  # REACTIVE SECTION: POS and Ohm Analytics Screener ----
  filtered_ohm_installer <- reactive({
    filtered_data <- ohm_data
    if (!is.null(input$state_installer) && input$state_installer != "All") {
      filtered_data <- filtered_data %>% filter(State == input$state_installer)
    }
    if(!is.null(input$rt_customer) && input$rt_customer==T){
      filtered_data <- filtered_data %>% filter(RT_POS_Customer == input$rt_customer)
    }
    if (!is.null(input$filter_metric) && input$filter_metric == "Kw") {
      if (!is.null(input$install_kw) && length(input$install_kw) == 2) {
        filtered_data <- filtered_data %>% filter(Qtrly_Avg_Kw >= input$install_kw[1], Qtrly_Avg_Kw <= input$install_kw[2])
      }
    } else {
      if (!is.null(input$install_pc) && length(input$install_pc) == 2) {
        filtered_data <- filtered_data %>% filter(Qtrly_Avg_PC >= input$install_pc[1], Qtrly_Avg_PC <= input$install_pc[2])
      }
    }
    filtered_data
  })
  
  output$installer_list_ui <- renderUI({
    filtered_data <- filtered_ohm_installer()
    if (!is.null(input$filter_metric) && input$filter_metric == "Kw") {
      if (!is.null(input$install_kw) && length(input$install_kw) == 2) {
        filtered_data <- filtered_data %>% filter(Qtrly_Avg_Kw >= input$install_kw[1], Qtrly_Avg_Kw <= input$install_kw[2])
      }
    } else {
      if (!is.null(input$install_pc) && length(input$install_pc) == 2) {
        filtered_data <- filtered_data %>% filter(Qtrly_Avg_PC >= input$install_pc[1], Qtrly_Avg_PC <= input$install_pc[2])
      }
    }
    
    selectInput("selected_installer", "Installer to Screen:",
                choices = c("None", unique(filtered_data$Contractor_Name)),
                selected = "None")
  })
  
  output$ohm_installer_quarterly_avg_table <- DT::renderDataTable({
    filtered_data <- filtered_ohm_installer()
    filtered_data <- filtered_data[,-"Contractor_Id"]
    
    if(!is.null(input$state_installer) && input$state_installer=="All"){
      states_table <- filtered_data[, .(
        States = paste(unique(State), collapse = ", ")
      ), by = .(Contractor_Name)]
      filtered_data <- filtered_data[, .(
        Kw = round(sum(Kw, na.rm = TRUE), 2),
        PC = round(sum(PC, na.rm = TRUE), 2)
      ), by = .(Contractor_Name, Year_Qtr)]
      quarterly_avg_data <- left_join(filtered_data[, .(
        Qtrly_Average_Kw = round(mean(Kw, na.rm = TRUE), 2),
        Qtrly_Average_PC = round(mean(PC, na.rm = TRUE), 2)
      ), by = .(Contractor_Name)],states_table)
    }else{
      quarterly_avg_data <- filtered_data[, .(
        Qtrly_Average_Kw = round(mean(Kw, na.rm = TRUE), 2),
        Qtrly_Average_PC = round(mean(PC, na.rm = TRUE), 2),
        States = paste(unique(State), collapse = ", ")
      ), by = .(Contractor_Name)]
    }
    quarterly_avg_data[,Average_Kw_per_Project := round(Qtrly_Average_Kw/Qtrly_Average_PC, 2)]
    setorder(quarterly_avg_data, -"Qtrly_Average_Kw")
    DT::datatable(quarterly_avg_data, options = list(pageLength = 15), rownames = FALSE)
  })
  
  output$install_trend_plot <- renderPlotly({
    if (is.null(input$selected_installer) || input$selected_installer == "None") {
      plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            text = "Please select an Installer to Screen from the dropdown to view analytics",
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 16)
          )
        )
    } else {
      req(input$selected_installer)
      req(input$pos_ohm_input_category)
      
      ohm_filtered <- ohm_data %>%
        filter(Contractor_Name == input$selected_installer)
      
      pos_filtered <- pos_data %>%
        filter(Ohm_ID %in% ohm_filtered$Contractor_Id) %>%
        filter(Category == input$pos_ohm_input_category)
      
      if(!is.null(input$filter_metric) && input$filter_metric=="Kw"){
        ohm_filtered <- ohm_filtered[,c("State", "Contractor_Name", "Kw","Year_Qtr")]
      } else {
        ohm_filtered <- ohm_filtered[,c("State", "Contractor_Name", "PC","Year_Qtr")]
      }
      
      pos_filtered <- pos_filtered[, c("Year_Qtr","Cust_Key","State","Revenue")]
      
      if(nrow(pos_filtered)>0){
        merged_data <- merge(ohm_filtered[,-"Contractor_Name"], pos_filtered[,-"Cust_Key"], by = c("Year_Qtr", "State"), all = T, allow.cartesian = T)
        if(!is.null(input$filter_metric) && input$filter_metric=="Kw"){
          setnames(merged_data,"Kw", "installation_kpi")
        } else {
          setnames(merged_data,"PC", "installation_kpi")
        }
        scaleFactor <- max(merged_data$Revenue, na.rm = TRUE)/max(merged_data$installation_kpi, na.rm = TRUE)
        
        p <- ggplot(merged_data) +
          geom_bar(aes(x = Year_Qtr, y = Revenue, fill = State,
                       text = paste0("Year_Qtr: ", Year_Qtr,
                                     "<br>State: ", State,
                                     "<br>Revenue: $", round(Revenue,2))),
                   stat = "identity", position = "stack") +
          geom_line(aes(x = Year_Qtr, y = installation_kpi * scaleFactor, color = State, group = State,
                        text = paste0("Year_Qtr: ", Year_Qtr,
                                      "<br>State: ", State,
                                      "<br>Installation KPI (Unscaled): ", round(installation_kpi,1), " ",input$filter_metric)),
                    size = 1.2) +
          scale_y_continuous(
            name = paste0(input$pos_ohm_input_category ," Revenue (Line Graph)"),
            sec.axis = sec_axis(~./scaleFactor, name = paste0("Installed ", input$filter_metric," (Bar Graph)"))
          ) +
          labs(title = paste0("Installation Trends and ", input$pos_ohm_input_category," Revenue for ", input$selected_installer),
               x = "Year_Quarter",
               y = paste0(input$pos_ohm_input_category ," Revenue (Line Graph)"),
               fill = "State (Stacked Bar)",
               color = "State (Line)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        plotly_obj <- ggplotly(p, tooltip = "text")
        ay <- list(
          tickfont = list(size = 11.7),
          titlefont = list(size = 14.6),
          overlaying = "y",
          nticks = 5,
          side = "right",
          title = "Installed (Kw)"
        )
        plotly_obj %>%
          add_lines(x = ~merged_data$Year_Qtr,
                    y = ~merged_data$installation_kpi,
                    name = "Installed (Kw)",
                    yaxis = "y2",
                    showlegend = TRUE) %>%
          layout(yaxis2 = ay)
      } else {
        setnames(ohm_filtered, if(!is.null(input$filter_metric) && input$filter_metric=="Kw"){input$filter_metric}else{"PC"} , "installation_kpi")
        ggplotly(ggplot(ohm_filtered) +
                   geom_bar(aes(x = Year_Qtr, y = installation_kpi, fill = State),
                            stat = "identity", position = "stack") +
                   scale_y_continuous(
                     name =  paste0("Installed ", input$filter_metric," (Bar Graph)")
                   ) +
                   labs(title = paste0("Installation Trends and ", input$pos_ohm_input_category," Revenue for ", input$selected_installer),
                        x = "Year_Quarter", y = paste0("Installed ", input$filter_metric," (Bar Graph)"),
                        fill = "State (Stacked Bar)", color = "State (Line)") +
                   theme(axis.text.x = element_text(angle = 45, hjust = 1)))
      }
    }
  })
  
  # Display installer contacts
  output$installer_contacts_table <- renderTable({
    req(input$selected_installer)
    contacts_data %>%
      filter(Contractor_Name == input$selected_installer) %>%
      select(Name, Position, Email, Phone)
  })
  
  ################################### File Download for Yukiko ###############################################
  
  # Password-protection flag
  download_authorized <- reactiveVal(FALSE)
  
  observeEvent(input$main_navbar, {
    if (input$main_navbar != "Download POS File") {
      download_authorized(FALSE)
    }
  })
  
  observeEvent(input$validate_download, {
    if (input$download_password == "1968") {
      download_authorized(TRUE)
    } else {
      download_authorized(FALSE)
      showNotification("Incorrect password", type = "error")
    }
  })
  
  output$download_ui <- renderUI({
    req(download_authorized())
    downloadButton("download_pos", "Download POS_Agg CSV")
  })
  
  admin_tab <- reactive({
    req(credentials()$user_auth)
    if (tolower(credentials()$info$user) != "admin") return(list())
    list(
      tabPanel(
        "Download POS File",
        sidebarLayout(
          sidebarPanel(
            passwordInput("download_password", "Enter Password:"),
            actionButton("validate_download", "Unlock Download")
          ),
          mainPanel(
            uiOutput("download_ui")
          )
        )
      )
    )
  })
  
  observeEvent(input$main_navbar, {
    if (input$main_navbar != "Download POS File") {
      download_authorized(FALSE)
      updateTextInput(session, "download_password", value = "")
    }
  })
  
  output$download_pos <- downloadHandler(
    filename = function() {
      paste0("POS_Agg_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      fwrite(POS_Agg, file)
    }
  )

  # TAB OUTPUT: GT PC# Table (CED Branch Contacts) ----
  output$PCtable <- DT::renderDataTable({
    req(credentials()$user_auth)

    dt <- data.table::copy(PCdata)
    data.table::setDT(dt)

    # Apply filters
    if (!is.null(input$rep2) && input$rep2 != "All") {
      dt <- dt[Rep == input$rep2]
    }
    if (!is.null(input$state) && input$state != "All") {
      dt <- dt[State == input$state]
    }
    if (!is.null(input$division) && input$division != "All") {
      dt <- dt[Division == input$division]
    }
    if (!is.null(input$type) && input$type != "All") {
      dt <- dt[Type == input$type]
    }

    DT::datatable(
      dt,
      options = list(
        pageLength = 25,
        autoWidth = FALSE,
        scrollX = FALSE,
        dom = 'lfrtip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  }, escape = FALSE)

  # TAB OUTPUT: Cooper Branch Table ----
  output$CPtable <- DT::renderDataTable({
    req(credentials()$user_auth)

    dt <- data.table::copy(Cooperdata)
    data.table::setDT(dt)

    # Apply filters
    if (!is.null(input$rep3) && input$rep3 != "All") {
      dt <- dt[Rep == input$rep3]
    }
    if (!is.null(input$state3) && input$state3 != "All") {
      dt <- dt[State == input$state3]
    }

    DT::datatable(
      dt,
      options = list(
        pageLength = 25,
        autoWidth = FALSE,
        scrollX = FALSE,
        dom = 'lfrtip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  }, escape = FALSE)

  # TAB OUTPUT: Wesco Branch Table ----
  output$Wescotable <- DT::renderDataTable({
    req(credentials()$user_auth)

    dt <- data.table::copy(wesco_branches)
    data.table::setDT(dt)

    # Apply filters
    if (!is.null(input$rep5) && input$rep5 != "All") {
      dt <- dt[Rep == input$rep5]
    }
    if (!is.null(input$state5) && input$state5 != "All") {
      dt <- dt[`State/Province` == input$state5]
    }
    if (!is.null(input$country5) && input$country5 != "All") {
      dt <- dt[Country == input$country5]
    }
    if (!is.null(input$branch5) && input$branch5 != "All") {
      dt <- dt[`Branch ID` == input$branch5]
    }

    DT::datatable(
      dt,
      options = list(
        pageLength = 25,
        autoWidth = FALSE,
        scrollX = FALSE,
        dom = 'lfrtip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  }, escape = FALSE)

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
}
