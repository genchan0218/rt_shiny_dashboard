# ============================================================================
# UI.R - User Interface
# ============================================================================
# Note: Libraries and data are loaded in global.R

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  #condition = "is.null(output.login_status)",
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
  uiOutput("loggedinUI0"),
  
  uiOutput("loggedinUI")
  
  
  
  # Generate a row with a sidebar
  
)



