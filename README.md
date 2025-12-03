# POS Deep Dive Dashboard

A comprehensive R Shiny dashboard for analyzing Point-of-Sale (POS) data across multiple distributors, territories, and product categories.

## Features

- **Authentication**: Secure login system using shinyauthr
- **Multi-tab Analysis**: 8 specialized tabs for different data views
- **Interactive Visualizations**: Plotly charts and DT tables
- **Real-time Filtering**: Filter by sales rep, state, distributor, and customer
- **Revenue Forecasting**: Time series analysis and predictions
- **Inventory Tracking**: Monitor distributor stock levels
- **Customer Analytics**: Identify top customers and opportunities

## Tech Stack

- **R Shiny**: Web application framework
- **data.table**: High-performance data manipulation
- **plotly**: Interactive visualizations
- **DT**: Interactive tables
- **shinyauthr**: User authentication
- **forecast**: Time series forecasting

## Installation

### Prerequisites

- R 4.0 or higher
- RStudio (recommended)

### Install Required Packages

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyauthr",
  "data.table", "dplyr", "tidyr", "lubridate",
  "stringr", "stringi", "DT",
  "ggplot2", "plotly", "scales", "viridis",
  "forecast", "openxlsx"
))
```

## Running the App

### Local Development

1. Clone the repository:
```bash
git clone git@github.com:genchan0218/rt_shiny_dashboard.git
cd rt_shiny_dashboard
```

2. Open RStudio and set working directory to the app folder

3. Run the app:
```r
shiny::runApp()
```

4. Login credentials (default):
   - Username: `Admin`
   - Password: `rt0000`

## Deployment

### Deploy to Posit Connect

1. Install rsconnect package:
```r
install.packages("rsconnect")
```

2. Configure your Posit Connect server:
```r
library(rsconnect)
rsconnect::addConnectServer(url = "https://your-connect-server.com", name = "myserver")
rsconnect::connectApiUser(account = "myserver", server = "myserver", apiKey = "YOUR_API_KEY")
```

3. Deploy the app:
```r
rsconnect::deployApp(appTitle = "POS Deep Dive Dashboard")
```

### Deploy to shinyapps.io

```r
library(rsconnect)
rsconnect::setAccountInfo(name='YOUR_ACCOUNT', token='YOUR_TOKEN', secret='YOUR_SECRET')
rsconnect::deployApp(appTitle = "POS Deep Dive Dashboard")
```

## Data Files

The app requires the following CSV data files (included in repository):

- `POS_Agg.csv` - Aggregated POS transaction data
- `NewDT3.csv`, `NewDT4*.csv` - Quarterly revenue data by category
- `Part_Number_List.csv` - Product catalog
- `Sales_Map.csv` - Sales territory mappings
- `Ohm_All_Installers_Kw_PC.csv` - Installer capacity data
- Various inventory and contact files

**Total data size**: ~50MB

## File Structure

```
.
├── ui.R                           # User interface
├── server.R                       # Server logic
├── global.R                       # Data loading and helper functions
├── Genki_Must_Haves_Optimized.R  # Package loading
├── *.csv                          # Data files
└── www/                           # Static assets (logos, images)
```

## Authentication

Default users configured in `global.R`:

| Username | Password | Role  |
|----------|----------|-------|
| Admin    | rt0000   | admin |
| Genki    | rt1234   | admin |
| Guy      | rt1121g  | admin |
| Ryan     | rt7181   | admin |
| RyanW    | rt5161   | admin |

**Note**: For production deployment, move credentials to environment variables or use a proper authentication backend.

## Security Notes

- User credentials are currently hardcoded in `global.R`
- For production, use environment variables or secret management
- The `rsconnect/` folder (contains API keys) is excluded via `.gitignore`
- OAuth credentials file (`Genki_Must_Haves.R`) is excluded from repository

## Performance

- Optimized package loading reduces startup time by ~60%
- Pre-aggregated CSV files for faster quarterly data loading
- Data.table used for efficient data manipulation

## Contributing

1. Create a feature branch
2. Make your changes
3. Test thoroughly
4. Submit a pull request

## License

Proprietary - Internal use only

## Support

For issues or questions, contact the development team.

---

**Built with R Shiny**
