# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly) # Keeping if future complex plots are needed
library(ggplot2) # Keeping for potential future use or local plotting debug
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(highcharter)
library(waiter)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)
library(stringr)
library(shinyjs)

# ======================
# Translation Setup
# ======================
# Create translation dictionary for macro indicators data context
translations <- list(
  en = list(
    dashboard_title = "Pakistan Macro Indicators Dashboard",
    overview_details = "Overview & Details",
    trends = "Trend Analysis",
    explorer = "Data Explorer",
    about = "About",
    select_indicator = "Select Macro Indicator:",
    select_element = "Select Element Type:",
    year_range = "Year Range:",
    total_indicators = "Total Indicators Tracked",
    time_period = "Time Period Covered",
    data_points = "Total Data Points",
    average_value = "Average Value",
    key_indicators_overview = "Key Macro Indicators Overview",
    quick_insights = "Quick Insights",
    recent_data = "Recent Data by Indicator & Element",
    indicator_trends = "Macro Indicator Trends Over Time",
    trend_options = "Trend Analysis Options",
    statistical_summary = "Statistical Summary",
    raw_data_explorer = "Raw Data Explorer",
    download_data = "Download Data",
    about_title = "About Pakistan Macro Indicators Dashboard",
    about_project_details = "This dashboard provides a comprehensive view of Pakistan's key macroeconomic indicators, enabling analysis of vital trends over time. It's designed to facilitate data-driven policy decisions and economic research.",
    about_author_details = "This dashboard is a data visualization project by Owais Ali Shah, an economics graduate and a data analyst with a passion for using data to understand and solve real-world problems. This dashboard is a reflection of that commitment."
  ),
  ur = list(
    dashboard_title = "پاکستان کے معاشی اشاروں کا ڈیش بورڈ",
    overview_details = "جائزہ اور تفصیلات",
    trends = "رجحان کا تجزیہ",
    explorer = "ڈیٹا ایکسپلورر",
    about = "کے بارے میں",
    select_indicator = "بڑے اشارے منتخب کریں:",
    select_element = "عناصر کی قسم منتخب کریں:",
    year_range = "سال کی رینج:",
    total_indicators = "ٹریک کردہ کل اشارے",
    time_period = "احاطہ شدہ وقت",
    data_points = "کل ڈیٹا پوائنٹس",
    average_value = "اوسط قدر",
    key_indicators_overview = "اہم معاشی اشاروں کا جائزہ",
    quick_insights = "فوری بصیرت",
    recent_data = "اشارے اور عنصر کے لحاظ سے حالیہ ڈیٹا",
    indicator_trends = "وقت کے ساتھ معاشی اشاروں کے رجحانات",
    trend_options = "رجحان کے تجزیہ کے اختیارات",
    statistical_summary = "اعداد و شمار کا خلاصہ",
    raw_data_explorer = "خام ڈیٹا ایکسپلورر",
    download_data = "ڈیٹا ڈاؤن لوڈ کریں",
    about_title = "پاکستان کے معاشی اشاروں کے ڈیش بورڈ کے بارے میں",
    about_project_details = "یہ ڈیش بورڈ پاکستان کے اہم میکرو اکنامک اشاروں کا ایک جامع نظریہ فراہم کرتا ہے، جو وقت کے ساتھ اہم رجحانات کا تجزیہ کرنے کے قابل بناتا ہے۔ یہ ڈیٹا پر مبنی پالیسی فیصلوں اور اقتصادی تحقیق کو آسان بنانے کے لیے ڈیزائن کیا گیا ہے۔",
    about_author_details = "یہ ڈیش بورڈ اویس علی شاہ کا ایک ڈیٹا ویژولائزیشن پراجیکٹ ہے، جو ایک اقتصادیات کا گریجویٹ اور ڈیٹا تجزیہ کار ہے اور حقیقی دنیا کے مسائل کو سمجھنے اور حل کرنے کے لیے ڈیٹا کا استعمال کرنے کا جذبہ رکھتا ہے۔ یہ ڈیش بورڈ اسی عزم کا عکاس ہے۔"
  )
)

# ======================
# Load & Clean Data
# ======================
# Set up waiting screen
waiting_screen <- tagList(
  div(
    style = "text-align: center; color: #fff;",
    div(
      style = "margin-bottom: 20px;",
      img(src = "https://upload.wikimedia.org/wikipedia/commons/3/32/Flag_of_Pakistan.svg",
          height = "60px") # Flag of Pakistan icon
    ),
    h4("Loading Pakistan Macro Indicators Database", style = "margin-bottom: 20px;"),
    spin_fading_circles(), # Changed spinner to a valid function
    p("Preparing data for analysis...", style = "margin-top: 20px;")
  )
)

# Load the Macro Indicators data from the CSV file
# Using read_csv from readr for better default parsing
data <- read_csv("pakistan_Macro Indicators.csv")

# Filter for 'Pakistan' as the Area, if it contains other areas
data <- data %>% filter(Area == "Pakistan")

# Select relevant columns and clean names for easier use
data <- data %>%
  select(Year, Item, Value, Unit, Element) %>%
  rename(Indicator = Item) # Renaming Item to Indicator for consistency

# Get unique indicators, elements, and years
available_indicators <- unique(data$Indicator)
available_elements <- unique(data$Element)
available_years <- unique(data$Year)

# ======================
# Custom Theme & Styling - Vibrant Purples & Golds
# ======================
custom_css <- "
/* Main styling */
body {
  font-family: 'Inter', sans-serif;
  background-color: #f3e5f5; /* Light Lavender */
  color: #4a148c; /* Dark Purple */
}

/* Header styling with vibrant gradient */
.skin-blue .main-header .navbar {
  background: linear-gradient(135deg, #673ab7 0%, #512da8 100%) !important; /* Deep Purple gradient */
}

/* Styling for the .logo area, now containing project title and author */
.skin-blue .main-header .logo {
  background-color: transparent !important;
  color: #fff !important;
  font-weight: 700;
  border-bottom: 1px solid rgba(255, 255, 255, 0.2);
  /* Ensure logo content is properly aligned and responsive */
  text-align: left; /* Default alignment */
  padding-left: 15px; /* Spacing from left edge */
  display: flex; /* Use flexbox to align h4 and h6 vertically */
  flex-direction: column;
  justify-content: center; /* Center vertically within the logo area */
  align-items: flex-start; /* Align text to the left */
}

.skin-blue .main-header .logo:hover {
  background-color: transparent !important;
}

/* Adjust the logo's inner content for correct alignment */
.skin-blue .main-header .logo .logo-lg {
  display: flex; /* Ensure inner span is also flex for vertical alignment */
  flex-direction: column;
  justify-content: center;
  align-items: flex-start;
  height: 100%;
  padding: 0;
  white-space: nowrap; /* Prevent wrapping on desktop */
}
.skin-blue .main-header .logo .logo-lg h4,
.skin-blue .main-header .logo .logo-lg h6 {
  text-align: left; /* Explicitly align text within the logo */
  margin: 0; /* Remove default margins */
  padding: 0; /* Remove default padding */
}


/* Professional sidebar */
.skin-blue .main-sidebar {
  background-color: #512da8 !important; /* Medium Purple */
}

.skin-blue .sidebar-menu > li > a {
  border-left: 3px solid transparent;
  color: #e1bee7; /* Light Purple for sidebar text */
}

.skin-blue .sidebar-menu > li.active > a, 
.skin-blue .sidebar-menu > li > a:hover {
  border-left-color: #ffc107; /* Amber accent */
  background-color: #673ab7 !important; /* Deep Purple */
  color: #fff;
}

/* Enhanced boxes with subtle shadows */
.box {
  border-top: 3px solid #ffc107; /* Amber top border */
  border-radius: 8px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08) !important;
  transition: transform 0.2s, box-shadow 0.2s;
}

.box:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 16px rgba(0, 0, 0, 0.12) !important;
}

.box-header {
  border-bottom: 1px solid #e0e0e0;
  padding: 15px;
  font-weight: 600;
  color: #4a148c; /* Dark Purple for headers */
}

/* Professional value boxes */
.small-box {
  border-radius: 8px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  transition: transform 0.2s;
}

.small-box:hover {
  transform: translateY(-3px);
}

.small-box h3 {
  font-size: 24px;
  font-weight: 700;
  color: #4a148c; /* Dark Purple */
}

.small-box .icon {
  font-size: 70px;
  top: -10px;
  color: rgba(74, 20, 140, 0.2); /* Semi-transparent dark purple */
}

/* Enhanced buttons */
.btn {
  border-radius: 4px;
  font-weight: 500;
  transition: all 0.2s;
}

.btn-primary {
  background-color: #ffc107; /* Amber */
  border-color: #ffc107;
  color: #4a148c; /* Dark Purple */
}

.btn-primary:hover,
.btn-primary:focus {
  background-color: #ffb300; /* Slightly darker amber */
  border-color: #ffb300;
  color: #4a148c;
}

/* Switch styling for language toggle */
.form-switch .form-check-input {
  width: 2.5em;
  height: 1.25em;
  background-color: #ccc;
  border-radius: 1.25em;
  transition: all 0.2s ease-in-out;
  cursor: pointer;
}

.form-switch .form-check-input:checked {
  background-color: #ffc107; /* Amber when checked */
}

/* DT table styling */
.dataTables_wrapper .dataTables_filter input {
  border-radius: 4px;
  border: 1px solid #ccc;
  padding: 6px 12px;
}

/* Custom spinners */
.waiter-overlay-content .fa-spinner {
  font-size: 3em;
  color: #fff;
}

/* Footer styling */
.main-footer {
  background-color: #512da8; /* Medium Purple */
  color: #e1bee7; /* Light Purple */
  padding: 15px;
  text-align: center;
}

/* --- Responsive & Header Positioning Adjustments --- */

/* Revert navbar to default shinydashboard flex behavior or rely on floats */
.main-header .navbar {
  /* Let default block flow / floats work, shinydashboard manages layout somewhat */
  display: block;
  position: relative;
  height: 100%;
  padding: 0;
  background: linear-gradient(135deg, #673ab7 0%, #512da8 100%) !important; /* Keep gradient */
}

/* Position language toggle container to the far right */
.main-header .navbar .language-toggle-container {
  float: right !important; /* Float it to the right */
  height: 100%;
  display: flex; /* Use flex for vertical alignment of its content */
  align-items: center;
  padding-right: 15px; /* Spacing from the right edge */
  z-index: 1000;
}

/* Ensure language toggle switch itself is vertically centered */
.main-header .navbar .language-toggle-container .div {
  display: flex;
  align-items: center;
}

/* --- Sidebar State and Responsiveness --- */

/* For wide screens (desktop) */
@media (min-width: 768px) {
  /* Force sidebar to be always open */
  body:not(.sidebar-collapse) .main-sidebar, /* If not collapsed (normal state) */
  body.sidebar-collapse .main-sidebar /* If somehow collapsed (override) */
  {
    transform: translate(0, 0) !important; /* Keep it open */
    width: 300px !important; /* Set its width */
    left: 0 !important; /* Ensure it's visible */
  }

  /* Adjust content area for the open sidebar */
  .content-wrapper,
  .main-footer,
  .main-header .navbar {
    margin-left: 300px !important;
  }

  /* Hide the sidebar toggle button on desktop when sidebar is open */
  .main-header .navbar > .sidebar-toggle {
    display: none !important;
  }

  /* Adjust the logo title (project name/author) width for desktop to avoid overlap with language toggle */
  .skin-blue .main-header .logo {
    width: 300px !important; /* Match sidebar width */
    max-width: none !important; /* Override any max-width */
    overflow: hidden; /* Hide overflow if text is too long */
    text-overflow: ellipsis; /* Add ellipsis */
  }
}

/* For small screens (mobile) */
@media (max-width: 767px) {
  /* Ensure toggle button is visible on mobile */
  .main-header .navbar > .sidebar-toggle {
    display: block !important; /* Re-enable default display for mobile */
    float: left; /* Shinydashboard default */
  }
  /* Re-enable the collapse behavior on mobile */
  body.sidebar-mini.sidebar-collapse .main-sidebar {
    transform: translate(-100%, 0) !important; /* Collapse it off-screen */
  }
  /* Adjust the logo text size on mobile for better fit */
  .skin-blue .main-header .logo .logo-lg h4,
  .skin-blue .main-header .logo .logo-lg h6 {
    font-size: smaller;
    white-space: normal; /* Allow wrapping on mobile */
    text-overflow: clip; /* No ellipsis when wrapping */
  }
  /* Adjust language toggle container for mobile responsiveness */
  .main-header .navbar .language-toggle-container {
    padding-right: 5px; /* Less padding on mobile */
  }
}

/* Reset the body's sidebar-collapse margin to fix initial rendering issues */
.content-wrapper, .right-side, .main-footer {
    transition: margin-left .3s ease-in-out;
    margin-left: 0; /* Reset default to allow our media queries to control */
}
.sidebar-mini.sidebar-collapse .content-wrapper,
.sidebar-mini.sidebar-collapse .right-side,
.sidebar-mini.sidebar-collapse .main-footer {
  margin-left: 0 !important; /* Explicitly reset when collapsed, handled by JS normally */
}

/* Ensure initial sidebar state */
body:not(.sidebar-collapse) .main-sidebar {
    left: 0 !important;
}

body.sidebar-collapse .main-sidebar {
    left: -230px !important; /* Default collapsed state */
}

/* Override the default shinydashboard behavior that adds sidebar-collapse class on page load */
body.sidebar-mini {
  /* This prevents the body from starting with sidebar-collapse on desktop */
  /* This is a common point of conflict with shinyapps.io's iframe behavior */
}
"
# ======================
# UI Definition
# ======================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    # Project title and author name on the left (default position for dashboardHeader title)
    title = tagList(
      span(class = "logo-mini", "PMID"), # Short version for collapsed sidebar
      span(class = "logo-lg",
           div(
             h4(textOutput("dashboard_title_text"), style = "color: white; margin: 0; padding: 0; text-align: left;"),
             h6("by ", strong("Owais Ali Shah"), style = "color: #ffeb3b; margin: 0; padding: 0; font-size: 1.1em; text-align: left;")
           )
      )
    ),
    titleWidth = 300, # Give the title area a consistent width
    # Language toggle button moved to the far right using a new class
    tags$li(
      class = "dropdown language-toggle-container", # New class for the language toggle
      div(
        style = "margin-top: 10px; margin-right: 15px; display: inline-block;",
        span(style = "color:white;", "EN"),
        switchInput(
          inputId = "lang_toggle",
          value = FALSE,
          size = "mini"
        ),
        span(style = "color:white;", "UR")
      )
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem(textOutput("overview_details"), tabName = "overview_details", icon = icon("info-circle")),
      menuItem(textOutput("trends"), tabName = "trends", icon = icon("chart-line")),
      menuItem(textOutput("explorer"), tabName = "explorer", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    use_waiter(),
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "overview_details",
        fluidRow(
          valueBoxOutput("total_indicators_box"),
          valueBoxOutput("time_period_box"),
          valueBoxOutput("data_points_box"),
          valueBoxOutput("avg_value_overview_box")
        ),
        fluidRow(
          box(
            title = textOutput("key_indicators_overview"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            column(
              width = 7,
              withSpinner(highchartOutput("overview_chart", height = "400px"))
            ),
            column(
              width = 5,
              h4(textOutput("quick_insights")),
              htmlOutput("insights_text", class = "insights-box"),
              br(),
              downloadButton("download_overview_data", textOutput("download_data"), class = "btn-primary")
            )
          )
        ),
        fluidRow(
          box(
            title = textOutput("recent_data"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("latest_data_table")
          )
        ),
        fluidRow(
          box(
            title = textOutput("about_title"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4(strong("Project Details")),
            HTML(paste0("<h4>", textOutput("about_project_details"), "</h4>")),
            br(),
            h4(strong("About the Author")),
            HTML(paste0("<h4>", textOutput("about_author_details"), "</h4>"))
          )
        )
      ),
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = textOutput("trend_options"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            column(
              width = 4,
              selectInput("indicator_select", textOutput("select_indicator"),
                          choices = available_indicators,
                          selected = "Gross Domestic Product") # Default to GDP
            ),
            column(
              width = 4,
              selectInput("element_select", textOutput("select_element"),
                          choices = available_elements,
                          selected = "Annual growth US$") # Default to Annual growth
            ),
            column(
              width = 4,
              sliderInput("year_range", textOutput("year_range"),
                          min = min(available_years), max = max(available_years),
                          value = range(available_years), sep = "")
            ),
            column(
              width = 12, # Full width for chart type buttons
              radioButtons("trend_type", "Select Chart Type:",
                           choices = c("Line" = "line", "Area" = "area", "Bar" = "column"),
                           selected = "line", inline = TRUE)
            )
          )
        ),
        fluidRow(
          box(
            title = textOutput("indicator_trends"),
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            withSpinner(highchartOutput("trend_chart", height = "400px"))
          ),
          box(
            title = textOutput("statistical_summary"),
            status = "info",
            solidHeader = TRUE,
            width = 4,
            verbatimTextOutput("trend_summary")
          )
        )
      ),
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = textOutput("raw_data_explorer"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            withSpinner(DTOutput("data_table"))
          )
        )
      )
    ),
    # Footer section with your name
    tags$footer(
      tags$div(
        class = "main-footer",
        HTML("© 2025 Owais Ali Shah - All rights reserved.")
      )
    )
  )
)

# ======================
# Server Logic
# ======================
server <- function(input, output, session) {
  
  # Reactive value for current language
  current_language <- reactiveVal("en")
  
  # Initialize waiter
  waiter <- Waiter$new(
    id = c("overview_chart", "trend_chart", "data_table", "latest_data_table"),
    html = waiting_screen,
    color = "#512da8" # Medium Purple for waiter
  )
  
  # Observe language toggle
  observeEvent(input$lang_toggle, {
    if (input$lang_toggle) {
      current_language("ur")
      showNotification("Language switched to Urdu", type = "message", duration = 3)
    } else {
      current_language("en")
      showNotification("Language switched to English", type = "message", duration = 3)
    }
  })
  
  # Function to get translated text
  t <- function(key) {
    translations[[current_language()]][[key]]
  }
  
  # Reactive UI text elements
  # Using dashboard_title_text for the UI title, and dashboard_title for other places
  output$dashboard_title_text <- renderText({ t("dashboard_title") })
  output$dashboard_title <- renderText({ t("dashboard_title") }) # Keep for overview tab
  output$trends <- renderText({ t("trends") })
  output$explorer <- renderText({ t("explorer") })
  output$overview_details <- renderText({ t("overview_details") })
  output$about_title <- renderText({ t("about_title") })
  output$about_project_details <- renderText({ t("about_project_details") })
  output$about_author_details <- renderText({ t("about_author_details") })
  output$select_indicator <- renderText({ t("select_indicator") })
  output$select_element <- renderText({ t("select_element") })
  output$year_range <- renderText({ t("year_range") })
  output$key_indicators_overview <- renderText({ t("key_indicators_overview") })
  output$quick_insights <- renderText({ t("quick_insights") })
  output$recent_data <- renderText({ t("recent_data") })
  output$download_data <- renderText({ t("download_data") })
  output$trend_options <- renderText({ t("trend_options") })
  output$indicator_trends <- renderText({ t("indicator_trends") })
  output$statistical_summary <- renderText({ t("statistical_summary") })
  output$raw_data_explorer <- renderText({ t("raw_data_explorer") })
  
  # Reactive data based on filters for Trend Analysis
  filtered_data <- reactive({
    req(input$indicator_select, input$element_select) # Ensure an indicator and element are selected
    data %>%
      filter(
        Indicator == input$indicator_select,
        Element == input$element_select,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
  # ================= Value Boxes =================
  output$total_indicators_box <- renderValueBox({
    valueBox(
      value = length(unique(data$Indicator)),
      subtitle = t("total_indicators"),
      icon = icon("chart-pie"),
      color = "purple", # Using a warm tone
      width = 3
    )
  })
  
  output$time_period_box <- renderValueBox({
    valueBox(
      value = paste(min(data$Year), "-", max(data$Year)),
      subtitle = t("time_period"),
      icon = icon("calendar-alt"),
      color = "light-blue", # Using a standard color that blends
      width = 3
    )
  })
  
  output$data_points_box <- renderValueBox({
    valueBox(
      value = format(nrow(data), big.mark = ","),
      subtitle = t("data_points"),
      icon = icon("database"),
      color = "maroon", # Using a warm tone
      width = 3
    )
  })
  
  output$avg_value_overview_box <- renderValueBox({
    # Calculate average of a default indicator for the overview box, e.g., 'Gross Domestic Product' with 'Annual growth US$'
    default_indicator_avg <- data %>%
      filter(Indicator == "Gross Domestic Product", Element == "Annual growth US$") %>%
      pull(Value) %>%
      mean(na.rm = TRUE)
    
    valueBox(
      value = paste0(round(default_indicator_avg, 2), "%"),
      subtitle = paste(t("average_value"), "(GDP Growth)"), # Specify for which indicator
      icon = icon("calculator"),
      color = "orange", # Using a standard color that blends
      width = 3
    )
  })
  
  # ================= Overview Tab Content =================
  
  # Latest data table for overview
  output$latest_data_table <- renderDT({
    waiter$show()
    on.exit(waiter$hide())
    
    # Get the latest year available
    latest_year <- max(data$Year, na.rm = TRUE)
    
    latest_data <- data %>%
      filter(Year == latest_year) %>%
      select(Indicator, Element, Year, Value, Unit) %>%
      arrange(Indicator, Element) %>%
      rename(
        Indicator = Indicator,
        Element = Element,
        Year = Year,
        `Latest Value` = Value,
        Unit = Unit
      )
    
    datatable(
      latest_data,
      options = list(
        dom = 't', # Only show table
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 'Latest Value', digits = 2)
  })
  
  # Quick Insights text
  output$insights_text <- renderUI({
    # Insights based on 'Gross Domestic Product' and 'Annual growth US$'
    gdp_growth_data <- data %>%
      filter(Indicator == "Gross Domestic Product", Element == "Annual growth US$")
    
    if (nrow(gdp_growth_data) == 0) {
      return(HTML("<div style='font-size: 14px;'><p>No data available for Gross Domestic Product Annual Growth.</p></div>"))
    }
    
    avg_gdp_growth <- mean(gdp_growth_data$Value, na.rm = TRUE)
    min_gdp_growth <- min(gdp_growth_data$Value, na.rm = TRUE)
    max_gdp_growth <- max(gdp_growth_data$Value, na.rm = TRUE)
    
    # Trend for GDP Growth
    gdp_growth_by_year <- gdp_growth_data %>%
      group_by(Year) %>%
      summarize(Avg_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(Year)
    
    trend_text <- "stable"
    if(nrow(gdp_growth_by_year) >= 2) {
      # Check if the last value is significantly higher than the first value
      if(last(gdp_growth_by_year$Avg_Value) > (first(gdp_growth_by_year$Avg_Value) * 1.05)) { # 5% increase considered significant
        trend_text <- "increasing"
      } else if (last(gdp_growth_by_year$Avg_Value) < (first(gdp_growth_by_year$Avg_Value) * 0.95)) { # 5% decrease considered significant
        trend_text <- "decreasing"
      }
    }
    
    HTML(paste0(
      "<div style='font-size: 14px; line-height: 1.6'>",
      "<p><strong>Indicator:</strong> Gross Domestic Product (GDP)</p>",
      "<p><strong>Element:</strong> Annual growth US$</p>",
      "<p><strong>Trend:</strong> <span style='color:", ifelse(trend_text == "increasing", "#1b5e20", ifelse(trend_text == "decreasing", "#b71c1c", "#4a148c")), "'>", trend_text, "</span></p>", # Green for increasing, Red for decreasing
      "<p><strong>Range (%):</strong> ", round(min_gdp_growth, 2), "% to ", round(max_gdp_growth, 2), "%</p>",
      "<p><strong>Average (%):</strong> ", round(avg_gdp_growth, 2), "%</p>",
      "<p><strong>Latest Value (", max(gdp_growth_by_year$Year), "):</strong> ",
      round(last(gdp_growth_by_year$Avg_Value), 2),
      "%</p>",
      "</div>"
    ))
  })
  
  # Overview chart - Average value per indicator for a default element
  output$overview_chart <- renderHighchart({
    waiter$show()
    on.exit(waiter$hide())
    
    # Filter for a common element like 'Annual growth US$' or 'Value US$' for overview comparison
    overview_filter_element <- "Annual growth US$" 
    
    summary_data <- data %>%
      filter(Element == overview_filter_element) %>%
      group_by(Indicator) %>%
      summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Average_Value)) %>%
      head(10) # Show top 10 indicators for clarity
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = paste(t("key_indicators_overview"), " (", overview_filter_element, ")")) %>%
      hc_xAxis(categories = summary_data$Indicator, title = list(text = "Indicator")) %>%
      hc_yAxis(title = list(text = paste("Average Value (", overview_filter_element, ")"))) %>%
      hc_add_series(
        name = "Average Value",
        data = summary_data$Average_Value,
        color = "#ffc107" # Amber accent
      ) %>%
      hc_tooltip(
        valueDecimals = 2,
        shared = TRUE,
        crosshairs = TRUE,
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.2f}</b><br/>'
      ) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE))) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # Download handler for raw data
  output$download_overview_data <- downloadHandler(
    filename = function() {
      paste0("pakistan_macro_indicators_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # ================= Trend Analysis Tab Content =================
  
  # Trend chart for selected item and element
  output$trend_chart <- renderHighchart({
    waiter$show()
    on.exit(waiter$hide())
    
    current_data <- filtered_data()
    
    if (nrow(current_data) == 0) {
      return(highchart() %>% hc_title(text = "No data available for the selected indicator, element, and year range."))
    }
    
    # Aggregate data by year, in case an indicator/element has multiple entries per year
    plot_data <- current_data %>%
      group_by(Year) %>%
      summarize(Aggregated_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      arrange(Year)
    
    # Determine unit for Y-axis label
    selected_unit <- unique(current_data$Unit)[1]
    
    hc_type <- ifelse(input$trend_type == "line", "line",
                      ifelse(input$trend_type == "area", "area", "column"))
    
    highchart() %>%
      hc_chart(type = hc_type) %>%
      hc_title(text = paste(t("indicator_trends"), ":", input$indicator_select, " (", input$element_select, ")")) %>%
      hc_xAxis(categories = plot_data$Year, title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = paste("Value (", selected_unit, ")"))) %>%
      hc_add_series(
        name = paste(input$indicator_select, " (", input$element_select, ")"),
        data = plot_data$Aggregated_Value,
        color = "#a1887f" # Medium Brown (using a muted brown for continuity with previous example, could be purple here)
      ) %>%
      hc_tooltip(
        valueDecimals = 2,
        shared = TRUE,
        crosshairs = TRUE,
        pointFormat = paste0('<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.2f} ', selected_unit, '</b><br/>')
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # Trend summary for selected item and element
  output$trend_summary <- renderPrint({
    current_data <- filtered_data()
    summary(current_data$Value)
  })
  
  # ================= Data Explorer Tab Content =================
  
  # Data table for raw data
  output$data_table <- renderDT({
    waiter$show()
    on.exit(waiter$hide())
    
    datatable(
      data,
      extensions = c('Buttons', 'Scroller', 'Responsive'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn-sm'),
          list(extend = 'csv', className = 'btn-sm'),
          list(extend = 'excel', className = 'btn-sm'),
          list(extend = 'pdf', className = 'btn-sm')
        ),
        scrollX = TRUE,
        scrollY = "500px",
        scroller = TRUE,
        pageLength = 10,
        responsive = TRUE,
        autoWidth = TRUE,
        language = list(
          search = "Filter:",
          paginate = list(
            `first` = "First", `last` = "Last", `next` = "Next", `previous` = "Previous"
          )
        )
      ),
      class = "cell-border stripe hover",
      rownames = FALSE,
      filter = 'top'
    ) %>%
      formatRound(columns = 'Value', digits = 2)
  })
}

shinyApp(ui, server)
