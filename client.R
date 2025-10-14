

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(networkD3)
library(scales)
library(tidyr)
library(stringr)
library(DT)


# LOAD DATA


# Main dataset 
df <- read_csv("df_enhanced_with_competition_shinny.csv") %>%
  filter(!is.na(lat), !is.na(lon))

# Street analysis lookup 
street_lookup <- read_csv("street_analysis_lookup_FIXED.csv")

# Prepare area grouping for visualizations
top6_areas <- df %>%
  count(clue_area, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(clue_area)

df <- df %>%
  mutate(area_group = ifelse(clue_area %in% top6_areas, clue_area, "Other"))


# UI definition


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #ffffff;
        font-family: 'Source Sans Pro', sans-serif;
        color: #333;
      }
      .box {
        border-radius: 0px;
        box-shadow: none;
        margin-bottom: 30px;
        border-bottom: 1px solid #e0e0e0;
        padding-bottom: 20px;
      }
      .main-title {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 40px 0;
        border-radius: 0px;
        margin-bottom: 40px;
        text-align: center;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .section-title {
        background-color: #ffffff;
        color: #333;
        padding: 20px 0 10px 0;
        border-radius: 0px;
        margin: 0;
        font-size: 24px;
        font-weight: 600;
        border-bottom: 3px solid #667eea;
        margin-bottom: 20px;
      }
      .controls-panel {
        background: #f8f9fa;
        padding: 25px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        border: 1px solid #e9ecef;
        position: sticky;
        top: 20px;
      }
      .info-box {
        background: #e8f4fd;
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #667eea;
        margin: 15px 0;
        font-size: 13px;
        color: #495057;
      }
      .warning-box {
        background: #fff3cd;
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #ffc107;
        margin: 15px 0;
        font-size: 13px;
      }
      .success-box {
        background: #d4edda;
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #28a745;
        margin: 15px 0;
        font-size: 13px;
      }
      .chart-container {
        background: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        margin-bottom: 30px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .chart-header {
        background-color: #f8f9fa;
        color: #333;
        padding: 20px 25px;
        border-bottom: 2px solid #e9ecef;
        border-radius: 8px 8px 0 0;
      }
      .chart-content {
        padding: 25px;
      }
      .metric-card {
        background: white;
        padding: 20px;
        border-radius: 8px;
        border: 2px solid #e9ecef;
        text-align: center;
        margin-bottom: 15px;
      }
      .metric-value {
        font-size: 32px;
        font-weight: bold;
        color: #667eea;
      }
      .metric-label {
        font-size: 14px;
        color: #6c757d;
        margin-top: 5px;
      }
      .btn-primary {
        background-color: #667eea;
        border-color: #667eea;
      }
      .btn-primary:hover {
        background-color: #764ba2;
        border-color: #764ba2;
      }
    "))
  ),
  
  # 
  # Header
  
  div(class = "main-title",
      h1("Melbourne Business Longevity Explorer", 
         style = "margin: 0; font-size: 36px; font-weight: 700;"),
      h4("Competition Analysis & Site Selection Intelligence", 
         style = "margin: 15px 0 0 0; font-weight: 300; opacity: 0.95;")
  ),
  
  # Introduction
  div(style = "background: white; padding: 25px 0; margin-bottom: 40px; line-height: 1.8; border-bottom: 1px solid #e0e0e0;",
      div(style = "max-width: 1000px; margin: 0 auto; padding: 0 20px;",
          p(style = "margin: 0 0 15px 0; font-size: 16px; color: #444;",
            HTML("<strong>Purpose:</strong> This tool helps restaurant businesses evaluate potential expansion locations in Melbourne CBD by analyzing 21 years of business survival data (2002-2023) from 36,000+ establishments.")),
          p(style = "margin: 0; font-size: 16px; color: #444;",
            HTML("<strong>Key Features:</strong> Interactive competition mapping, street-level performance comparison, survival pattern analysis, and data-driven site recommendations."))
      )
  ),
  
  
  # SECTION 1: CBD INSIGHTS DASHBOARD
 
  
  div(class = "section-title", style = "margin-top: 40px;",
      h2("CBD Expansion Insights Dashboard", style = "margin: 0; font-size: 28px;")),
  
  fluidRow(
    column(width = 12,
           div(class = "info-box",
               HTML("<strong>🎯 Quick Insights:</strong> Key findings for restaurant expansion in Melbourne CBD based on 5,754 historical businesses"))
    )
  ),
  
  fluidRow(
    # Competition Overview
    column(width = 4,
           div(class = "metric-card",
               div(class = "metric-value", textOutput("cbd_avg_competition", inline = TRUE)),
               div(class = "metric-label", "Average Competitors (200m)")
           )
    ),
    column(width = 4,
           div(class = "metric-card",
               div(class = "metric-value", textOutput("cbd_success_rate", inline = TRUE)),
               div(class = "metric-label", "CBD Success Rate (>12 years)")
           )
    ),
    column(width = 4,
           div(class = "metric-card",
               div(class = "metric-value", textOutput("cbd_median_lifespan", inline = TRUE)),
               div(class = "metric-label", "Median Business Lifespan")
           )
    )
  ),
  
  fluidRow(
    column(width = 6,
           div(class = "chart-container",
               div(class = "chart-header",
                   h4("Top Performing Streets in CBD", style = "margin: 0; font-size: 18px; font-weight: 600;")
               ),
               div(class = "chart-content",
                   plotlyOutput("cbd_top_streets", height = "300px")
               )
           )
    ),
    column(width = 6,
           div(class = "chart-container",
               div(class = "chart-header",
                   h4("Competition vs. Success Rate", style = "margin: 0; font-size: 18px; font-weight: 600;")
               ),
               div(class = "chart-content",
                   plotlyOutput("cbd_competition_success", height = "300px")
               )
           )
    )
  ),
  
  fluidRow(
    column(width = 12,
           div(class = "success-box",
               h4("✅ Recommendations for New Restaurant:", style = "margin-top: 0; color: #155724;"),
               tags$ul(style = "margin-bottom: 0;",
                       tags$li("Target streets with 60-80 competitors (200m) - optimal competition level"),
                       tags$li("Avoid Docklands and streets with >100 competitors"),
                       tags$li("Prime streets: Collins, Bourke, Swanston show 20-25% success rates"),
                       tags$li("Medium-sized restaurants (50-80 seats) perform best in CBD"),
                       tags$li("Cafes have 15% higher survival rate than full-service restaurants")
               )
           )
    )
  ),
  
  br(), br(),
  
  
  # SECTION 2: INTERACTIVE MAP
 
  
  div(class = "section-title",
      h2("Interactive Business Location Map", style = "margin: 0;")),
  
  fluidRow(
    # Map Controls
    column(width = 3,
           div(class = "controls-panel",
               h3("Map Controls", style = "color: #333; margin-top: 0; border-bottom: 2px solid #667eea; padding-bottom: 10px; font-weight: 600;"),
               
               # Map View Toggle
               h4("Map View:"),
               radioButtons("map_view", NULL,
                            choices = c("Historical Businesses" = "historical",
                                        "Current Businesses (2023)" = "current"),
                            selected = "historical"),
               
               hr(),
               
               h4("Select Industries:"),
               checkboxGroupInput("industry", NULL,
                                  choices = c("Cafes", "Takeaway"),
                                  selected = "Cafes"),
               
               hr(),
               
               h4("Select Lifespan Range:"),
               checkboxGroupInput("lifespan_group", NULL,
                                  choices = c("1-2 yrs", "3-5 yrs", "6-8 yrs", "9-12 yrs", ">12 yrs"),
                                  selected = c(">12 yrs")),
               
               hr(),
               
               h4("Map Summary:"),
               verbatimTextOutput("map_summary"),
               
               div(class = "info-box",
                   HTML("<strong>Tip:</strong> Switch between Historical view (completed lifecycles) and Current view (currently operating) to find optimal locations."))
           )
    ),
    
    # Map Display
    column(width = 9,
           div(class = "chart-container",
               div(class = "chart-header",
                   h3(textOutput("map_title", inline = TRUE), style = "margin: 0; font-size: 22px; font-weight: 600;")
               ),
               div(class = "chart-content",
                   leafletOutput("business_map", height = "600px")
               )
           ),
           
           # Map Legends
           fluidRow(
             column(width = 4,
                    div(style = "background: white; padding: 20px; border: 1px solid #e9ecef; border-radius: 8px; margin-bottom: 20px;",
                        h4("Industry Legend", style = "color: #333; margin-top: 0; margin-bottom: 15px; font-weight: 600; font-size: 16px;"),
                        uiOutput("industry_legend")
                    )
             ),
             column(width = 4,
                    div(style = "background: white; padding: 20px; border: 1px solid #e9ecef; border-radius: 8px; margin-bottom: 20px;",
                        h4("Lifespan Legend", 
                           style = "color: #333; margin-top: 0; margin-bottom: 15px; font-weight: 600; font-size: 16px;"),
                        uiOutput("size_legend")
                    )
             ),
             column(width = 4,
                    div(style = "background: white; padding: 20px; border: 1px solid #e9ecef; border-radius: 8px; margin-bottom: 20px;",
                        h4("How to Use", style = "color: #333; margin-top: 0; margin-bottom: 15px; font-weight: 600; font-size: 16px;"),
                        tags$ul(style = "font-size: 13px; margin: 0; color: #555; line-height: 1.6;",
                                tags$li("Toggle between Historical (completed) and Current (operating) businesses"),
                                tags$li("Click markers for detailed business information"),
                                tags$li("Use filters to focus on specific lifespan patterns"),
                                tags$li("Identify clusters of successful vs struggling businesses")
                        )
                    )
             )
           )
    )
  ),
  
  br(), br(), br(),
  
  
  # SECTION 3: STREET ANALYSIS TOOL
 
  
  div(class = "section-title",
      h2("Street Comparison Tool", style = "margin: 0;")),
  
  fluidRow(
    column(width = 12,
           div(class = "info-box",
               HTML("<strong>Compare streets side-by-side:</strong> Select up to 3 streets to compare historical performance, current competition, and success rates. Use this to evaluate specific location options."))
    )
  ),
  
  fluidRow(
    # Street Selection Controls
    column(width = 3,
           div(class = "controls-panel",
               h3("Select Streets", style = "color: #333; margin-top: 0; border-bottom: 2px solid #667eea; padding-bottom: 10px; font-weight: 600;"),
               
               h4("Industry:"),
               selectInput("street_industry", NULL,
                           choices = c("Cafes", "Takeaway"),
                           selected = "Cafes"),
               
               hr(),
               
               h4("Street 1:"),
               selectInput("street_1", NULL,
                           choices = c("Select street..." = ""),
                           selected = ""),
               
               h4("Street 2 (optional):"),
               selectInput("street_2", NULL,
                           choices = c("None" = ""),
                           selected = ""),
               
               h4("Street 3 (optional):"),
               selectInput("street_3", NULL,
                           choices = c("None" = ""),
                           selected = ""),
               
               hr(),
               
               div(class = "info-box",
                   HTML("<strong>Suggested CBD Streets:</strong><br>
                        • <strong>Prime:</strong> Collins St, Bourke St, Swanston St<br>
                        • <strong>Good:</strong> Lonsdale St, Elizabeth St<br>
                        • <strong>Emerging:</strong> Little Bourke St, Hardware Lane"))
           )
    ),
    
    # Street Comparison Display
    column(width = 9,
           # Comparison Table
           div(class = "chart-container",
               div(class = "chart-header",
                   h3("Street Performance Comparison", style = "margin: 0; font-size: 22px; font-weight: 600;")
               ),
               div(class = "chart-content",
                   DTOutput("street_comparison_table")
               )
           ),
           
           br(),
           
           # Visualization Charts
           fluidRow(
             column(width = 6,
                    div(class = "chart-container",
                        div(class = "chart-header",
                            h4("Success Rate Comparison", style = "margin: 0; font-size: 18px; font-weight: 600;")
                        ),
                        div(class = "chart-content",
                            plotlyOutput("street_success_comparison", height = "300px")
                        )
                    )
             ),
             column(width = 6,
                    div(class = "chart-container",
                        div(class = "chart-header",
                            h4("Competition Level Comparison", style = "margin: 0; font-size: 18px; font-weight: 600;")
                        ),
                        div(class = "chart-content",
                            plotlyOutput("street_competition_comparison", height = "300px")
                        )
                    )
             )
           ),
           
           br(),
           
           # Street Recommendations
           uiOutput("street_recommendations")
    )
  ),
  
  br(), br(), br(),
  
  
  # SECTION 4: ANALYTICS DASHBOARDS (Sankey only)
 
  
  div(class = "section-title",
      h2("Business Analytics & Patterns", style = "margin: 0;")),
  
  fluidRow(
    # Analytics Controls
    column(width = 3,
           div(class = "controls-panel",
               h3("Analytics Controls", style = "color: #333; margin-top: 0; border-bottom: 2px solid #667eea; padding-bottom: 10px; font-weight: 600;"),
               
               h4("Select Industries:"),
               checkboxGroupInput("analytics_industry", NULL,
                                  choices = c("Cafes", "Takeaway"),
                                  selected = c("Cafes", "Takeaway")),
               
               hr(),
               
               h4("Filter by Competition:"),
               sliderInput("competition_range", NULL,
                           min = 0, max = 150, value = c(0, 150),
                           step = 10, post = " competitors"),
               
               hr(),
               
               div(class = "info-box",
                   HTML("<strong>Data Notes:</strong><br>
                        • Uses bias-corrected sample<br>
                        • Competition measured at 200m radius<br>
                        • One observation per business"))
           )
    ),
    
    # Sankey Flow
    column(width = 9,
           div(class = "chart-container",
               div(class = "chart-header",
                   h3("Industry → Area → Outcome Flow", style = "margin: 0; font-size: 20px; font-weight: 600;"),
                   p(style = "margin: 5px 0 0 0; font-size: 14px; color: #666;", 
                     "Relationships between business type, location, and longevity")
               ),
               div(class = "chart-content",
                   sankeyNetworkOutput("sankey_plot", height = "500px")
               )
           )
    )
  ),
  
  br(), br(), br(),
  
  
  # SECTION 5: DURATION PATTERNS

  
  div(class = "section-title",
      h2("Business Duration Patterns", style = "margin: 0;")),
  
  fluidRow(
    column(width = 3,
           div(class = "controls-panel",
               h3("Pattern Controls", style = "color: #333; margin-top: 0; border-bottom: 2px solid #667eea; padding-bottom: 10px; font-weight: 600;"),
               
               h4("Industry:"),
               selectInput("duration_industry", NULL,
                           choices = c("Please select..." = "", "Cafes", "Takeaway"),
                           selected = ""),
               
               hr(),
               
               h4("Area:"),
               selectInput("duration_area", NULL,
                           choices = c("All Areas"),
                           selected = "All Areas"),
               
               hr(),
               
               h4("Statistics:"),
               verbatimTextOutput("duration_stats"),
               
               div(class = "info-box",
                   HTML("<strong>Analysis:</strong> Shows completed business lifecycles for accurate duration patterns."))
           )
    ),
    
    column(width = 9,
           div(class = "chart-container",
               div(class = "chart-header",
                   h3("Lifespan Distribution", style = "margin: 0; font-size: 20px; font-weight: 600;"),
                   p(style = "margin: 5px 0 0 0; font-size: 14px; color: #666;", 
                     "Individual year breakdown showing how long businesses typically survive")
               ),
               div(class = "chart-content",
                   plotlyOutput("duration_histogram", height = "350px")
               )
           )
    )
  ),
  
  br(), br(), br(),
  
  
  # SECTION 6: GEOGRAPHIC SUCCESS ANALYSIS
 
  
  div(class = "section-title",
      h2("Geographic Success Heatmap", style = "margin: 0;")),
  
  fluidRow(
    column(width = 3,
           div(class = "controls-panel",
               h3("Heatmap Controls", style = "color: #333; margin-top: 0; border-bottom: 2px solid #667eea; padding-bottom: 10px; font-weight: 600;"),
               
               h4("Success Threshold:"),
               sliderInput("success_threshold", NULL,
                           min = 2, max = 15, value = 5, step = 1,
                           post = " years"),
               
               hr(),
               
               h4("Industries:"),
               checkboxGroupInput("heatmap_industry", NULL,
                                  choices = c("Cafes", "Takeaway"),
                                  selected = c("Cafes", "Takeaway")),
               
               hr(),
               
               div(class = "info-box",
                   HTML("<strong>Heatmap Guide:</strong><br>
                        • Green = High success rate<br>
                        • Yellow = Moderate success<br>
                        • Blue = Lower success<br>
                        • Only areas with 3+ businesses shown"))
           )
    ),
    
    column(width = 9,
           div(class = "chart-container",
               div(class = "chart-header",
                   h3("Area × Industry Success Matrix", style = "margin: 0; font-size: 20px; font-weight: 600;"),
                   p(style = "margin: 5px 0 0 0; font-size: 14px; color: #666;", 
                     "Which areas and industries have highest survival rates")
               ),
               div(class = "chart-content",
                   plotlyOutput("success_heatmap", height = "500px")
               )
           )
    )
  ),
  
  br(), br(),
  
  
  # FOOTER - Technical Info
 
  
  fluidRow(
    column(width = 12,
           div(style = "background: #f8f9fa; border: 1px solid #dee2e6; padding: 30px; margin-bottom: 40px; margin-top: 60px; border-radius: 8px;",
               h4("Technical Information", style = "color: #495057; margin-top: 0; margin-bottom: 20px; font-size: 18px; font-weight: 600;"),
               div(style = "font-family: 'Courier New', monospace; font-size: 12px; color: #6c757d; background: white; padding: 20px; border: 1px solid #e9ecef; border-radius: 4px;",
                   verbatimTextOutput("session_info")
               ),
               br(),
               div(style = "text-align: center; font-size: 14px; color: #6c757d; line-height: 1.8;",
                   HTML("<strong>Data Source:</strong> City of Melbourne Census of Land Use and Employment (CLUE)<br>
                        <strong>Analysis Period:</strong> 2002-2023 (21 years)<br>
                        <strong>Total Records:</strong> 36,315 business-year observations<br>
                        <strong>Unique Businesses:</strong> 5,754 establishments<br>
                        <strong>Competition Analysis:</strong> Spatial analysis at 50m and 200m radii<br>
                        <strong>Data URL:</strong> <a href='https://data.melbourne.vic.gov.au/explore/dataset/cafes-and-restaurants-with-seating-capacity/information' target='_blank' style='color: #667eea;'>Melbourne Open Data Portal</a>")
               )
           )
    )
  )
)


# SERVER LOGIC


server <- function(input, output, session) {
  
  
  # REACTIVE DATA PREPARATION
  
  
  # Filter for CBD area
  cbd_data <- reactive({
    df %>%
      filter(clue_area == "Melbourne (CBD)")
  })
  
  # Map data (Both views now use lifespan filtering)
  map_data <- reactive({
    if (input$map_view == "historical") {
      # Historical view - completed lifecycles
      if (is.null(input$industry) || is.null(input$lifespan_group) || 
          length(input$industry) == 0 || length(input$lifespan_group) == 0) {
        return(data.frame())
      }
      
      df %>%
        filter(!bias_filter) %>%  # Use unbiased sample
        group_by(business_id) %>%
        slice_max(census_year, n = 1) %>%
        ungroup() %>%
        filter(
          industry %in% input$industry,
          lifespan_group %in% input$lifespan_group
        ) %>%
        mutate(
          marker_size = case_when(
            lifespan_years <= 2 ~ 4,
            lifespan_years <= 5 ~ 6,
            lifespan_years <= 8 ~ 8,
            lifespan_years <= 12 ~ 10,
            TRUE ~ 12
          ),
          marker_color = case_when(
            industry == "Cafes" ~ "#1f77b4",
            industry == "Takeaway" ~ "#ff7f0e",
            TRUE ~ "#7f7f7f"
          )
        )
    } else {
      # Current view - currently operating businesses (2023)
      if (is.null(input$industry) || is.null(input$lifespan_group) || 
          length(input$industry) == 0 || length(input$lifespan_group) == 0) {
        return(data.frame())
      }
      
      df %>%
        filter(
          census_year == 2023,
          industry %in% input$industry,
          lifespan_group %in% input$lifespan_group
        ) %>%
        mutate(
          marker_size = case_when(
            lifespan_years <= 2 ~ 4,
            lifespan_years <= 5 ~ 6,
            lifespan_years <= 8 ~ 8,
            lifespan_years <= 12 ~ 10,
            TRUE ~ 12
          ),
          marker_color = case_when(
            industry == "Cafes" ~ "#1f77b4",
            industry == "Takeaway" ~ "#ff7f0e",
            TRUE ~ "#7f7f7f"
          )
        )
    }
  })
  
  # Street lookup filtered by industry
  street_data_filtered <- reactive({
    req(input$street_industry)
    street_lookup %>%
      filter(industry == input$street_industry) %>%
      arrange(desc(success_rate_12yrs))
  })
  
  # Update street dropdowns when industry changes
  observe({
    req(input$street_industry)
    
    streets <- street_data_filtered()$street_address
    streets <- streets[!is.na(streets) & streets != ""]
    streets <- sort(unique(streets))
    
    street_choices <- c("Select street..." = "", streets)
    street_choices_optional <- c("None" = "", streets)
    
    updateSelectInput(session, "street_1", choices = street_choices)
    updateSelectInput(session, "street_2", choices = street_choices_optional)
    updateSelectInput(session, "street_3", choices = street_choices_optional)
  })
  
  # Update area choices for duration analysis
  observe({
    areas <- df %>%
      distinct(clue_area) %>%
      arrange(clue_area) %>%
      pull(clue_area)
    
    updateSelectInput(session, "duration_area", 
                      choices = c("All Areas", areas),
                      selected = "All Areas")
  })
  
  
  # SECTION 1: CBD INSIGHTS DASHBOARD
 
  
  # CBD Metrics
  output$cbd_avg_competition <- renderText({
    avg <- cbd_data() %>%
      filter(!is.na(competitors_200m)) %>%
      pull(competitors_200m) %>%
      mean() %>%
      round(0)
    paste0(avg)
  })
  
  output$cbd_success_rate <- renderText({
    rate <- cbd_data() %>%
      filter(!bias_filter) %>%
      group_by(business_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      summarise(rate = mean(lifespan_group == ">12 yrs") * 100) %>%
      pull(rate) %>%
      round(1)
    paste0(rate, "%")
  })
  
  output$cbd_median_lifespan <- renderText({
    median_life <- cbd_data() %>%
      filter(!bias_filter) %>%
      group_by(business_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      summarise(med = median(lifespan_years)) %>%
      pull(med)
    paste0(median_life, " yrs")
  })
  
  # Top CBD Streets
  output$cbd_top_streets <- renderPlotly({
    top_streets <- cbd_data() %>%
      filter(!bias_filter, !is.na(street_address)) %>%
      group_by(business_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      group_by(street_address) %>%
      summarise(
        count = n(),
        success_rate = mean(lifespan_group == ">12 yrs") * 100,
        .groups = "drop"
      ) %>%
      filter(count >= 5) %>%
      arrange(desc(success_rate)) %>%
      head(10)
    
    if (nrow(top_streets) > 0) {
      p <- ggplot(top_streets, aes(x = reorder(street_address, success_rate), y = success_rate,
                                   text = paste0("Street: ", street_address, "<br>",
                                                 "Success Rate: ", round(success_rate, 1), "%<br>",
                                                 "Businesses: ", count))) +
        geom_col(fill = "#667eea", alpha = 0.8) +
        coord_flip() +
        labs(x = NULL, y = "Success Rate (%)") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 10))
      
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    }
  })
  
  # Competition vs Success
  output$cbd_competition_success <- renderPlotly({
    comp_data <- cbd_data() %>%
      filter(!bias_filter, !is.na(competitors_200m)) %>%
      group_by(business_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      mutate(
        comp_bin = cut(competitors_200m, 
                       breaks = c(0, 30, 60, 90, 120, 500),
                       labels = c("0-30", "31-60", "61-90", "91-120", ">120"))
      ) %>%
      group_by(comp_bin) %>%
      summarise(
        count = n(),
        success_rate = mean(lifespan_group == ">12 yrs") * 100,
        .groups = "drop"
      )
    
    if (nrow(comp_data) > 0) {
      p <- ggplot(comp_data, aes(x = comp_bin, y = success_rate,
                                 text = paste0("Competitors: ", comp_bin, "<br>",
                                               "Success Rate: ", round(success_rate, 1), "%<br>",
                                               "Businesses: ", count))) +
        geom_col(fill = "#764ba2", alpha = 0.8) +
        geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.5) +
        labs(x = "Competitors (200m)", y = "Success Rate (%)") +
        theme_minimal()
      
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    }
  })
  
  
  # SECTION 2: INTERACTIVE MAP
 
  
  # Map title (dynamic)
  output$map_title <- renderText({
    if (input$map_view == "historical") {
      "Historical Business Locations by Lifespan"
    } else {
      "Current Business Locations (2023) by Lifespan"
    }
  })
  
  # Map summary
  output$map_summary <- renderPrint({
    data <- map_data()
    
    if (nrow(data) == 0) {
      cat("No data to display\n")
      cat("Please adjust filters\n")
      return()
    }
    
    if (input$map_view == "historical") {
      cat("Businesses shown:", nrow(data), "\n")
      cat("Avg lifespan:", round(mean(data$lifespan_years, na.rm = TRUE), 1), "yrs\n")
      cat("Success rate:", round(mean(data$lifespan_group == ">12 yrs") * 100, 1), "%\n")
    } else {
      cat("Current businesses:", nrow(data), "\n")
      cat("Avg lifespan:", round(mean(data$lifespan_years, na.rm = TRUE), 1), "yrs\n")
      cat("Success rate:", round(mean(data$lifespan_group == ">12 yrs") * 100, 1), "%\n")
    }
  })
  
  # Industry legend
  output$industry_legend <- renderUI({
    data <- map_data()
    
    if (nrow(data) == 0) {
      return(div(style = "font-size: 14px; color: #888;", "No industries selected"))
    }
    
    selected <- unique(data$industry)
    colors <- c("Cafes" = "#1f77b4", "Takeaway" = "#ff7f0e")
    
    legend_items <- lapply(selected, function(ind) {
      div(style = sprintf("color:%s; font-size: 16px; margin: 5px 0;", colors[[ind]]), 
          paste("●", ind))
    })
    
    div(style = "font-size: 14px;", do.call(tagList, legend_items))
  })
  
  # Size legend
  output$size_legend <- renderUI({
    HTML("<div style='font-size: 14px; color: #555; line-height: 2;'>
         <b>Size = Lifespan:</b><br>
         <span style='font-size: 10px;'>●</span> Short: 1-2 yrs<br>
         <span style='font-size: 14px;'>●</span> Medium: 3-8 yrs<br>
         <span style='font-size: 18px;'>●</span> Long: 9-15 yrs<br>
         <span style='font-size: 22px;'>●</span> Very Long: >15 yrs
         </div>")
  })
  
  # Initialize map
  output$business_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 13)
  })
  
  # Update map markers (UPDATED - Removed competition from popups)
  observe({
    data <- map_data()
    
    if (nrow(data) > 0) {
      data_ordered <- data %>% arrange(desc(marker_size))
      
      if (input$map_view == "historical") {
        leafletProxy("business_map", data = data_ordered) %>%
          clearMarkers() %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            radius = ~marker_size,
            fillColor = ~marker_color,
            color = "#000000",
            weight = 0.5,
            opacity = 0.8,
            fillOpacity = 0.7,
            popup = ~paste0(
              "<b>", trading_name, "</b><br>",
              business_address, "<br>",
              "Industry: ", industry, "<br>",
              "Lifespan: ", lifespan_years, " years<br>",
              "Status: ", business_status, "<br>",
              "Seats: ", num_seats
            )
          )
      } else {
        # Current businesses - removed competition info
        leafletProxy("business_map", data = data_ordered) %>%
          clearMarkers() %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            radius = ~marker_size,
            fillColor = ~marker_color,
            color = "#000000",
            weight = 0.5,
            opacity = 0.8,
            fillOpacity = 0.6,
            popup = ~paste0(
              "<b>", trading_name, "</b><br>",
              business_address, "<br>",
              "Industry: ", industry, "<br>",
              "Lifespan: ", lifespan_years, " years<br>",
              "Seats: ", num_seats
            )
          )
      }
    } else {
      leafletProxy("business_map") %>% clearMarkers()
    }
  })
  
  
  # SECTION 3: STREET COMPARISON TOOL (UPDATED)
  
  
  # Selected streets data (UPDATED with new columns)
  selected_streets_data <- reactive({
    req(input$street_industry)
    
    streets <- c(input$street_1, input$street_2, input$street_3)
    streets <- streets[streets != "" & !is.na(streets)]
    
    if (length(streets) == 0) {
      return(data.frame())
    }
    
    result <- street_data_filtered() %>%
      filter(street_address %in% streets) %>%
      group_by(street_address) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      mutate(
        performance_vs_area = ifelse(
          better_than_area, 
          paste0("+", round(vs_area_benchmark, 1), " yrs"), 
          paste0(round(vs_area_benchmark, 1), " yrs")
        ),
        current_businesses_2023 = ifelse(is.na(current_businesses_2023), 0, current_businesses_2023),
        avg_seats_2023 = ifelse(is.na(avg_seats_2023), 0, avg_seats_2023),
        mean_current_comp_200m = ifelse(is.na(mean_current_comp_200m), 0, mean_current_comp_200m)
      ) %>%
      dplyr::select(
        Street = street_address,
        `Historical Businesses` = total_businesses_historical,
        `Avg Lifespan (yrs)` = avg_lifespan_historical,
        `Success Rate (>12y)` = success_rate_12yrs,
        `Current Businesses` = current_businesses_2023,
        `Avg Seats (2023)` = avg_seats_2023,
        `Competition (200m)` = mean_current_comp_200m,
        `vs Area Benchmark` = performance_vs_area,
        `CLUE Area` = clue_area
      )
    
    return(result)
  })
  
  # Street comparison table (UPDATED)
  output$street_comparison_table <- renderDT({
    data <- selected_streets_data()
    
    if (nrow(data) == 0) {
      return(datatable(
        data.frame(Message = "Please select at least one street to compare"),
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    data <- data %>%
      distinct(Street, .keep_all = TRUE)
    
    datatable(
      data,
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("Avg Lifespan (yrs)", "Success Rate (>12y)"), 1) %>%
      formatRound(c("Avg Seats (2023)", "Competition (200m)"), 0) %>%
      formatStyle(
        'Success Rate (>12y)',
        backgroundColor = styleInterval(
          c(15, 20, 25), 
          c('#ffebee', '#fff9c4', '#c8e6c9', '#81c784')
        )
      ) %>%
      formatStyle(
        'Competition (200m)',
        backgroundColor = styleInterval(
          c(30, 50, 70), 
          c('#c8e6c9', '#fff9c4', '#ffccbc', '#ffebee')
        )
      ) %>%
      formatStyle(
        'Avg Seats (2023)',
        backgroundColor = styleInterval(
          c(30, 50, 70),
          c('#e3f2fd', '#bbdefb', '#90caf9', '#64b5f6')
        )
      )
  })
  
  # Street success comparison chart
  output$street_success_comparison <- renderPlotly({
    data <- selected_streets_data()
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        geom_blank() +
        labs(title = "Select streets to compare") +
        theme_minimal()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }
    
    plot_data <- data %>%
      distinct(Street, .keep_all = TRUE) %>%
      dplyr::select(Street, `Success Rate (>12y)`) %>%
      arrange(desc(`Success Rate (>12y)`))
    
    p <- ggplot(plot_data, aes(
      x = reorder(Street, `Success Rate (>12y)`), 
      y = `Success Rate (>12y)`,
      text = paste0(
        "Street: ", Street, "<br>",
        "Success Rate: ", round(`Success Rate (>12y)`, 1), "%"
      )
    )) +
      geom_col(fill = "#667eea", alpha = 0.8) +
      geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.5) +
      coord_flip() +
      labs(x = NULL, y = "Success Rate (%)") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 11, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Street competition comparison chart (UPDATED)
  output$street_competition_comparison <- renderPlotly({
    data <- selected_streets_data()
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        geom_blank() +
        labs(title = "Select streets to compare") +
        theme_minimal()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }
    
    plot_data <- data %>%
      distinct(Street, .keep_all = TRUE) %>%
      dplyr::select(Street, `Competition (200m)`) %>%
      arrange(`Competition (200m)`)
    
    p <- ggplot(plot_data, aes(
      x = reorder(Street, `Competition (200m)`), 
      y = `Competition (200m)`,
      text = paste0(
        "Street: ", Street, "<br>",
        "TRUE 2023 Competition: ", round(`Competition (200m)`, 0)
      )
    )) +
      geom_col(fill = "#764ba2", alpha = 0.8) +
      coord_flip() +
      labs(x = NULL, y = "Current Competition (2023, 200m radius)") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 11, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Street recommendations (UPDATED)
  output$street_recommendations <- renderUI({
    data <- selected_streets_data()
    
    if (nrow(data) == 0) return(NULL)
    
    data <- data %>% distinct(Street, .keep_all = TRUE)
    
    best_street <- data %>%
      arrange(desc(`Success Rate (>12y)`)) %>%
      slice_head(n = 1)
    
    worst_street <- if (nrow(data) > 1) {
      data %>%
        arrange(`Success Rate (>12y)`) %>%
        slice_head(n = 1)
    } else {
      NULL
    }
    
    recommendation_class <- if (best_street$`Success Rate (>12y)` >= 20) {
      "success-box"
    } else if (best_street$`Success Rate (>12y)` >= 15) {
      "warning-box"
    } else {
      "info-box"
    }
    
    div(class = recommendation_class,
        h4("📊 Comparison Insights:", style = "margin-top: 0;"),
        tags$ul(style = "margin-bottom: 0;",
                tags$li(HTML(paste0(
                  "<strong>Best performer:</strong> ", best_street$Street, 
                  " (", round(best_street$`Success Rate (>12y)`, 1), "% success rate, ",
                  best_street$`Historical Businesses`, " historical businesses)"
                ))),
                if (!is.null(worst_street) && nrow(data) > 1) {
                  tags$li(HTML(paste0(
                    "<strong>Lowest performer:</strong> ", worst_street$Street, 
                    " (", round(worst_street$`Success Rate (>12y)`, 1), "% success rate)"
                  )))
                },
                tags$li(HTML(paste0(
                  "<strong>TRUE 2023 Competition:</strong> ", 
                  round(best_street$`Competition (200m)`, 0), " competitors - ",
                  ifelse(best_street$`Competition (200m)` < 40, "✅ Favorable", 
                         ifelse(best_street$`Competition (200m)` < 60, "Moderate", "⚠ High"))
                ))),
                tags$li(HTML(paste0(
                  "<strong>Average restaurant size (2023):</strong> ",
                  round(best_street$`Avg Seats (2023)`, 0), " seats"
                ))),
                tags$li(HTML(paste0(
                  "<strong>Current activity:</strong> ",
                  sum(data$`Current Businesses`, na.rm = TRUE), 
                  " active businesses across selected streets"
                ))),
                tags$li(HTML("💡 All metrics show TRUE 2023 current data (not historical entry-year data)"))
        )
    )
  })
  
  
  # SECTION 4: ANALYTICS DASHBOARDS (Sankey only)
 
  
  # Sankey diagram
  output$sankey_plot <- renderSankeyNetwork({
    if (is.null(input$analytics_industry) || length(input$analytics_industry) == 0) {
      return(NULL)
    }
    
    sankey_data <- df %>%
      filter(
        !bias_filter,
        industry %in% input$analytics_industry,
        !is.na(competitors_200m),
        competitors_200m >= input$competition_range[1],
        competitors_200m <= input$competition_range[2]
      ) %>%
      group_by(business_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      count(industry, area_group, lifespan_group) %>%
      filter(!is.na(lifespan_group))
    
    if (nrow(sankey_data) == 0) return(NULL)
    
    # Create nodes
    nodes <- data.frame(
      name = c(
        unique(sankey_data$industry),
        unique(sankey_data$area_group),
        unique(sankey_data$lifespan_group)
      )
    )
    
    # Create links
    sankey_links <- sankey_data %>%
      mutate(
        source = match(industry, nodes$name) - 1,
        intermediate = match(area_group, nodes$name) - 1,
        target_final = match(lifespan_group, nodes$name) - 1
      ) %>%
      dplyr::select(source, target = intermediate, value = n) %>%
      bind_rows(
        sankey_data %>%
          mutate(
            source = match(area_group, nodes$name) - 1,
            target = match(lifespan_group, nodes$name) - 1
          ) %>%
          dplyr::select(source, target, value = n)
      )
    
    sankeyNetwork(
      Links = sankey_links, 
      Nodes = nodes,
      Source = "source", 
      Target = "target",
      Value = "value", 
      NodeID = "name",
      fontSize = 14, 
      nodeWidth = 30
    )
  })
  
  
  # SECTION 5: DURATION PATTERNS
  
  
  duration_data <- reactive({
    if (is.null(input$duration_industry) || input$duration_industry == "") {
      return(data.frame())
    }
    
    data <- df %>%
      filter(!bias_filter) %>%
      group_by(business_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      filter(industry == input$duration_industry)
    
    if (input$duration_area != "All Areas") {
      data <- data %>% filter(clue_area == input$duration_area)
    }
    
    data
  })
  
  output$duration_stats <- renderPrint({
    data <- duration_data()
    
    if (nrow(data) == 0) {
      cat("No data available\n")
      return()
    }
    
    cat("Total:", nrow(data), "\n")
    cat("Median:", median(data$lifespan_years), "yrs\n")
    cat("Short-term (≤2):", sum(data$lifespan_years <= 2), "\n")
    cat("Success rate:", round(mean(data$lifespan_group == ">12 yrs") * 100, 1), "%\n")
  })
  
  output$duration_histogram <- renderPlotly({
    data <- duration_data()
    
    if (nrow(data) == 0) {
      p <- ggplot() + geom_blank() +
        labs(title = "Please select an industry") +
        theme_minimal()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }
    
    hist_data <- data %>%
      mutate(
        lifespan_bin = case_when(
          lifespan_years >= 20 ~ "20+",
          TRUE ~ as.character(lifespan_years)
        ),
        color_group = case_when(
          lifespan_years <= 2 ~ "Short-term (1-2 years)",
          lifespan_years <= 5 ~ "Medium-term (3-5 years)",
          TRUE ~ "Long-term (6+ years)"
        )
      ) %>%
      count(lifespan_bin, color_group) %>%
      mutate(
        lifespan_numeric = ifelse(lifespan_bin == "20+", 20, as.numeric(lifespan_bin)),
        text_label = paste0("Lifespan: ", lifespan_bin, " years<br>",
                            "Count: ", n, "<br>",
                            "Category: ", color_group)
      ) %>%
      arrange(lifespan_numeric)
    
    p <- ggplot(hist_data, aes(x = factor(lifespan_bin, levels = unique(lifespan_bin[order(lifespan_numeric)])),
                               y = n, fill = color_group, text = text_label)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = c("Short-term (1-2 years)" = "#1f77b4",
                                   "Medium-term (3-5 years)" = "#ff7f0e",
                                   "Long-term (6+ years)" = "#2ca02c")) +
      labs(x = "Lifespan (Years)", y = "Number of Businesses", fill = "Duration") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  
  # SECTION 6: GEOGRAPHIC HEATMAP
  
  
  output$success_heatmap <- renderPlotly({
    if (is.null(input$heatmap_industry) || length(input$heatmap_industry) == 0) {
      p <- ggplot() + geom_blank() +
        labs(title = "Please select industries") +
        theme_minimal()
      return(ggplotly(p) %>% config(displayModeBar = FALSE))
    }
    
    heatmap_data <- df %>%
      group_by(business_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      filter(industry %in% input$heatmap_industry) %>%
      group_by(clue_area, industry) %>%
      summarise(
        total = n(),
        survivors = sum(lifespan_years >= input$success_threshold, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(total >= 3) %>%
      mutate(
        success_rate = round((survivors / total) * 100, 1),
        text_label = paste0(clue_area, " - ", industry, "<br>",
                            "Success Rate: ", success_rate, "%<br>",
                            "Survivors: ", survivors, "/", total, "<br>",
                            "Threshold: ≥", input$success_threshold, " years")
      )
    
    if (nrow(heatmap_data) > 0) {
      p <- ggplot(heatmap_data, aes(x = industry, y = clue_area, fill = success_rate, text = text_label)) +
        geom_tile(color = "white", size = 1) +
        scale_fill_gradient2(low = "#1f77b4", mid = "#ffffcc", high = "#2ca02c",
                             midpoint = 50, name = "Success %") +
        geom_text(aes(label = paste0(success_rate, "%")), size = 3.5, fontface = "bold") +
        labs(x = "Industry", y = "Melbourne Area") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank())
      
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    }
  })
  
  
  # FOOTER
 
  
  output$session_info <- renderPrint({
    cat("Melbourne Business Longevity Explorer - Enhanced Edition\n")
    cat("========================================================\n")
    cat("R version:", R.version.string, "\n")
    cat("Date:", as.character(Sys.Date()), "\n\n")
    cat("Key Packages:\n")
    cat("- shiny:", as.character(packageVersion("shiny")), "\n")
    cat("- leaflet:", as.character(packageVersion("leaflet")), "\n")
    cat("- plotly:", as.character(packageVersion("plotly")), "\n")
    cat("- DT:", as.character(packageVersion("DT")), "\n\n")
    cat("Dataset Statistics:\n")
    cat("- Total records:", nrow(df), "\n")
    cat("- Unique businesses:", n_distinct(df$business_id), "\n")
    cat("- Businesses with TRUE 2023 competition:", sum(!is.na(df$true_current_comp_200m)), "\n")
    cat("- Streets analyzed:", nrow(street_lookup), "\n")
    cat("- Analysis period: 2002-2023 (21 years)\n\n")
    cat("Features:\n")
    cat("- TRUE 2023 competition density (not entry-year)\n")
    cat("- Street-by-street comparison tool\n")
    cat("- CBD expansion insights dashboard\n")
    cat("- Enhanced analytics with competition context\n")
  })
}



shinyApp(ui = ui, server = server)