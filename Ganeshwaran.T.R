# ==========================================================
# WORLD HAPPINESS REPORT - SHINY DASHBOARD
# ==========================================================

# ==========================================================
# PACKAGES
# ==========================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)


# ==========================================================
# ZIP FILE
# ==========================================================

zip_file <- "C:/Users/Administrator/Downloads/archive (6).zip"

if (!file.exists(zip_file)) {
  stop(
    paste0(
      "ZIP FILE NOT FOUND:\n",
      zip_file
    )
  )
}


# ==========================================================
# FIND CSV INSIDE ZIP
# ==========================================================

zip_contents <- unzip(
  zip_file,
  list = TRUE
)

csv_files <- zip_contents$Name[
  grepl(
    "\\.csv$",
    zip_contents$Name,
    ignore.case = TRUE
  )
]

if (length(csv_files) == 0) {
  stop("No CSV file was found inside the ZIP file.")
}


# Prefer a World Happiness CSV
happiness_files <- csv_files[
  grepl(
    "happiness|world",
    basename(csv_files),
    ignore.case = TRUE
  )
]

if (length(happiness_files) > 0) {
  csv_inside_zip <- happiness_files[1]
} else {
  csv_inside_zip <- csv_files[1]
}


cat("\nCSV selected from ZIP:\n")
cat(csv_inside_zip, "\n")


# ==========================================================
# EXTRACT CSV
# ==========================================================

temp_folder <- tempfile(
  "happiness_"
)

dir.create(
  temp_folder,
  recursive = TRUE
)

unzip(
  zip_file,
  files = csv_inside_zip,
  exdir = temp_folder
)

csv_file <- file.path(
  temp_folder,
  csv_inside_zip
)

if (!file.exists(csv_file)) {
  stop(
    paste0(
      "CSV extraction failed:\n",
      csv_file
    )
  )
}


# ==========================================================
# READ CSV
# ==========================================================

data <- tryCatch(
  
  read.csv(
    csv_file,
    stringsAsFactors = FALSE,
    check.names = TRUE,
    fileEncoding = "UTF-8-BOM"
  ),
  
  error = function(e) {
    
    read.csv(
      csv_file,
      stringsAsFactors = FALSE,
      check.names = TRUE,
      fileEncoding = "Windows-1252"
    )
  }
)


# ==========================================================
# COLUMN NAMES
# ==========================================================

names(data) <- make.names(
  names(data),
  unique = TRUE
)

cat("\nColumns found:\n")
print(names(data))


# ==========================================================
# REQUIRED COLUMNS
# ==========================================================

required_columns <- c(
  "Country.name",
  "year",
  "Life.Ladder",
  "Log.GDP.per.capita",
  "Social.support",
  "Healthy.life.expectancy.at.birth",
  "Freedom.to.make.life.choices"
)

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  
  stop(
    paste0(
      "\nMissing columns:\n",
      paste(
        missing_columns,
        collapse = "\n"
      ),
      "\n\nAvailable columns:\n",
      paste(
        names(data),
        collapse = "\n"
      )
    )
  )
}


# ==========================================================
# NUMERIC CONVERSION
# ==========================================================

numeric_columns <- c(
  "year",
  "Life.Ladder",
  "Log.GDP.per.capita",
  "Social.support",
  "Healthy.life.expectancy.at.birth",
  "Freedom.to.make.life.choices"
)

for (col in numeric_columns) {
  
  data[[col]] <- suppressWarnings(
    as.numeric(
      gsub(
        ",",
        "",
        as.character(data[[col]])
      )
    )
  )
}


# ==========================================================
# CLEAN DATA
# ==========================================================

data <- data %>%
  filter(
    !is.na(Country.name),
    Country.name != "",
    !is.na(year),
    !is.na(Life.Ladder)
  )


# ==========================================================
# USER INTERFACE
# ==========================================================

ui <- dashboardPage(
  
  # --------------------------------------------------------
  # HEADER
  # --------------------------------------------------------
  
  dashboardHeader(
    title = "World Happiness Report"
  ),
  
  
  # --------------------------------------------------------
  # SIDEBAR
  # --------------------------------------------------------
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Overview",
        tabName = "overview"
      ),
      
      menuItem(
        "Country Analysis",
        tabName = "country"
      ),
      
      menuItem(
        "Relationships",
        tabName = "relationships"
      ),
      
      menuItem(
        "Trends",
        tabName = "trends"
      ),
      
      menuItem(
        "Data Explorer",
        tabName = "data"
      )
    ),
    
    hr(),
    
    h4(
      "Filters",
      style = "padding-left:15px;"
    ),
    
    selectInput(
      "year_filter",
      "Select Year:",
      choices = sort(
        unique(data$year)
      ),
      selected = max(
        data$year,
        na.rm = TRUE
      )
    ),
    
    selectInput(
      "country_filter",
      "Select Countries:",
      choices = sort(
        unique(data$Country.name)
      ),
      multiple = TRUE
    ),
    
    br(),
    
    actionButton(
      "reset_filters",
      "Reset Filters",
      class = "btn-primary"
    )
  ),
  
  
  # --------------------------------------------------------
  # BODY
  # --------------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      
      tags$style(
        HTML(
          "
          .content-wrapper,
          .right-side {
            background-color: #f5f6fa;
          }

          .small-box {
            border-radius: 10px;
          }

          .box {
            border-radius: 10px;
          }

          .main-header .logo {
            font-weight: bold;
          }
          "
        )
      )
    ),
    
    
    tabItems(
      
      # ====================================================
      # OVERVIEW
      # ====================================================
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          
          valueBoxOutput(
            "avg_happiness",
            width = 3
          ),
          
          valueBoxOutput(
            "top_country",
            width = 3
          ),
          
          valueBoxOutput(
            "avg_gdp",
            width = 3
          ),
          
          valueBoxOutput(
            "country_count",
            width = 3
          )
        ),
        
        
        fluidRow(
          
          box(
            title = "Top 20 Happiest Countries",
            status = "primary",
            solidHeader = TRUE,
            width = 7,
            
            plotlyOutput(
              "ranking_plot",
              height = "500px"
            )
          ),
          
          box(
            title = "Happiness Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 5,
            
            plotlyOutput(
              "distribution_plot",
              height = "500px"
            )
          )
        )
      ),
      
      
      # ====================================================
      # COUNTRY ANALYSIS
      # ====================================================
      
      tabItem(
        tabName = "country",
        
        fluidRow(
          
          box(
            title = "Country Happiness Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput(
              "country_plot",
              height = "550px"
            )
          )
        ),
        
        fluidRow(
          
          box(
            title = "GDP per Capita",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "gdp_plot",
              height = "400px"
            )
          ),
          
          box(
            title = "Social Support",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "social_plot",
              height = "400px"
            )
          )
        )
      ),
      
      
      # ====================================================
      # RELATIONSHIPS
      # ====================================================
      
      tabItem(
        tabName = "relationships",
        
        fluidRow(
          
          box(
            title = "Happiness vs Healthy Life Expectancy",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "health_plot",
              height = "450px"
            )
          ),
          
          box(
            title = "Happiness vs Freedom",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "freedom_plot",
              height = "450px"
            )
          )
        )
      ),
      
      
      # ====================================================
      # TRENDS
      # ====================================================
      
      tabItem(
        tabName = "trends",
        
        fluidRow(
          
          box(
            title = "Happiness Trends Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput(
              "trend_plot",
              height = "550px"
            )
          )
        )
      ),
      
      
      # ====================================================
      # DATA EXPLORER
      # ====================================================
      
      tabItem(
        tabName = "data",
        
        fluidRow(
          
          box(
            title = "World Happiness Dataset",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            downloadButton(
              "download_data",
              "Download Filtered Data"
            ),
            
            br(),
            br(),
            
            DTOutput(
              "data_table"
            )
          )
        )
      )
    )
  )
)


# ==========================================================
# SERVER
# ==========================================================

server <- function(input, output, session) {
  
  
  # ========================================================
  # FILTERED DATA
  # ========================================================
  
  filtered_data <- reactive({
    
    d <- data
    
    
    if (
      !is.null(input$year_filter) &&
      input$year_filter != ""
    ) {
      
      d <- d %>%
        filter(
          year == as.numeric(
            input$year_filter
          )
        )
    }
    
    
    if (
      !is.null(input$country_filter) &&
      length(input$country_filter) > 0
    ) {
      
      d <- d %>%
        filter(
          Country.name %in%
            input$country_filter
        )
    }
    
    
    d
  })
  
  
  # ========================================================
  # RESET
  # ========================================================
  
  observeEvent(
    input$reset_filters,
    {
      
      updateSelectInput(
        session,
        "year_filter",
        selected = max(
          data$year,
          na.rm = TRUE
        )
      )
      
      updateSelectInput(
        session,
        "country_filter",
        selected = character(0)
      )
    }
  )
  
  
  # ========================================================
  # KPI - HAPPINESS
  # ========================================================
  
  output$avg_happiness <- renderValueBox({
    
    d <- filtered_data()
    
    value <- mean(
      d$Life.Ladder,
      na.rm = TRUE
    )
    
    valueBox(
      value = round(
        value,
        2
      ),
      subtitle = "Average Happiness",
      color = "aqua"
    )
  })
  
  
  # ========================================================
  # KPI - TOP COUNTRY
  # ========================================================
  
  output$top_country <- renderValueBox({
    
    d <- filtered_data() %>%
      filter(
        !is.na(Life.Ladder)
      ) %>%
      arrange(
        desc(Life.Ladder)
      )
    
    
    if (nrow(d) == 0) {
      country <- "No data"
    } else {
      country <- d$Country.name[1]
    }
    
    
    valueBox(
      value = country,
      subtitle = "Highest Happiness",
      color = "green"
    )
  })
  
  
  # ========================================================
  # KPI - GDP
  # ========================================================
  
  output$avg_gdp <- renderValueBox({
    
    d <- filtered_data()
    
    value <- mean(
      d$Log.GDP.per.capita,
      na.rm = TRUE
    )
    
    
    valueBox(
      value = round(
        value,
        2
      ),
      subtitle = "Average Log GDP per Capita",
      color = "yellow"
    )
  })
  
  
  # ========================================================
  # KPI - COUNTRY COUNT
  # ========================================================
  
  output$country_count <- renderValueBox({
    
    d <- filtered_data()
    
    value <- n_distinct(
      d$Country.name
    )
    
    
    valueBox(
      value = value,
      subtitle = "Countries",
      color = "purple"
    )
  })
  
  
  # ========================================================
  # RANKING
  # ========================================================
  
  output$ranking_plot <- renderPlotly({
    
    d <- filtered_data() %>%
      arrange(
        desc(Life.Ladder)
      ) %>%
      slice_head(
        n = 20
      ) %>%
      mutate(
        Country.name = reorder(
          Country.name,
          Life.Ladder
        )
      )
    
    
    p <- ggplot(
      d,
      aes(
        x = Life.Ladder,
        y = Country.name,
        text = paste0(
          "Country: ",
          Country.name,
          "<br>Happiness: ",
          round(
            Life.Ladder,
            2
          )
        )
      )
    ) +
      geom_col() +
      labs(
        x = "Happiness Score",
        y = NULL
      ) +
      theme_minimal()
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ========================================================
  # DISTRIBUTION
  # ========================================================
  
  output$distribution_plot <- renderPlotly({
    
    d <- filtered_data()
    
    
    p <- ggplot(
      d,
      aes(
        x = Life.Ladder
      )
    ) +
      geom_histogram(
        bins = 25
      ) +
      labs(
        x = "Happiness Score",
        y = "Number of Countries"
      ) +
      theme_minimal()
    
    
    ggplotly(p)
  })
  
  
  # ========================================================
  # COUNTRY COMPARISON
  # ========================================================
  
  output$country_plot <- renderPlotly({
    
    d <- filtered_data()
    
    
    p <- ggplot(
      d,
      aes(
        x = reorder(
          Country.name,
          Life.Ladder
        ),
        y = Life.Ladder,
        text = paste0(
          "Country: ",
          Country.name,
          "<br>Happiness: ",
          round(
            Life.Ladder,
            2
          )
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        x = NULL,
        y = "Happiness Score"
      ) +
      theme_minimal()
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ========================================================
  # GDP
  # ========================================================
  
  output$gdp_plot <- renderPlotly({
    
    d <- filtered_data()
    
    
    p <- ggplot(
      d,
      aes(
        x = Log.GDP.per.capita,
        y = Life.Ladder,
        text = Country.name
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      labs(
        x = "Log GDP per Capita",
        y = "Happiness Score"
      ) +
      theme_minimal()
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ========================================================
  # SOCIAL SUPPORT
  # ========================================================
  
  output$social_plot <- renderPlotly({
    
    d <- filtered_data()
    
    
    p <- ggplot(
      d,
      aes(
        x = Social.support,
        y = Life.Ladder,
        text = Country.name
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      labs(
        x = "Social Support",
        y = "Happiness Score"
      ) +
      theme_minimal()
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ========================================================
  # HEALTH
  # ========================================================
  
  output$health_plot <- renderPlotly({
    
    d <- filtered_data()
    
    
    p <- ggplot(
      d,
      aes(
        x = Healthy.life.expectancy.at.birth,
        y = Life.Ladder,
        text = Country.name
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      labs(
        x = "Healthy Life Expectancy",
        y = "Happiness Score"
      ) +
      theme_minimal()
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ========================================================
  # FREEDOM
  # ========================================================
  
  output$freedom_plot <- renderPlotly({
    
    d <- filtered_data()
    
    
    p <- ggplot(
      d,
      aes(
        x = Freedom.to.make.life.choices,
        y = Life.Ladder,
        text = Country.name
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      labs(
        x = "Freedom to Make Life Choices",
        y = "Happiness Score"
      ) +
      theme_minimal()
    
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  
  # ========================================================
  # TREND
  # ========================================================
  
  output$trend_plot <- renderPlotly({
    
    d <- data
    
    
    if (
      !is.null(input$country_filter) &&
      length(input$country_filter) > 0
    ) {
      
      d <- d %>%
        filter(
          Country.name %in%
            input$country_filter
        )
    }
    
    
    trend_data <- d %>%
      group_by(year) %>%
      summarise(
        Happiness = mean(
          Life.Ladder,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    
    p <- ggplot(
      trend_data,
      aes(
        x = year,
        y = Happiness
      )
    ) +
      geom_line(
        linewidth = 1.2
      ) +
      geom_point(
        size = 2
      ) +
      labs(
        x = "Year",
        y = "Average Happiness"
      ) +
      theme_minimal()
    
    
    ggplotly(p)
  })
  
  
  # ========================================================
  # DATA TABLE
  # ========================================================
  
  output$data_table <- renderDT({
    
    datatable(
      filtered_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  
  # ========================================================
  # DOWNLOAD
  # ========================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "world_happiness_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}


# ==========================================================
# RUN APP
# ==========================================================

shinyApp(
  ui = ui,
  server = server
)