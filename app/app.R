library(shiny)
library(bslib)
library(DT)
library(gt)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggdist)

# Define UI
ui <- page_sidebar(
  title = "Low Trust Data Analysis Explorer",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#04daab",
    secondary = "#95a5a6",
    success = "#2ecc71",
    info = "#17a2b8",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),
  
  # Sidebar with controls
  sidebar = sidebar(
    width = 300,
    h4("Data Manipulation Controls", class = "text-primary"),
    p("Adjust these parameters to see how data uncertainty affects analysis:", 
      class = "text-muted small"),
    
    sliderInput("missing_pct",
                "Missing Data (%)",
                min = 0, max = 30, value = 5, step = 1,
                post = "%"),
    
    sliderInput("bootstrap_pct",
                "Bootstrapped Data (%)",
                min = 0, max = 50, value = 10, step = 1,
                post = "%"),
    
    sliderInput("noise_level",
                "Noise Level",
                min = 0, max = 2, value = 0.1, step = 0.1),
    
    sliderInput("outlier_pct",
                "Outlier Injection (%)",
                min = 0, max = 10, value = 2, step = 1,
                post = "%"),
    
    sliderInput("refresh_rate",
                "Refresh Rate (seconds)",
                min = 1, max = 10, value = 3, step = 1,
                post = "s"),
    
    hr(),
    div(
      class = "alert alert-info",
      style = "font-size: 0.85em;",
      HTML("<strong>💡 Purpose:</strong> This app simulates data quality issues to help you understand how data uncertainty affects your analysis conclusions.")
    )
  ),
  
  # Main panel with tabs
  navset_card_tab(
    nav_panel("Raw Data",
              card(
                card_header("Current Dataset"),
                DT::dataTableOutput("raw_table")
              )
    ),
    
    nav_panel("Summary",
              card(
                card_header("Statistical Summary"),
                gt_output("summary_table")
              )
    ),
    
    nav_panel("Visualizations",
              layout_columns(
                card(
                  card_header("Speed vs Distance"),
                  plotOutput("scatter_plot", height = "300px")
                ),
                card(
                  card_header("Distribution Comparison"),
                  plotOutput("dist_plot", height = "300px")
                ),
                col_widths = c(6, 6)
              ),
              card(
                card_header("Correlation Heatmap"),
                plotOutput("correlation_plot", height = "400px")
              )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Original data
  original_data <- cars
  
  # Reactive data that updates based on timer and inputs
  modified_data <- reactive({
    # Auto-refresh based on refresh rate
    invalidateLater(input$refresh_rate * 1000)
    
    data <- original_data
    n <- nrow(data)
    
    # Add missing data
    if (input$missing_pct > 0) {
      missing_indices <- sample(1:(n * ncol(data)), 
                                size = round((input$missing_pct/100) * n * ncol(data)))
      data_matrix <- as.matrix(data)
      data_matrix[missing_indices] <- NA
      data <- as.data.frame(data_matrix)
    }
    
    # Add noise
    if (input$noise_level > 0) {
      numeric_cols <- sapply(data, is.numeric)
      data[numeric_cols] <- lapply(data[numeric_cols], function(x) {
        noise <- rnorm(length(x), 0, input$noise_level * sd(x, na.rm = TRUE))
        x + noise
      })
    }
    
    # Bootstrap some data points
    if (input$bootstrap_pct > 0) {
      bootstrap_n <- round((input$bootstrap_pct/100) * n)
      if (bootstrap_n > 0) {
        bootstrap_indices <- sample(1:n, bootstrap_n, replace = TRUE)
        replace_indices <- sample(1:n, bootstrap_n)
        data[replace_indices, ] <- data[bootstrap_indices, ]
      }
    }
    
    # Add outliers
    if (input$outlier_pct > 0) {
      outlier_n <- round((input$outlier_pct/100) * n)
      if (outlier_n > 0) {
        outlier_indices <- sample(1:n, outlier_n)
        # Make outliers by multiplying by random factor
        numeric_cols <- sapply(data, is.numeric)
        for (col in names(data)[numeric_cols]) {
          outlier_factor <- runif(outlier_n, 2, 4) * sample(c(-1, 1), outlier_n, replace = TRUE)
          data[outlier_indices, col] <- data[outlier_indices, col] * outlier_factor
        }
      }
    }
    
    return(data)
  })
  
  # Initialize the raw data table on first render
  output$raw_table <- DT::renderDataTable({
    DT::datatable(
      original_data,
      options = list(
        pageLength = 999,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(columns = c('speed', 'dist'), digits = 2)
  })
  
  # Create proxy for the data table
  proxy_table <- dataTableProxy('raw_table')
  
  # Update table data without resetting position/paging
  observe({
    new_data <- modified_data()
    # Format the data for display (round numeric columns)
    display_data <- new_data
    display_data[c('speed', 'dist')] <- lapply(display_data[c('speed', 'dist')], 
                                               function(x) round(x, 2))
    
    replaceData(proxy_table, display_data, resetPaging = FALSE)
  })
  
  # Summary table using gt
  output$summary_table <- render_gt({
    data <- modified_data()
    
    # Calculate summary statistics
    summary_stats <- data %>%
      summarise(
        across(where(is.numeric), 
               list(
                 Mean = ~round(mean(.x, na.rm = TRUE), 2),
                 Median = ~round(median(.x, na.rm = TRUE), 2),
                 SD = ~round(sd(.x, na.rm = TRUE), 2),
                 Min = ~round(min(.x, na.rm = TRUE), 2),
                 Max = ~round(max(.x, na.rm = TRUE), 2),
                 Missing = ~sum(is.na(.x))
               ),
               .names = "{.col}_{.fn}")
      ) %>%
      pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>%
      separate(Statistic, into = c("Variable", "Metric"), sep = "_") %>%
      pivot_wider(names_from = Metric, values_from = Value)
    
    summary_stats %>%
      gt() %>%
      tab_header(
        title = "Dataset Summary Statistics",
        subtitle = paste("Based on current data modifications (n =", nrow(data), ")")
      ) %>%
      fmt_number(
        columns = c(Mean, Median, SD, Min, Max),
        decimals = 2
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#f8f9fa"),
          cell_text(weight = "bold")
        ),
        locations = cells_column_labels()
      ) %>%
      tab_options(
        table.font.size = 16,
        heading.background.color = "#3498db",
        heading.title.font.size = 24
      )
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    data <- modified_data()
    
    ggplot(data, aes(x = speed, y = dist)) +
      geom_point(alpha = 0.7, color = "#3498db", size = 2) +
      geom_smooth(method = "lm", se = TRUE, color = "#e74c3c", alpha = 0.3) +
      labs(
        title = "Speed vs Stopping Distance",
        x = "Speed (mph)",
        y = "Stopping Distance (ft)"
      ) +
      xlim(0, 30)+
      ylim(0, 200)+
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray90", fill = NA)
      )
  })
  
  # Distribution plots
  output$dist_plot <- renderPlot({
    current_data <- modified_data()
    
    # Combine original and current data for comparison
    comparison_data <- bind_rows(
      original_data %>% mutate(Type = "Original"),
      current_data %>% mutate(Type = "Modified")
    ) %>%
      pivot_longer(cols = c(speed, dist), names_to = "Variable", values_to = "Value") %>%
      filter(!is.na(Value))  # Remove missing values for visualization
    
    ggplot(comparison_data, aes(x = Value, fill = Type)) +
      stat_halfeye(
        alpha = 0.7,
        adjust = 1.2,
        width = 0.6,
        position = position_dodge(width = 0.8)
      ) +
      facet_wrap(~Variable, scales = "free", 
                 labeller = labeller(Variable = c("speed" = "Speed (mph)", 
                                                  "dist" = "Distance (ft)"))) +
      scale_fill_manual(values = c("Original" = "#2ecc71", "Modified" = "#e74c3c")) +
      labs(
        title = "Distribution Comparison",
        x = "Value",
        y = "Density",
        fill = "Dataset"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold")
      )+
      ylim(-1, 1)
  })
  
  # Correlation heatmap
  output$correlation_plot <- renderPlot({
    data <- modified_data()
    
    # Calculate correlation with original data
    original_cor <- cor(original_data, use = "complete.obs")[1,2]
    current_cor <- cor(data, use = "complete.obs")[1,2]
    
    # Create comparison data
    cor_data <- data.frame(
      Dataset = c("Original", "Modified"),
      Correlation = c(original_cor, current_cor),
      stringsAsFactors = FALSE
    )
    
    ggplot(cor_data, aes(x = Dataset, y = Correlation, fill = Dataset)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_text(aes(label = round(Correlation, 3)), 
                vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Original" = "#2ecc71", "Modified" = "#e74c3c")) +
      ylim(0, 1) +
      labs(
        title = "Speed-Distance Correlation Comparison",
        subtitle = "How data modifications affect correlation strength",
        y = "Correlation Coefficient"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold")
      )
  })
}


shinyApp(
  ui = ui,
  server = server
)
# Run the application