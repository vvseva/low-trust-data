# ==============================================================================
# Swedish Migration Analysis Shiny App
#
# Description:
# This Shiny app visualizes and analyzes Swedish immigration and emigration
# data. It features an interactive ggplot2 line chart where users can click
# to add a custom annotated uncertainty interval.
#
# Author: Gemini
# Date: 2024-07-15
#
# Libraries required: shiny, ggplot2, dplyr, tidyr, bslib, ggdist, here, readr, ggrepel
# ==============================================================================

# -- 1. Load Necessary Libraries -----------------------------------------------
# Ensure all required packages are installed.
# you can run: install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "bslib", "ggdist", "here", "readr", "ggrepel"))

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)
library(ggdist)
library(here)
library(readr)
library(ggrepel) # For better label placement

# -- 2. Data Loading and Preprocessing -----------------------------------------
tryCatch({
  immigrations_raw <- read_csv(here::here("Immigrations.csv"))
  emigrations_raw <- read_csv(here::here("Emigrations.csv"))

  immigrations <- immigrations_raw %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Count") %>%
    mutate(Type = "Immigration", Year = as.numeric(Year), Count = as.numeric(Count))

  emigrations <- emigrations_raw %>%
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Count") %>%
    mutate(Type = "Emigration", Year = as.numeric(Year), Count = as.numeric(Count))

  migration_data <- bind_rows(immigrations, emigrations) %>%
    filter(!is.na(Year), !is.na(Count))

}, error = function(e) {
  stop(paste("Error reading or processing CSV files:", e$message,
             "\nPlease ensure 'Immigrations.csv' and 'Emigrations.csv' are in the project directory and in the correct format."))
})

# --- Annotation Data ---
annotations_df <- data.frame(
  year = c(1999, 2006, 2015, 2022),
  label = c("Kosovo War", "Iraqi Refugee\nInflux", "Syrian Refugee\nCrisis", "Ukraine War"),
  y_pos = c(85000, 105000, 170000, 150000),
  type = "Immigration"
)

# -- 3. User Interface (UI) ----------------------------------------------------
ui <- page_sidebar(
  title = "Swedish Migration Trends (1997-2024)",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  sidebar = sidebar(
    title = "Plot Controls",
    width = 350,
    card(
      card_header("Uncertainty Interval"),
      card_body(
        sliderInput("interval_width", "Interval Width (.width):", min = 0.1, max = 0.99, value = 0.9, step = 0.05),
        p(class = "text-muted", "Click on the plot to add a custom annotated interval. This slider controls its width.")
      )
    )
  ),
  card(
    card_header("Migration Timeline"),
    card_body(plotOutput("migration_plot", click = "plot_click", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")))
  ),
  card(
    card_header("Hover Information"),
    card_body(uiOutput("hover_info"))
  )
)

# -- 4. Server Logic -----------------------------------------------------------
server <- function(input, output, session) {

  # --- Reactive Values ---
  # Stores user-added hunches and temporary click information.
  rv <- reactiveValues(
    hunches = data.frame(Year=numeric(), Count=numeric(), Type=character(), Comment=character(), id=character()),
    pending_hunch = NULL
  )

  # --- Event Observers ---
  # Clear hunches if the interval width is changed.
  observeEvent(input$interval_width, {
    rv$hunches <- data.frame(Year=numeric(), Count=numeric(), Type=character(), Comment=character(), id=character())
  })

  # Triggered when user clicks the plot.
  observeEvent(input$plot_click, {
    click <- input$plot_click
    
    # Create interpolation functions for both lines
    imm_data <- migration_data %>% filter(Type == "Immigration")
    em_data <- migration_data %>% filter(Type == "Emigration")
    
    imm_fun <- approxfun(imm_data$Year, imm_data$Count)
    em_fun <- approxfun(em_data$Year, em_data$Count)
    
    # Interpolate Y values at the clicked X coordinate
    y_imm <- imm_fun(click$x)
    y_em <- em_fun(click$x)
    
    # Proceed only if the click is within the data range
    if(is.na(y_imm) || is.na(y_em)) return()

    # Determine which line was closer to the click's Y coordinate
    if (abs(click$y - y_imm) < abs(click$y - y_em)) {
      hunch_type <- "Immigration"
      hunch_count <- y_imm
    } else {
      hunch_type <- "Emigration"
      hunch_count <- y_em
    }
    
    # Store the pending hunch data
    rv$pending_hunch <- list(Year = click$x, Count = hunch_count, Type = hunch_type)
    
    # Show a modal dialog to ask for a comment
    showModal(modalDialog(
      title = "Add Annotation",
      textInput("hunch_comment", "Enter your comment or hunch:", placeholder = "e.g., 'Possible economic downturn'"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_hunch", "Add Annotation")
      )
    ))
  })
  
  # Triggered when user submits the comment in the modal.
  observeEvent(input$submit_hunch, {
    req(rv$pending_hunch, input$hunch_comment)
    
    new_hunch <- data.frame(
      Year = rv$pending_hunch$Year,
      Count = rv$pending_hunch$Count,
      Type = rv$pending_hunch$Type,
      Comment = input$hunch_comment,
      id = paste0("hunch-", Sys.time()) # Unique ID
    )
    
    rv$hunches <- bind_rows(rv$hunches, new_hunch)
    rv$pending_hunch <- NULL # Clear pending hunch
    removeModal()
  })

  # --- Base Plot ---
  base_plot <- reactive({
    ggplot(migration_data, aes(x = Year, y = Count, color = Type)) +
      geom_line(linewidth = 1.2) +
      geom_vline(data = annotations_df, aes(xintercept = year), linetype = "dashed", color = "grey40") +
      geom_text(data = annotations_df, aes(x = year, y = y_pos, label = label), hjust = -0.05, vjust = 0, color = "grey20", size = 3.5, inherit.aes = FALSE) +
      scale_color_manual(values = c("Immigration" = "#0072B2", "Emigration" = "#D55E00")) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      labs(title = "Annual Immigration vs. Emigration in Sweden", subtitle = "Annotations highlight key events influencing immigration.", x = "Year", y = "Number of People", color = "Flow Type") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 18), plot.subtitle = element_text(color = "grey30"))
  })

  # --- Main Plot Rendering ---
  output$migration_plot <- renderPlot({
    p <- base_plot()

    # If there are user-added hunches, add them to the plot.
    if (nrow(rv$hunches) > 0) {
      user_hunches <- rv$hunches
      
      # Generate distribution data for each hunch
      interval_data <- user_hunches %>%
        rowwise() %>%
        reframe(
          Year = Year,
          Value = rnorm(1000, mean = Count, sd = Count * 0.1),
          Comment = Comment,
          Type = Type
        )
      
      p <- p +
        # Add the gradient interval layer for each hunch
        stat_gradientinterval(data = interval_data, aes(x = Year, y = Value, fill = after_stat(level), group = Comment), .width = input$interval_width, inherit.aes = FALSE, show.legend = FALSE) +
        # Add a point to highlight the hunch location
        geom_point(data = user_hunches, aes(x = Year, y = Count), color = "black", size = 4, shape = 21, fill = "white", stroke = 1.5, inherit.aes = FALSE) +
        # Add the user's comment as a label
        geom_text_repel(data = user_hunches, aes(x = Year, y = Count, label = Comment), box.padding = 1, point.padding = 0.5, segment.color = 'grey50', inherit.aes = FALSE) +
        scale_fill_brewer(palette = "Blues")
    }

    p
  }, res = 100)

  # --- Hover Information UI ---
  output$hover_info <- renderUI({
    req(input$plot_hover)
    
    # Check for proximity to both pre-defined annotations and user hunches
    near_annotation <- nearPoints(annotations_df, input$plot_hover, xvar = "year", yvar = "y_pos", threshold = 10, maxpoints = 1)
    near_hunch <- if(nrow(rv$hunches) > 0) nearPoints(rv$hunches, input$plot_hover, xvar = "Year", yvar = "Count", threshold = 10, maxpoints = 1) else data.frame()
    
    if (nrow(near_hunch) > 0) {
      info <- near_hunch
      div(class = "alert alert-info",
          h5("User Annotation Details"),
          p(strong("Type: "), info$Type, br(),
            strong("Year: "), round(info$Year, 1), br(),
            strong("Value (Interpolated): "), scales::comma(round(info$Count)), br(),
            strong("Comment: "), info$Comment
          )
      )
    } else if (nrow(near_annotation) > 0) {
      info <- near_annotation
      div(class = "alert alert-secondary",
          h5("Event Details"),
          p(strong("Event: "), info$label, br(),
            strong("Year: "), info$year
          )
      )
    } else {
      p("Hover over an annotation point to see details here.", class = "text-muted")
    }
  })
}

# -- 5. Run the Application ----------------------------------------------------
shinyApp(ui = ui, server = server)
