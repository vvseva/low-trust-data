# ==============================================================================
# Swedish Migration Analysis Shiny App
#
# Description:
# This Shiny app visualizes and analyzes Swedish immigration and emigration
# data. It features an interactive ggplot2 line chart where users can click
# to add, and double-click to remove, custom annotated uncertainty intervals.
#
# Libraries required: shiny, ggplot2, dplyr, tidyr, bslib, ggdist, here, readr, ggrepel, shinyjs
# ==============================================================================

# -- 1. Load Necessary Libraries -----------------------------------------------
# Ensure all required packages are installed.
# you can run: install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "bslib", "ggdist", "here", "readr", "ggrepel", "shinyjs"))

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)
library(ggdist)
library(here)
library(readr)
library(ggrepel) # For better label placement
library(shinyjs)   # For clipboard functionality

# -- 2. Data Loading and Preprocessing -----------------------------------------
tryCatch({
  immigrations_raw <- read_csv("Immigrations.csv")
  emigrations_raw <- read_csv("Emigrations.csv")
  
  immigrations <- immigrations_raw |>
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Count") |>
    mutate(Type = "Immigration", Year = as.numeric(Year), Count = as.numeric(Count))
  
  emigrations <- emigrations_raw |>
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Count") |>
    mutate(Type = "Emigration", Year = as.numeric(Year), Count = as.numeric(Count))
  
  migration_data <- bind_rows(immigrations, emigrations) |>
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
  # Initialize shinyjs
  useShinyjs(),
  sidebar = sidebar(
    title = "Plot Controls",
    width = 350,
    card(
      card_header("Uncertainty Interval"),
      card_body(
        sliderInput("interval_width", "Interval Width for New Annotation:", min = 0.1, max = 0.99, value = 0.5, step = 0.05),
        p(class = "text-muted", "Click on the plot to add a custom annotated interval. This slider controls its width.")
      )
    ),
    card(
      card_header("Manage Annotations"),
      card_body(
        actionButton("clear_hunches", "Clear All My Annotations", icon = icon("trash"), class = "btn-danger w-100"),
        p(class = "text-muted mt-2", "You can also double-click an annotation on the plot to remove it individually."),
        hr(),
        actionButton("copy_code", "Copy Plot Code", icon = icon("copy"), class = "btn-primary w-100")
      )
    )
  ),
  card(
    card_header("Migration Timeline"),
    card_body(plotOutput("migration_plot", click = "plot_click", dblclick = "plot_dblclick", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")))
  ),
  card(
    # Set a minimum height for this card to prevent the plot from shifting on hover.
    style = "min-height: 290px;",
    card_header("Hover Information"),
    card_body(uiOutput("hover_info"))
  )
)

# -- 4. Server Logic -----------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Reactive Values ---
  rv <- reactiveValues(
    hunches = data.frame(Year=numeric(), Count=numeric(), Comment=character(), id=character(), IntervalWidth=numeric()),
    pending_hunch = NULL
  )
  
  # --- Event Observers ---
  observeEvent(input$plot_click, {
    click <- input$plot_click
    min_year <- min(migration_data$Year, na.rm = TRUE)
    max_year <- max(migration_data$Year, na.rm = TRUE)
    if(click$x < min_year || click$x > max_year) return()
    rv$pending_hunch <- list(Year = click$x, Count = click$y)
    showModal(modalDialog(
      title = "Add Annotation",
      textInput("hunch_comment", "Enter your comment or hunch:", placeholder = "e.g., 'Possible economic downturn'"),
      footer = tagList(modalButton("Cancel"), actionButton("submit_hunch", "Add Annotation"))
    ))
  })
  
  observeEvent(input$submit_hunch, {
    req(rv$pending_hunch, input$hunch_comment)
    new_hunch <- data.frame(
      Year = rv$pending_hunch$Year,
      Count = rv$pending_hunch$Count,
      Comment = input$hunch_comment,
      id = paste0("hunch-", as.numeric(Sys.time())),
      IntervalWidth = input$interval_width
    )
    rv$hunches <- bind_rows(rv$hunches, new_hunch)
    rv$pending_hunch <- NULL
    removeModal()
  })
  
  observeEvent(input$clear_hunches, {
    rv$hunches <- data.frame(Year=numeric(), Count=numeric(), Comment=character(), id=character(), IntervalWidth=numeric())
  })
  
  observeEvent(input$plot_dblclick, {
    if (nrow(rv$hunches) == 0) return()
    near_point_to_delete <- nearPoints(rv$hunches, input$plot_dblclick, maxpoints = 1, threshold = 10)
    if (nrow(near_point_to_delete) > 0) {
      rv$hunches <- rv$hunches |> filter(id != near_point_to_delete$id)
    }
  })
  
  # --- Code Generation and Copying ---
  observeEvent(input$copy_code, {
    # Use dput to create a reproducible version of the user's annotations
    hunches_code <- if (nrow(rv$hunches) > 0) {
      paste("user_annotations <-", paste(capture.output(dput(rv$hunches)), collapse = "\n"))
    } else {
      "user_annotations <- data.frame(Year=numeric(), Count=numeric(), Comment=character(), id=character(), IntervalWidth=numeric())"
    }
    
    # Generate only the code for the user-created hunches
    reproducible_code <- paste(
      "# --- User-Generated Annotations Code ---",
      "# This code defines a data frame with your custom annotations.",
      hunches_code,
      sep = "\n"
    )
    
    # Use shinyjs to run JavaScript that copies the text to the clipboard
    runjs(paste0("
      const el = document.createElement('textarea');
      el.value = ", jsonlite::toJSON(reproducible_code), ";
      document.body.appendChild(el);
      el.select();
      document.execCommand('copy');
      document.body.removeChild(el);
    "))
    
    # Show a notification to the user
    showNotification("User annotation code copied to clipboard!", type = "message")
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
      labs(title = "Annual Immigration vs. Emigration in Sweden", 
           subtitle = "Annotations highlight key events influencing immigration.", 
           x = NULL, y = "Number of People", color = "Flow Type") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", 
            plot.title = element_text(face = "bold", size = 18), 
            plot.subtitle = element_text(color = "grey30"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            )+
      expand_limits(y=0)
    
  })
  
  # --- Reactive Plot Object ---
  plot_to_render <- reactive({
    p <- base_plot()
    if (nrow(rv$hunches) > 0) {
      for (i in 1:nrow(rv$hunches)) {
        hunch <- rv$hunches[i, ]
        interval_data <- data.frame(Year = hunch$Year, Value = rnorm(1000, mean = hunch$Count, sd = hunch$IntervalWidth * hunch$Count)) |> 
          filter(Value >= 0)
        p <- p + stat_interval(
          data = interval_data, 
          aes(x = Year, y = Value),
          #     slab_alpha  = after_stat(level)),
          # fill_type = "gradient",
          # fill = "cyan4",
          # color = "white",
          alpha  = 0.5,
          inherit.aes = FALSE,
          show.legend = FALSE)
      }
      p <- p + scale_color_brewer(palette = "Blues")
    }
    p
  })
  
  # --- Main Plot Rendering ---
  output$migration_plot <- renderPlot({
    plot_to_render()
  }, res = 100)
  
  # --- Hover Information UI ---
  output$hover_info <- renderUI({
    req(input$plot_hover)
    near_annotation <- nearPoints(annotations_df, input$plot_hover, xvar = "year", yvar = "y_pos", threshold = 10, maxpoints = 1)
    near_hunch <- if(nrow(rv$hunches) > 0) nearPoints(rv$hunches, input$plot_hover, xvar = "Year", yvar = "Count", threshold = 10, maxpoints = 1) else data.frame()
    if (nrow(near_hunch) > 0) {
      info <- near_hunch
      div(class = "alert alert-info",
          h5("User Annotation Details"),
          p(strong("Year: "), round(info$Year, 1), br(), strong("Value: "), scales::comma(round(info$Count)), br(), strong("Comment: "), info$Comment, br(), strong("Interval Width: "), paste0(info$IntervalWidth * 100, "%")),
          p(em("Double-click this point to delete."))
      )
    } else if (nrow(near_annotation) > 0) {
      info <- near_annotation
      div(class = "alert alert-secondary",
          h5("Event Details"),
          p(strong("Event: "), info$label, br(), strong("Year: "), info$year)
      )
    } else {
      p("Hover over an annotation point to see details here.", class = "text-muted")
    }
  })
}

# -- 5. Run the Application ----------------------------------------------------
shinyApp(ui = ui, server = server)

# --- User-Generated Annotations Code ---
# This code defines a data frame with your custom annotations.
