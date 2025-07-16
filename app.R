# app.R for Generic CRO Lab Shiny App (Corrected Version)

# Set the maximum upload size to 20 MB (20 * 1024 * 1024 bytes)
options(shiny.maxRequestSize = 20 * 1024^2) 

# --- 1. Load Required Packages ---
# List of required packages
#packages <- c("shiny", "tidyr", "DT", "ggplot2", "dplyr", "purrr", "scales")

# Check if packages are installed and install if not
#invisible(lapply(packages, function(pkg) {
#  if (!requireNamespace(pkg, quietly = TRUE)) {
#    install.packages(pkg, dependencies = TRUE)
#  }
#}))

# Invoke libraries
library(shiny)
library(tidyr)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)
library(scales)

# --- 2. Create Sample Data (if it doesn't exist) ---
if (!file.exists("data/boxbliss_demo.csv")) {
  dir.create("data", showWarnings = FALSE)
  sample_data <- data.frame(
    id = 1:1000,
    variant = rep(c("Control", "VariantA", "VariantC"), length.out = 1000),
    outcome = rbinom(1000, 1, 0.02), # More realistic base conversion rate
    segment = sample(c("Mobile", "Desktop"), 1000, replace = TRUE)
  )
  write.csv(sample_data, "data/boxbliss_demo.csv", row.names = FALSE)
}

# --- 3. Define UI ---
ui <- fluidPage(
  titlePanel("CRO Lab App: Bayesian A/B Testing"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload CSV", accept = ".csv"),
      hr(),
      h5("1) Column Mapping"),
      selectInput("col_id", "ID column", choices = NULL),
      selectInput("col_variant", "Variant column", choices = NULL),
      selectInput("col_control", "Control Variant", choices = NULL),
      selectInput("col_outcome", "Outcome column (0 or 1)", choices = NULL),
      selectInput("col_timestamp", "Timestamp (optional)", choices = NULL),
      hr(),
      h5("2) Segmentation"),
      selectInput("col_segment", "Segment column (optional)", choices = NULL),
      uiOutput("segment_levels_ui"),
      hr(),
      h5("3) Bayesian Priors & Stopping"),
      numericInput("prior_alpha", "Prior α (Successes)", 1, min = 0),
      numericInput("prior_beta", "Prior β (Failures)", 1, min = 0),
      numericInput("min_n", "Min n/arm", 100, min = 1),
      numericInput("max_n", "Max n/arm", 5000, min = 1),
      checkboxInput("enable_seq", "Enable sequential stopping", TRUE),
      hr(),
      actionButton("run", "Run Analysis", class = "btn-primary"),
      actionButton("reset", "Reset")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(12,
                          h4("Welcome to the CRO Lab App"),
                          HTML("
                            <ul>
                              <li><b>Upload your data:</b> Select any CSV with one row per session (visitor ID, variant label, conversion flag, etc.).</li>
                              <li><b>Map columns:</b> Use the dropdowns to specify your variant, control, outcome, ID, timestamp, and segment columns.</li>
                              <li><b>Set priors & stopping rules:</b> Adjust Bayesian priors (α/β), minimum/maximum sample size per arm, and toggle sequential stopping.</li>
                            </ul>
                          "),
                          tags$p(
                            # located inside the tabPanel("Overview", ...)
                          tagList(
                            downloadButton("download_sample_boxbliss", "Download BoxBliss Demo"),
                            downloadButton("download_sample_innovate", "Download InnovateEcho Demo")
                          ),
                            style = "margin-top:1em;"
                          ),
                          helpText("Start by uploading your CSV on the left—then follow the tabs to run and refine your CRO experiments.")
                   )
                 )
        ),
        tabPanel("Data Preview",
                 DTOutput("tbl"),
                 verbatimTextOutput("summary")
        ),
        tabPanel("Segmentation",
                 helpText("Conversion rate by arm and segment."),
                 plotOutput("seg_plot"),
                 DTOutput("seg_table")
        ),
        tabPanel("A/B Results",
                 helpText("Posterior density & 95% CI on lift (may take a few seconds to process)."),
                 plotOutput("post_plot"),
                 plotOutput("ci_plot")
        ),
        tabPanel("Sequential",
                 helpText("This tab shows the probability that each variant outperforms the control as sample size increases. A dashed line at 95% indicates the stopping threshold."),
                 conditionalPanel(
                   condition = "input.enable_seq",
                   plotOutput("seq_plot"),
                   verbatimTextOutput("stop_text")
                 )
        ),
        tabPanel("What-If Simulator",
                 helpText("This tab simulates posterior conversion rate distributions for a hypothetical sample size, based on observed rates and priors."),
                 sliderInput(
                   "hypo_n",
                   "Hypothetical n per arm",
                   min = 100, max = 20000, value = 2000, step = 100
                 ),
                 plotOutput("sim_plot"),
                 DTOutput("sim_table")
        ),
        tabPanel("Download",
                 helpText("Preview your A/B test results here and download as CSV."),
                 DTOutput("res_table"),
                 downloadButton("dl_csv", "Download Results CSV")
        )
      )
    )
  )
)

# --- 4. Define Server Logic ---
server <- function(input, output, session) {
  
  # Load & map
  raw <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  })
  
  observeEvent(raw(), {
    cols <- names(raw())
    for (id in c("col_variant", "col_outcome", "col_id", "col_timestamp", "col_segment")) {
      updateSelectInput(session, id, choices = c("", cols))
    }
  })
  
  observeEvent(input$col_variant, {
    req(raw(), input$col_variant)
    updateSelectInput(session, "col_control", choices = unique(raw()[[input$col_variant]]))
  })
  
  # Segment levels UI
  output$segment_levels_ui <- renderUI({
    req(input$col_segment)
    levs <- unique(raw()[[input$col_segment]])
    checkboxGroupInput("segment_levels", "Select Levels to Analyze", choices = levs, selected = levs, inline = TRUE)
  })
  
  # Filtered data with validation
  df <- reactive({
    req(raw(), input$col_variant, input$col_outcome, input$col_id, input$col_control)
    d <- raw()
    
    # Validate outcome column
    if (!all(d[[input$col_outcome]] %in% c(0, 1, NA))) {
      showNotification("Outcome column must contain only 0s, 1s, and NAs.", type = "error")
      return(NULL)
    }
    
    d <- d[!duplicated(d[[input$col_id]]), ]
    
    if (input$col_segment != "" && !is.null(input$segment_levels)) {
      d <- d[d[[input$col_segment]] %in% input$segment_levels, ]
    }
    
    if (!input$col_control %in% unique(d[[input$col_variant]])) {
      showNotification("Selected control variant not found in the filtered data.", type = "warning")
      return(NULL)
    }
    d
  })
  
  # Data Preview (immediately after upload)
  output$tbl <- renderDT({
    req(raw())
    head(raw(), 10)
  })
  
  output$summary <- renderPrint({
    req(raw())
    cat("Rows:", nrow(raw()), "\n", "Columns:", paste(names(raw()), collapse = ", "), "\n")
  })
  
  # ---- Segmentation logic ----
  seg_summary <- reactive({
    req(df(), input$col_variant, input$col_outcome)
    
    if (nzchar(input$col_segment)) {
      df() %>%
        filter(.data[[input$col_segment]] %in% input$segment_levels) %>%
        group_by(arm = .data[[input$col_variant]], segment = .data[[input$col_segment]]) %>%
        summarise(
          n = n(),
          conv = sum(.data[[input$col_outcome]] == 1, na.rm = TRUE),
          rate = conv / n,
          .groups = "drop"
        )
    } else {
      df() %>%
        group_by(arm = .data[[input$col_variant]]) %>%
        summarise(
          n = n(),
          conv = sum(.data[[input$col_outcome]] == 1, na.rm = TRUE),
          rate = conv / n,
          .groups = "drop"
        )
    }
  })
  
  output$seg_plot <- renderPlot({
    df_seg <- seg_summary()
    req(df_seg)
    p <- ggplot(df_seg, aes(x = arm, y = rate, fill = arm)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(title = "Conversion Rate by Variant", x = "Variant", y = "Conversion Rate") +
      theme_minimal(base_size = 14)
    
    if (nzchar(input$col_segment)) {
      p <- p + facet_wrap(~segment, scales = "free_x", drop = TRUE)
    }
    p
  })
  
  output$seg_table <- renderDT({
    req(seg_summary())
    seg_summary() %>% mutate(rate = scales::percent(rate, accuracy = 0.1))
  })
  
  # ---- A/B Results logic ----
  ab_results <- eventReactive(input$run, {
    req(df(), input$col_control)
    d <- df()
    arms <- unique(d[[input$col_variant]])
    arms <- c(input$col_control, setdiff(arms, input$col_control))
    
    base <- tibble(arm = arms) %>%
      rowwise() %>%
      mutate(
        suc = sum(d[[input$col_outcome]][d[[input$col_variant]] == arm] == 1, na.rm = TRUE),
        fail = sum(d[[input$col_outcome]][d[[input$col_variant]] == arm] == 0, na.rm = TRUE),
        a = input$prior_alpha + suc,
        b = input$prior_beta + fail,
        samples = list(rbeta(2000, a, b))
      ) %>% ungroup()
    
    ctrl_samps <- base$samples[[1]]
    
    base %>%
      mutate(
        lift_mean = ifelse(arm == input$col_control, 0, map_dbl(samples, ~ mean((.x - ctrl_samps) / ctrl_samps, na.rm=TRUE) * 100)),
        lift_low = ifelse(arm == input$col_control, 0, map_dbl(samples, ~ quantile((.x - ctrl_samps) / ctrl_samps, 0.025, na.rm=TRUE) * 100)),
        lift_high = ifelse(arm == input$col_control, 0, map_dbl(samples, ~ quantile((.x - ctrl_samps) / ctrl_samps, 0.975, na.rm=TRUE) * 100))
      )
  })
  
  output$post_plot <- renderPlot({
    res <- ab_results(); req(res)
    dfp <- res %>% select(arm, samples) %>% unnest(samples)
    ggplot(dfp, aes(x = samples, fill = arm)) + geom_density(alpha = 0.5) + labs(title = "Posterior Distributions of Conversion Rate", x = "Conversion Rate", y = "Density") + theme_minimal(base_size = 14)
  })
  
  output$ci_plot <- renderPlot({
    res <- ab_results(); req(res)
    ggplot(res, aes(x = arm, ymin = lift_low, ymax = lift_high, color = arm)) + geom_errorbar(width = 0.2, linewidth = 1.2) + labs(title = "95% Credible Interval on Lift (%) vs. Control", x = NULL, y = "Lift (%)") + theme_minimal(base_size = 14) + theme(legend.position = "none")
  })
  
  # ---- Sequential Stopping logic ----
  seq_results <- eventReactive(input$run, {
    if (!input$enable_seq) return(NULL)
    req(df(), input$col_variant, input$col_outcome, input$col_control, input$min_n, input$max_n)
    
    d <- df()
    arms <- unique(d[[input$col_variant]])
    control <- input$col_control
    
    map_dfr(
      seq(from = input$min_n, to = min(nrow(d) / length(arms), input$max_n), by = max(floor(input$min_n/2), 20)),
      function(n_per_arm) {
        sub <- d %>% group_by(.data[[input$col_variant]]) %>% slice_head(n = n_per_arm)
        
        hits_c <- sum(sub[[input$col_outcome]][sub[[input$col_variant]] == control] == 1, na.rm = TRUE)
        miss_c <- sum(sub[[input$col_outcome]][sub[[input$col_variant]] == control] == 0, na.rm = TRUE)
        post_c <- rbeta(2000, input$prior_alpha + hits_c, input$prior_beta + miss_c)
        
        tibble(
          n = n_per_arm * length(arms),
          arm = arms[arms != control],
          p_gt = map_dbl(
            arms[arms != control],
            function(a) {
              hits_a <- sum(sub[[input$col_outcome]][sub[[input$col_variant]] == a] == 1, na.rm = TRUE)
              miss_a <- sum(sub[[input$col_outcome]][sub[[input$col_variant]] == a] == 0, na.rm = TRUE)
              post_a <- rbeta(2000, input$prior_alpha + hits_a, input$prior_beta + miss_a)
              mean(post_a > post_c, na.rm = TRUE)
            }
          )
        )
      }
    )
  })
  
  output$seq_plot <- renderPlot({
    seq_df <- seq_results(); req(seq_df)
    ggplot(seq_df, aes(x = n, y = p_gt, color = arm)) + geom_line(linewidth = 1) + geom_hline(yintercept = 0.95, linetype = "dashed") + scale_y_continuous(labels = percent_format(1)) + labs(title = "Sequential Stopping Trajectory", x = "Cumulative Sample Size", y = "P(Variant > Control)") + theme_minimal(base_size = 14)
  })
  
  output$stop_text <- renderText({
    seq_df <- seq_results(); req(seq_df)
    stop_pt <- seq_df %>% filter(p_gt > 0.95) %>% slice(1)
    if (nrow(stop_pt)) { paste0("Stop at n = ", stop_pt$n, " for variant '", stop_pt$arm, "' (P > 95%)") } 
    else { "No variant has yet crossed the 95% threshold." }
  })
  
  
  # ---- What-If Simulator logic ----
  sim_results <- eventReactive(input$hypo_n, {
    req(ab_results())
    res <- ab_results()
    res %>% mutate(
      obs_rate = suc / (suc + fail),
      new_suc = round(obs_rate * input$hypo_n),
      new_fail = input$hypo_n - new_suc,
      sim_samps = map2(input$prior_alpha + new_suc, input$prior_beta + new_fail, ~rbeta(2000, .x, .y))
    )
  })
  
  output$sim_plot <- renderPlot({
    sim_df <- sim_results(); req(sim_df)
    dfp <- sim_df %>% select(arm, sim_samps) %>% unnest(sim_samps)
    ggplot(dfp, aes(x = sim_samps, fill = arm)) + geom_density(alpha = 0.5) + labs(title = paste0("Simulated Posteriors at n = ", input$hypo_n), x = "Conversion Rate", y = "Density") + theme_minimal(base_size = 14)
  })
  
  output$sim_table <- renderDT({
    sim_df <- sim_results(); req(sim_df)
    sim_df %>%
      transmute(
        Variant = arm,
        Mean = map_dbl(sim_samps, mean),
        `2.5%` = map_dbl(sim_samps, ~quantile(.x, 0.025)),
        `97.5%` = map_dbl(sim_samps, ~quantile(.x, 0.975))
      ) %>% 
      datatable(options = list(dom = 't')) %>% 
      formatPercentage(c('Mean', '2.5%', '97.5%'), 2)
  })
  
  
  # ---- Download tab logic ----
  output$res_table <- renderDT({
    res <- ab_results(); req(res)
    res %>%
      transmute(
        Variant = arm,
        Successes = suc,
        Failures = fail,
        Rate = scales::percent((a - input$prior_alpha) / (a + b - input$prior_alpha - input$prior_beta), 0.1),
        `Lift Mean` = paste0(round(lift_mean, 1), "%"),
        `Lift 2.5%` = paste0(round(lift_low, 1), "%"),
        `Lift 97.5%` = paste0(round(lift_high, 1), "%")
      )
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("CRO_results_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(ab_results() %>% select(arm, suc, fail, lift_mean, lift_low, lift_high), file, row.names = FALSE)
    }
  )
  
# Handler for the BoxBliss demo data
output$download_sample_boxbliss <- downloadHandler(
  filename = function() {
    "boxbliss_demo.csv"
  },
  content = function(destfile) {
    # Assumes the file is in a 'data' subdirectory of your app's main directory
    file.copy("data/boxbliss_demo.csv", destfile)
  }
)

# Handler for the InnovateEcho demo data
output$download_sample_innovate <- downloadHandler(
  filename = function() {
    "innovateEcho_clickstream_data.csv"
  },
  content = function(destfile) {
    # Assumes the file is in a 'data' subdirectory of your app's main directory
    file.copy("data/innovateEcho_clickstream_data.csv", destfile)
  }
)
  
  # Reset inputs
  observeEvent(input$reset, {
    session$reload()
  })
}

# --- 5. Run the Application ---
shinyApp(ui, server)
