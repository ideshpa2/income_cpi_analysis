# app.R

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinythemes)

mhi <- read.csv("mhi.csv", stringsAsFactors = FALSE)
names(mhi) <- c("observation_date", "Income")
mhi$Year <- as.numeric(substr(mhi$observation_date, 1, 4))

cpi <- read.csv("cpi-u.csv",
                header = TRUE,
                sep = "",
                quote = "",
                stringsAsFactors = FALSE)
cpi <- cpi %>% filter(grepl("^M", period))
cpi$year <- as.numeric(cpi$year)

cpi_yearly <- cpi %>%
  group_by(year) %>%
  summarise(CPI = mean(value, na.rm = TRUE)) %>%
  rename(Year = year)

mhi_clean <- mhi %>% select(Year, Income)

data_all <- mhi_clean %>%
  inner_join(cpi_yearly, by = "Year") %>%
  arrange(Year)

# UI

ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),

  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fb;
      }
      .well {
        background-color: #ffffff !important;
        border-radius: 10px !important;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .tabbable > .nav > li > a {
        font-weight: 500;
      }
    "))
  ),

  titlePanel(
    div(
      style = "display: flex; align-items: baseline; gap: 10px;",
      span("Income, CPI, and Inflation Analysis using Linear Regression", 
           style = "font-weight: 600; font-size: 26px;"),
      span("1984–2024", 
           style = "color: #555; font-size: 14px;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Controls"),

      sliderInput(
        "year_range",
        "Select year range:",
        min = min(data_all$Year),
        max = max(data_all$Year),
        value = c(min(data_all$Year), max(data_all$Year)),
        step = 1,
        sep = ""
      ),

      checkboxInput(
        "show_conf",
        "Show Regression Confidence Interval",
        TRUE
      ),

      tags$hr(),
      h5("Index Settings"),

      selectInput(
        "base_year",
        "Base Year for Index (Income Index = 100):",
        choices = data_all$Year,
        selected = min(data_all$Year)
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Regression (Income ~ CPI)",
          plotlyOutput("reg_plot", height = "450px")
        ),
        tabPanel(
          "Indexed Comparison",
          plotlyOutput("index_plot", height = "450px")
        ),
        tabPanel(
          "Data Table",
          tableOutput("table")
        ),
        tabPanel(
          "Regression Details",
          h3("Model Summary"),
          verbatimTextOutput("model_summary"),
          tags$hr(),
          h3("Residuals vs Fitted"),
          plotlyOutput("resid_plot", height = "300px")
        ),
        tabPanel(
          "Data Interpretation",
          h3("Interpretation of Results"),
          tags$ul(
  tags$li("The regression analysis shows a clear long-term positive relationship between CPI and median household income, meaning that as inflation increases, nominal income generally increases as well."),
  
  tags$li("However, the regression residuals reveal that income does not rise at a perfectly constant rate relative to CPI. The residual vs. fitted plot shows mild curvature, indicating that the strength of the relationship changes in different decades."),
  
  tags$li("During the 2008 financial crisis, actual income drops below what the regression model predicts, producing negative residuals. This indicates that income underperformed relative to inflation during the recession."),
  
  tags$li("Between 2012 and 2018, residuals move closer to zero, reflecting a period where income recovered and grew more in line with inflation. This period shows a stronger match between the fitted regression line and actual values."),
  
  tags$li("Post-COVID years show large positive and negative residuals, showing that CPI increased rapidly while income responded unevenly. This explains why the regression line appears flatter during these recent years."),
  
  tags$li("In the indexed comparison, CPI consistently grows faster than income across most years. This means real purchasing power has increased more slowly than inflation, even though nominal income is rising."),
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Filter data by year range
  filtered <- reactive({
    data_all %>%
      filter(
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })


  # Regression model

  model_fit <- reactive({
    df <- filtered()
    req(nrow(df) > 2)
    lm(Income ~ CPI, data = df)
  })

  # Model summary
  output$model_summary <- renderPrint({
    summary(model_fit())
  })

  # Residuals vs Fitted plot
  output$resid_plot <- renderPlotly({
    fit <- model_fit()
    df_res <- data.frame(
      Fitted = fitted(fit),
      Residuals = resid(fit)
    )

    p <- ggplot(df_res, aes(x = Fitted, y = Residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      geom_point(color = "lightsteelblue") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      ) +
      labs(
        title = "Residuals vs Fitted",
        x = "Fitted Income",
        y = "Residuals"
      )

    ggplotly(p)
  })


  # Regression Plot

  output$reg_plot <- renderPlotly({
    df <- filtered()
    req(nrow(df) > 2)

    p <- ggplot(df, aes(x = CPI, y = Income)) +
      geom_point(color = "lightsteelblue", size = 2.3) +
      geom_smooth(
        method = "lm",
        se = isTRUE(input$show_conf),
        color = "lightblue4",
        fill = "lightblue",
        alpha = 0.18
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      ) +
      labs(
        title = "Regression: Income vs CPI",
        x = "Consumer Price Index (CPI)",
        y = "Median Household Income (USD)"
      )

    ggplotly(p)
  })

  # Indexed Comparison Plot
  output$index_plot <- renderPlotly({
    df <- filtered()
    req(nrow(df) > 1)

    # base-year values taken from full data, so it still works
    base_income <- data_all$Income[data_all$Year == input$base_year]
    base_cpi    <- data_all$CPI[data_all$Year == input$base_year]
    req(length(base_income) == 1, length(base_cpi) == 1)

    df_idx <- df %>%
      mutate(
        IncomeIndex = 100 * Income / base_income,
        CPIIndex    = 100 * CPI / base_cpi
      )

    p <- ggplot(df_idx, aes(x = Year)) +
      geom_line(aes(y = IncomeIndex, color = "Income Index"), linewidth = 1.2) +
      geom_line(aes(y = CPIIndex, color = "CPI Index"), linewidth = 1.2) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom"
      ) +
      labs(
        title = paste("Indexed Comparison (Base Year:", input$base_year, ")"),
        y = "Index (Base = 100)"
      ) +
      scale_color_manual(values = c("Income Index" = "lightblue4",
                                    "CPI Index"    = "lightsteelblue"))

    ggplotly(p)
  })

    output$table <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)
