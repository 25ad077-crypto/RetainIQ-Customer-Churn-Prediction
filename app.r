
# ==========================================
# RetainIQ - Customer Churn Dashboard
# ==========================================

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

# Load generated reports
if (!file.exists("output/retention_report.csv")) {
  stop("Run 01_data_analysis.R, 02_churn_model.R, and 03_retention_engine.R first.")
}

customers <- read.csv(
  "output/retention_report.csv",
  stringsAsFactors = FALSE
)

model_results <- read.csv(
  "output/model_comparison.csv",
  stringsAsFactors = FALSE
)

# Dashboard UI
ui <- fluidPage(
  
  titlePanel("RetainIQ | Customer Retention Intelligence"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Customer Filters"),
      
      selectInput(
        "risk",
        "Select Risk Level",
        choices = c("All", "High", "Medium", "Low"),
        selected = "All"
      ),
      
      numericInput(
        "top_n",
        "Customers to Display",
        value = 25,
        min = 5,
        max = 100,
        step = 5
      ),
      
      textInput(
        "search",
        "Search Customer ID",
        value = ""
      ),
      
      downloadButton(
        "download",
        "Download Customer Report"
      ),
      
      br(), br(),
      
      downloadButton(
        "download_actions",
        "Download Retention Actions"
      )
    ),
    
    mainPanel(
      
      h2("Business Overview"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h4("Customers Analyzed"),
            h3(textOutput("total_customers"))
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("High-Risk Customers"),
            h3(textOutput("high_risk"))
          )
        ),
        
        column(
          4,
          wellPanel(
            h4("Monthly Revenue Exposure"),
            h3(textOutput("revenue_risk"))
          )
        )
      ),
      
      h3("Customer Risk Distribution"),
      plotlyOutput("risk_plot"),
      
      h3("Model Performance Comparison"),
      plotlyOutput("model_plot"),
      
      h3("Prioritized Retention List"),
      DTOutput("customer_table")
    )
  )
)

# Dashboard server
server <- function(input, output, session) {
  
  filtered_customers <- reactive({
    
    result <- customers
    
    if (input$risk != "All") {
      result <- result %>%
        filter(Risk == input$risk)
    }
    
    if (nzchar(trimws(input$search))) {
      result <- result[
        grepl(
          input$search,
          result$Customer,
          ignore.case = TRUE
        ),
        ,
        drop = FALSE
      ]
    }
    
    result %>%
      arrange(desc(Estimated_Monthly_Revenue_At_Risk))
  })
  
  output$total_customers <- renderText({
    nrow(filtered_customers())
  })
  
  output$high_risk <- renderText({
    sum(customers$Risk == "High")
  })
  
  output$revenue_risk <- renderText({
    paste0(
      "$",
      format(
        round(sum(
          customers$Estimated_Monthly_Revenue_At_Risk,
          na.rm = TRUE
        ), 2),
        big.mark = ",",
        nsmall = 2
      )
    )
  })
  
  output$risk_plot <- renderPlotly({
    
    risk_data <- customers %>%
      count(Risk, name = "Customers")
    
    p <- ggplot(
      risk_data,
      aes(
        x = Risk,
        y = Customers,
        fill = Risk,
        text = paste(
          "Risk:", Risk,
          "<br>Customers:", Customers
        )
      )
    ) +
      geom_col() +
      labs(
        title = "Customers by Risk Category",
        x = "Risk Level",
        y = "Number of Customers"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$model_plot <- renderPlotly({
    
    results_long <- data.frame(
      Model = rep(
        model_results$Model,
        times = 3
      ),
      Metric = rep(
        c("ROC-AUC", "PR-AUC", "F1"),
        each = nrow(model_results)
      ),
      Score = c(
        model_results$ROC_AUC,
        model_results$PR_AUC,
        model_results$F1
      )
    )
    
    p <- ggplot(
      results_long,
      aes(
        x = Model,
        y = Score,
        fill = Metric
      )
    ) +
      geom_col(position = "dodge") +
      labs(
        title = "Model Evaluation",
        x = "Model",
        y = "Score"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$customer_table <- renderDT({
    
    result <- filtered_customers()
    
    result <- head(result, input$top_n)
    
    display_columns <- c(
      "Customer",
      "Churn_Probability",
      "Risk",
      "MonthlyCharges",
      "Estimated_Monthly_Revenue_At_Risk",
      "Priority",
      "Recommended_Action"
    )
    
    result <- result[
      ,
      intersect(display_columns, names(result)),
      drop = FALSE
    ]
    
    datatable(
      result,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0("RetainIQ_Report_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(
        filtered_customers(),
        file,
        row.names = FALSE
      )
    }
  )
  
  output$download_actions <- downloadHandler(
    
    filename = function() {
      paste0("RetainIQ_Retention_Actions_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      
      actions <- filtered_customers() %>%
        filter(Risk != "Low") %>%
        head(100)
      
      write.csv(
        actions,
        file,
        row.names = FALSE
      )
    }
  )
}

# Launch application
shinyApp(ui = ui, server = server)
