library(shiny)
library(ggplot2)
library(dplyr)
library(xtable)
library(shinycssloaders)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #f0f2f5;
      }
      
      .container {
        background-color: white;
        max-width: 800px;
        margin: 30px auto;
        padding: 30px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }
      
      .numeric-input-container {
        display: flex;
        justify-content: center;
        margin-bottom: 20px;
      }
      
      .numeric-input {
        margin: 0 5px;
      }
      
      .main-plot-container {
        margin: 20px 0;
      }
      
      .matrix-container {
        margin: 20px 0;
        font-size: 16px;
        color: #333;
      }
      
      .shiny-options-group label {
        font-weight: normal;
      }
      
      .shiny-options-group input[type='number'] {
        border-color: #ced4da;
        border-radius: 4px;
      }
      
      h1 {
        font-size: 28px;
        font-weight: 500;
        margin-bottom: 30px;
        text-align: center;
      }
      
      h2 {
        font-size: 18px;
        font-weight: 500;
        margin-bottom: 20px;
        text-align: center;
      }
    "))),
  div(class = "container",
      h1("Interactive Heatmap"),
      h2("Select the number of rows and columns"),
      div(class = "numeric-input-container",
          column(6, numericInput("input_m", "Number of rows (m):", value = 5, min = 1), class = "numeric-input"),
          column(6, numericInput("input_n", "Number of columns (n):", value = 5, min = 1), class = "numeric-input")
      ),
      div(class = "main-plot-container",
          withSpinner(uiOutput("mainplot"))
      ),
      div(class = "matrix-container",
          withSpinner(uiOutput("matrix"))
      )
  )
)

server <- function(input, output, session) {
  
  mat <- reactive({
    matrix(0, nrow = input$input_m, ncol = input$input_n)
  })
  
  updated_mat <- reactiveVal()
  
  observe({
    updated_mat(mat())
  })
  
  output$plot_render <- renderPlot({
    plot_data <- expand.grid(x = 1:input$input_m, y = 1:input$input_n) %>%
      mutate(value = c(updated_mat()))
    
    p <- ggplot(data = plot_data,
                aes(x = x, y = y, fill = value)) +
      geom_tile(color = "black") +
      theme_minimal() +
      theme(panel.grid = element_blank(),axis.title  = element_blank())+
      scale_fill_gradient2(low = "white", mid = "blue", high = "red", midpoint = max(updated_mat())/2)
    
    p
  })
  
  output$mainplot <- renderUI({
    plotOutput("plot_render", click = "plot_click")
  })
  
  observeEvent(input$plot_click, {
    req(input$plot_click)
    click <- input$plot_click
    row <- ceiling(click$y - 0.5)
    col <- ceiling(click$x - 0.5)
    new_mat <- updated_mat()
    new_mat[col, row] <- new_mat[col, row] + 1
    updated_mat(new_mat)
  })
  
  output$matrix <- renderUI({
    M <- updated_mat()
    rownames(M) <- seq(1,nrow(M),1)
    M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    html <- paste0("$$", M, "$$")
    list(
      withMathJax(HTML(html))
    )
  })
}

shinyApp(ui, server)
