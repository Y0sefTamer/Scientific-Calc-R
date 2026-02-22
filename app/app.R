library(shiny)
#source("R/base_code.R")
# ================= UI =================
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  #includeCSS("style.css"),
  
  div(class = "calculator",
      
      div(class = "brand", "COMPLEX ZERO"),
      div(class = "display", textOutput("display")),
      
      
      div(class="row",
          actionButton("clear", "C", class="btn"),
          actionButton("sqrt", "√", class="btn operator"),
          actionButton("square", "x²", class="btn operator"),
          actionButton("qous",")",class="btn"),
          actionButton("back", "⌫", class="btn ")
      ),
      
      
      div(class="row",
          actionButton("sin", "sin", class="btn operator"),
          actionButton("cos", "cos", class="btn operator"),
          actionButton("tan", "tan", class="btn operator"),
          actionButton("log", "log", class="btn operator"),
          actionButton("mod", "%", class="btn operator"),
      ),
      
      
      div(class="row",
          actionButton("abs", "abs", class="btn operator"),
          actionButton("fact", "!", class="btn operator"),
          actionButton("pow", "^", class="btn operator"),
          actionButton("add", "+", class="btn operator")
      ),
      
      
      div(class="row",
          actionButton("7", "7", class="btn"),
          actionButton("8", "8", class="btn"),
          actionButton("9", "9", class="btn"),
          actionButton("mult", "*", class="btn operator")
      ),
      div(class="row",
          actionButton("4", "4", class="btn"),
          actionButton("5", "5", class="btn"),
          actionButton("6", "6", class="btn"),
          actionButton("div", "/", class="btn operator")
      ),
      div(class="row",
          actionButton("1", "1", class="btn"),
          actionButton("2", "2", class="btn"),
          actionButton("3", "3", class="btn"),
          actionButton("sub", "-", class="btn operator")
      ),
      div(class="row",
          actionButton("0", "0", class="btn"),
          actionButton("dot", ".", class="btn"),
          actionButton("ln", "ln", class="btn operator"),
          actionButton("equal", "=", class="btn equal")
      )
  )
)
# ================= SERVER =================
server <- function(input, output, session) {
  
  expr <- reactiveVal("")
  
  output$display <- renderText({ expr() })
  
  # Clear
  observeEvent(input$clear, { expr("") })
  
  # Numbers
  lapply(0:9, function(i){
    observeEvent(input[[as.character(i)]], {
      expr(paste0(expr(), i))
    })
  })
  
  # back
  observeEvent(input$back,{
    current <- expr()
    if(nchar(current) > 0){
      expr(substr(current,1,nchar(current) -1))
    }
  }
  )
  
  # Dot
  observeEvent(input$dot, { expr(paste0(expr(), ".")) })
  observeEvent(input$qous, { expr(paste0(expr(), ")")) })
  
  # Operators
  observeEvent(input$add, { expr(paste0(expr(), "+")) })
  observeEvent(input$sub, { expr(paste0(expr(), "-")) })
  observeEvent(input$mult, { expr(paste0(expr(), "*")) })
  observeEvent(input$div, { expr(paste0(expr(), "/")) })
  observeEvent(input$mod, { expr(paste0(expr(), "%%")) })
  observeEvent(input$pow, { expr(paste0(expr(), "^")) })
  observeEvent(input$fact, { expr(paste0(expr(),"Fact("))})
  observeEvent(input$square, { expr(paste0(expr(),"^2"))})
  observeEvent(input$ln, { expr(paste0(expr(),"Ln("))})
  observeEvent(input$abs, { expr(paste0(expr(),"Abs("))})
  
  # Scientific Functions
  observeEvent(input$sqrt, { expr(paste0(expr(), "Sqrt(")) })
  observeEvent(input$sin,  { expr(paste0(expr(), "Sin(")) })
  observeEvent(input$cos,  { expr(paste0(expr(), "Cos(")) })
  observeEvent(input$tan,  { expr(paste0(expr(), "Tan(")) })
  observeEvent(input$log,  { expr(paste0(expr(), "Log(")) })
  
  # Equal
  observeEvent(input$equal, {
    tryCatch({
      result <- eval(parse(text = expr()))
      expr(as.character(result))
    }, error = function(e){
      expr("Error")
    })
  })
}

shinyApp(ui, server)