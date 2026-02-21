library(shiny)

ui <- fluidPage(
  titlePanel("🧮 Logic Calculator"),
  
  fluidRow(
    column(12,
           textOutput("display"),
           br()
    )
  ),
  
  fluidRow(
    column(3, actionButton("one", "1")),
    column(3, actionButton("two", "2")),
    column(3, actionButton("three", "3")),
    column(3, actionButton("add", "+"))
  ),
  
  fluidRow(
    column(3, actionButton("four", "4")),
    column(3, actionButton("five", "5")),
    column(3, actionButton("six", "6")),
    column(3, actionButton("sub", "-"))
  ),
  
  fluidRow(
    column(3, actionButton("seven", "7")),
    column(3, actionButton("eight", "8")),
    column(3, actionButton("nine", "9")),
    column(3, actionButton("mul", "*"))
  ),
  
  fluidRow(
    column(3, actionButton("zero", "0")),
    column(3, actionButton("equal", "=")),
    column(3, actionButton("clear", "C")),
    column(3, actionButton("div", "/"))
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(
    display = "",
    num1 = NULL,
    operation = NULL
  )
  
  # تحديث الشاشة
  updateDisplay <- function(value){
    values$display <- paste0(values$display, value)
  }
  
  # أزرار الأرقام
  observeEvent(input$one,   updateDisplay("1"))
  observeEvent(input$two,   updateDisplay("2"))
  observeEvent(input$three, updateDisplay("3"))
  observeEvent(input$four,  updateDisplay("4"))
  observeEvent(input$five,  updateDisplay("5"))
  observeEvent(input$six,   updateDisplay("6"))
  observeEvent(input$seven, updateDisplay("7"))
  observeEvent(input$eight, updateDisplay("8"))
  observeEvent(input$nine,  updateDisplay("9"))
  observeEvent(input$zero,  updateDisplay("0"))
  
  # العمليات
  observeEvent(input$add, {
    values$num1 <- as.numeric(values$display)
    values$operation <- "+"
    values$display <- ""
  })
  
  observeEvent(input$sub, {
    values$num1 <- as.numeric(values$display)
    values$operation <- "-"
    values$display <- ""
  })
  
  observeEvent(input$mul, {
    values$num1 <- as.numeric(values$display)
    values$operation <- "*"
    values$display <- ""
  })
  
  observeEvent(input$div, {
    values$num1 <- as.numeric(values$display)
    values$operation <- "/"
    values$display <- ""
  })
  
  # زر =
  observeEvent(input$equal, {
    
    num2 <- as.numeric(values$display)
    
    result <- switch(values$operation,
                     "+" = values$num1 + num2,
                     "-" = values$num1 - num2,
                     "*" = values$num1 * num2,
                     "/" = if(num2 != 0) values$num1 / num2 else "Error")
    
    values$display <- as.character(result)
    values$num1 <- NULL
    values$operation <- NULL
  })
  
  # زر مسح
  observeEvent(input$clear, {
    values$display <- ""
    values$num1 <- NULL
    values$operation <- NULL
  })
  
  output$display <- renderText({
    paste("Display:", values$display)
  })
}

shinyApp(ui, server)