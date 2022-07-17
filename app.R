library(shiny)
library(readxl)
library(knitr)

calculateTFP <- function(dataQ, dataK, dataL) {
  # initialize variable
  numberOfYears <- length(colnames(dataQ)) - 2
  alphaMat <- data.frame(wilayah = dataQ[[1]], eK = 0, eL = 0)
  
  # calculate b1 and b2
  for (i in 1:numberOfYears) {
    q <- (dataQ[[i+2]] - dataQ[[i+1]]) / dataQ[[i+1]]
    k <- (dataK[[i+2]] - dataK[[i+1]]) / dataK[[i+1]]
    b1 <- q / k
    alphaMat$eK <- alphaMat$eK + b1
  }
  alphaMat$eK <- alphaMat$eK / numberOfYears
  alphaMat$eL <- 1 - alphaMat$eK
  
  # calculate A matrix (the size same as Q, L, and K dataset)
  dataA <- dataQ
  for (i in 1:numberOfYears) {
    q <- (dataQ[[i+2]] - dataQ[[i+1]]) / dataQ[[i+1]]
    k <- (dataK[[i+2]] - dataK[[i+1]]) / dataK[[i+1]]
    l <- (dataL[[i+2]] - dataL[[i+1]]) / dataL[[i+1]]
    b0 <- q - alphaMat$eK * k - alphaMat$eL * l
    dataA[[i+2]] <- exp(b0)
  }
  dataA
  
  
  # calculate source matrix (TFP EG, SK, and SL)
  sourceMat <- data.frame(wilayah = dataQ[[1]], tfpg = 0, eg = 0, sk = 0, sl = 0)
  
  for (i in 1:numberOfYears) {
    q <- (dataQ[[i+2]] - dataQ[[i+1]]) / dataQ[[i+1]]
    k <- (dataK[[i+2]] - dataK[[i+1]]) / dataK[[i+1]]
    l <- (dataL[[i+2]] - dataL[[i+1]]) / dataL[[i+1]]
    tfp <- (dataA[[i+2]] - dataA[[i+1]]) /  dataA[[i+1]]
    
    sourceMat$eg <- sourceMat$eg + q
    sourceMat$tfpg <- sourceMat$tfpg + tfp
    sourceMat$sk <- sourceMat$sk + (alphaMat$eK * k)
    sourceMat$sl <- sourceMat$sl + (alphaMat$eL * l)
  }
  
  sourceMat[, -1] <- lapply(sourceMat[, -1], function(x) x * 100 / numberOfYears)
  
  
  # output to R shiny (alphaMat and source Mat)
  return(list(
    alphaMat = alphaMat,
    sourceMat = sourceMat
  ))
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        max-width: none !important;
        overflow-x: hidden;
      }
      .tab-content {
        overflow: scroll
      }
      "
    ))
  ),
  titlePanel(strong("Penghitungan Sumber Pertumbuhan Output")),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload File Excel Here', accept = c(".xlsx")),
      br(),
      em("Note :"),
      strong("File excel terdiri dari 3 sheet dan secara berurutan berisi data Q(output), data K(Kapital), dan data L(Labor) untuk setiap sheet")),
    mainPanel(
      # fileInput("upload", NULL, accept = c(".csv", ".tsv", ".xls", ".xlsx")),
      tabsetPanel(
        tabPanel("Contoh", 
                 img(src = "contoh.png"), 
                 p(em(strong("contoh file yang di upload")))
                 ),
        tabPanel("Formula", withMathJax(), 
                 HTML(markdown::markdownToHTML(knit("formula.Rmd", quiet = T)))),
        navbarMenu("Data",
          tabPanel("DataQ", tableOutput('contentsQ')),
          tabPanel("DataK", tableOutput('contentsK')),
          tabPanel("DataL", tableOutput('contentsL'))
          ),
        tabPanel("elastisitas",tableOutput('alphaMat'),
                 p(em(strong("eK = elastisitas modal (Capital's Share)"))),
                 p(em(strong("eK = elastisitas tenaga kerja (Labor's Share)")))),
        tabPanel("sumber pertumbuhan ekonomi (%)",tableOutput('sourceMat'),
                 p(em(strong("eg = economic growth (Pertumbuhan ekonomi)"))),
                 p(em(strong("tfpg = TFP Growth (Pertumbuhn TFP)"))),
                 p(em(strong("sk = Contribution of Capital (kontribusi modal)"))),
                 p(em(strong("sl = Contribution of Labor (kontribusi tenaga kerja)"))))
      ))
      )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    
    inFile <- input$file1
    
    dataQ <- read_excel(inFile$datapath, sheet = 1)
    dataK <- read_excel(inFile$datapath, sheet = 2)
    dataL <- read_excel(inFile$datapath, sheet = 3)
    
    
    result <- calculateTFP(dataQ, dataK, dataL)
    result
  })
  
  output$contentsQ <- renderTable({
    req(input$file1)
    
    inFile <- input$file1
    tryCatch({
      df <- read_excel(inFile$datapath, sheet = 1)
      return(df)
    })
    
  })
  
  output$contentsK <- renderTable({
    req(input$file1)
    
    inFile <- input$file1
    tryCatch({
      df <- read_excel(inFile$datapath, sheet = 2)
      return(df)
    })
    
  })
  
  output$contentsL <- renderTable({
    req(input$file1)
    
    inFile <- input$file1
    tryCatch({
      df <- read_excel(inFile$datapath, sheet = 3)
      return(df)
    })
    
  })
  
  output$alphaMat <- renderTable({
    dataset <- data()
    dataset$alphaMat
    
  })
  
  output$sourceMat <- renderTable({
    dataset <- data()
    dataset$sourceMat
  })
  
  
  
}

shinyApp(ui, server)