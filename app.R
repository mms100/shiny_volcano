
library(shiny)
library(EnhancedVolcano)
library(DT)
library(plotly)

ui <- fluidPage(
  titlePanel("Volcano Plot Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV/TSV Table", accept = c(".csv", ".tsv", ".txt")),
      uiOutput("col_gene"),
      uiOutput("col_pval"),
      uiOutput("col_logfc"),
      uiOutput("gene_selector"),
      numericInput("pval", "P-value/FDR cutoff", value = 0.05),
      numericInput("logfc", "Log Fold Change cutoff", value = 1),
      textInput("xlim", "X-axis limits (comma-separated, e.g. -2,2)", ""),
      textInput("ylim", "Y-axis limits (comma-separated, e.g. 0,10)", ""),
      selectInput("highlight_option", "Highlight genes:",
                  choices = c("Top 20 significant genes" = "top20",
                              "Selected genes" = "selected",
                              "Both" = "both")),
      downloadButton("downloadPlot", "Download PDF")
    ),
    mainPanel(
      h3("Static Volcano Plot"),
      plotOutput("volcanoPlot", height = "600px"),
      h3("Interactive Volcano Plot"),
      plotlyOutput("interactiveVolcano", height = "600px"),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive: read uploaded data
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else {
      read.table(input$file$datapath, sep = ",", header = TRUE)
    }
  })
  
  # Column selectors
  output$col_gene <- renderUI({
    req(data())
    selectInput("col_gene", "Select Gene Column", choices = names(data()))
  })
  
  output$col_pval <- renderUI({
    req(data())
    selectInput("col_pval", "Select P-value Column", choices = names(data()))
  })
  
  output$col_logfc <- renderUI({
    req(data())
    selectInput("col_logfc", "Select logFC Column", choices = names(data()))
  })
  
  # Gene selector
  output$gene_selector <- renderUI({
    req(data(), input$col_gene)
    selectizeInput("genes", "Genes to label (search and select):",
                   choices = unique(as.character(data()[[input$col_gene]])),
                   multiple = TRUE)
  })
  
  # Determine genes to highlight
  genes_to_label_final <- reactive({
    req(data(), input$col_gene, input$col_pval, input$col_logfc)
    df <- data()
    genes_to_label <- input$genes
    pval_cutoff <- input$pval
    logfc_cutoff <- input$logfc
    highlight_option <- input$highlight_option
    
    sig <- df[!is.na(df[[input$col_pval]]) & !is.na(df[[input$col_logfc]]) &
                df[[input$col_pval]] < pval_cutoff & abs(df[[input$col_logfc]]) > logfc_cutoff, ]
    sig <- sig[order(sig[[input$col_pval]], -abs(sig[[input$col_logfc]])), ]
    top_genes <- head(as.character(sig[[input$col_gene]]), 20)
    
    if (highlight_option == "top20") {
      top_genes
    } else if (highlight_option == "selected") {
      genes_to_label
    } else {
      unique(c(genes_to_label, top_genes))
    }
  })
  
  # Static Volcano Plot
  output$volcanoPlot <- renderPlot({
    req(data(), input$col_gene, input$col_pval, input$col_logfc)
    df <- data()
    xlim <- if (nzchar(input$xlim)) as.numeric(strsplit(input$xlim, ",")[[1]]) else NULL
    ylim <- if (nzchar(input$ylim)) as.numeric(strsplit(input$ylim, ",")[[1]]) else NULL
    
    args <- list(
      df,
      lab = df[[input$col_gene]],
      x = input$col_logfc,
      y = input$col_pval,
      selectLab = genes_to_label_final(),
      pCutoff = input$pval,
      FCcutoff = input$logfc,
      pointSize = 2.5,
      labSize = 5,
      drawConnectors = TRUE, arrowheads =F,
      max.overlaps = Inf
    )
    if (!is.null(xlim)) args$xlim <- xlim
    if (!is.null(ylim)) args$ylim <- ylim
    
    do.call(EnhancedVolcano, args)
  })
  
  # Download Plot
  output$downloadPlot <- downloadHandler(
    filename = function() { "volcano_plot.pdf" },
    content = function(file) {
      df <- data()
      xlim <- if (nzchar(input$xlim)) as.numeric(strsplit(input$xlim, ",")[[1]]) else NULL
      ylim <- if (nzchar(input$ylim)) as.numeric(strsplit(input$ylim, ",")[[1]]) else NULL
      
      args <- list(
        df,
        lab = df[[input$col_gene]],
        x = input$col_logfc,
        y = input$col_pval,
        selectLab = genes_to_label_final(),
        pCutoff = input$pval,
        FCcutoff = input$logfc,
        pointSize = 2.5,
        labSize = 5,
        drawConnectors = TRUE,arrowheads =F,
        max.overlaps = Inf
      )
      if (!is.null(xlim)) args$xlim <- xlim
      if (!is.null(ylim)) args$ylim <- ylim
      
     grDevices::pdf(file, width = 6, height = 6)
     on.exit(grDevices::dev.off())
     volcano_plot <- do.call(EnhancedVolcano, args)
     print(volcano_plot)
    }
  )
  
  # Interactive Volcano Plot
  output$interactiveVolcano <- renderPlotly({
    req(data(), input$col_gene, input$col_pval, input$col_logfc)
    df <- data()
    df$neglog10p <- -log10(df[[input$col_pval]])
    df$color <- "NS"
    df$color[df[[input$col_pval]] < input$pval & abs(df[[input$col_logfc]]) > input$logfc] <- "Significant"
    
    plot_ly(
      data = df,
      x = ~df[[input$col_logfc]],
      y = ~df$neglog10p,
      type = "scatter",
      mode = "markers",
      color = ~df$color,
      text = ~df[[input$col_gene]],
      marker = list(size = 8)
    ) %>%
      layout(
        title = "Interactive Volcano Plot",
        xaxis = list(title = input$col_logfc),
        yaxis = list(title = paste0("-log10(", input$col_pval, ")"))
      )
  })
  
  # Show data table
  output$table <- renderDT({
    req(data())
    datatable(data())
  })
}

shinyApp(ui, server)
