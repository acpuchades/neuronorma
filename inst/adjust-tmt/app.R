library(dplyr)
library(magrittr)
library(readr)
library(readxl)
library(shiny)
library(shinyjs)
library(tools)
library(writexl)

library(neuronorma)

fileTypes <- c(".csv", ".tsv", ".xls", ".xlsx")

ui <- fluidPage(
  useShinyjs(),

  titlePanel("NEURONORMA Adjust TMT"),

  sidebarLayout(
    sidebarPanel(
      div(
        h4("Data source"),
        fileInput("inputFile", label = "Input file", accept=fileTypes),
        selectInput("sheetName", label = "Sheet name", choices = NULL),
      ),

      div(
        h4("Input"),
        varSelectInput("ageColumn", label = "Age column", data = NULL),
        varSelectInput("studiesColumn", label = "Studies column", data = NULL),
        varSelectInput("rawTMTaColumn", label = "Raw TMTa score column", data = NULL),
        varSelectInput("rawTMTbColumn", label = "Raw TMTb score column", data = NULL),

        h4("Output"),
        textInput("adjTMTaColumn", label = "TMTa output column"),
        textInput("adjTMTbColumn", label = "TMTb output column"),
      ),
    ),

    mainPanel(
      tableOutput("contents"),
      downloadLink("download", "Download data"),
    ),
  ),
)

app <- function(input, output, session) {
  observeEvent(input$inputFile, {
    req(input$inputFile)
    path <- input$inputFile$datapath
    ext <- file_ext(path)
    if (ext %in% c("xls", "xlsx")) {
      updateSelectInput(session, "sheetName", choices = excel_sheets(path))
    } else {
      updateSelectInput(session, "sheetName", choices = NULL)
    }
  })

  inputData <- reactive({
    path <- req(input$inputFile)$datapath
    ext <- file_ext(path)
    if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(path, sheet = req(input$sheetName))
    } else if (ext == "csv") {
      readr::read_csv(path)
    } else if (ext == "tsv") {
      readr::read_tsv(path)
    }
  })

  observe({
    colnames <- names(inputData())
    iparams <- c("ageColumn", "studiesColumn", "rawTMTaColumn", "rawTMTbColumn")
    for (i in seq_along(iparams)) {
      defvalue <- ifelse(i <= length(colnames), colnames[i], NULL)
      updateVarSelectInput(session, iparams[i], data = inputData(), selected = defvalue)
    }
  })

  observeEvent(input$rawTMTaColumn, {
    defvalue <- paste0(input$rawTMTaColumn, "_Adj")
    updateTextInput(session, "adjTMTaColumn", value = defvalue)
  })

  observeEvent(input$rawTMTbColumn, {
    defvalue <- paste0(input$rawTMTbColumn, "_Adj")
    updateTextInput(session, "adjTMTbColumn", value = defvalue)
  })

  outputData <- reactive({
    ageColumn <- req(input$ageColumn)
    studiesColumn <- req(input$studiesColumn)
    rawTMTaColumn <- req(input$rawTMTaColumn)
    rawTMTbColumn <- req(input$rawTMTbColumn)
    adjTMTaColumn <- req(input$adjTMTaColumn)
    adjTMTbColumn <- req(input$adjTMTbColumn)

    inputData() %>%
      mutate({{ adjTMTaColumn }} := adjust_TMTa(raw = !!sym(rawTMTaColumn),
                                                age = !!sym(ageColumn),
                                                studies = !!sym(studiesColumn)),
             {{ adjTMTbColumn }} := adjust_TMTb(raw = !!sym(rawTMTbColumn),
                                                age = !!sym(ageColumn),
                                                studies = !!sym(studiesColumn)))
  })

  output$contents <- renderTable(outputData())
  output$download <- downloadHandler(
    filename = function() {
      name <- file_path_sans_ext(input$inputFile$name)
      ext <- file_ext(input$inputFile$name)
      paste0(name, "_adj.", ext)
    },
    content = function(path) {
      ext <- file_ext(path)
      if (ext %in% c("xls", "xlsx")) {
        write_xlsx(outputData(), path)
      } else if (ext == "csv") {
        write_csv(outputData(), path)
      } else if (ext == "tsv") {
        write_tsv(outputData(), path)
      }
    }
  )
}

shinyApp(ui, app)
