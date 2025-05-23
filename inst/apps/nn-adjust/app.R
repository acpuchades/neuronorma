library(dplyr)
library(haven)
library(magrittr)
library(readr)
library(readxl)
library(shiny)
library(shinyjs)
library(tools)
library(writexl)

library(neuronorma)

is_multi_data_file <- function(path) {
  tools::file_ext(path) %in% c("xls", "xlsx")
}

input_file_types <- c(
  ".csv", ".tsv", ".sav", ".xls", ".xlsx"
)

output_file_types <- c(
  "Excel (.xlsx)" = "xlsx",
  "CSV file (.csv)" = "csv",
  "TSV file (.tsv)" = "tsv",
  "SPSS (.sav)" = "spss"
)

file_format_exts <- list(
  "csv" = ".csv",
  "tsv" = ".tsv",
  "spss" = ".sav",
  "xlsx" = ".xlsx"
)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("NEURONORMA Batch Adjust"),
  sidebarLayout(
    sidebarPanel(
      div(
        fileInput("input_file", label = "Input file", accept = input_file_types),
        hidden(selectInput("sheet_name", label = "Sheet name", choices = NULL)),
      ),
      hidden(
        div(
          id = "params",
          textInput("output_suffix", label = "Output suffix", value = "_adjusted"),
          varSelectInput("age_column", label = "Age column", data = NULL),
          varSelectInput("education_column", label = "Education column", data = NULL),
          varSelectInput("raw_tmt_a_columns", label = "Raw TMTa score columns", data = NULL, multiple = TRUE),
          varSelectInput("raw_tmt_b_columns", label = "Raw TMTb score columns", data = NULL, multiple = TRUE),
          varSelectInput("raw_rocf_acc_columns", label = "Raw ROCF score columns", data = NULL, multiple = TRUE),
          varSelectInput("raw_rocf_dr_acc_columns", label = "Raw ROCF DR score columns", data = NULL, multiple = TRUE),
          varSelectInput("raw_hvlt_a1_columns", label = "Raw HVLT A1 score columns", data = NULL, multiple = TRUE),
          varSelectInput("raw_hvlt_tr_columns", label = "Raw HVLT TR score columns", data = NULL, multiple = TRUE),
          varSelectInput("raw_hvlt_a4_columns", label = "Raw HVLT A4 score columns", data = NULL, multiple = TRUE),
          varSelectInput("raw_hvlt_di_columns", label = "Raw HVLT DI score columns", data = NULL, multiple = TRUE),
          selectInput("output_format", label = "Output format", choices = output_file_types),
          downloadButton("download", "Download data"),
        )
      ),
    ),
    mainPanel(
      tableOutput("contents"),
    ),
  ),
)

app <- function(input, output, session) {
  observeEvent(input$input_file, {
    path <- req(input$input_file)$datapath
    if (is_multi_data_file(path)) {
      datasheets <- excel_sheets(path)
      updateSelectInput(session, "sheet_name", choices = datasheets)
      show("sheet_name")
    } else {
      updateSelectInput(session, "sheet_name", choices = NULL)
      hide("sheet_name")
    }
    show("params")
  })

  input_data <- reactive({
    path <- req(input$input_file)$datapath
    switch(file_ext(path),
      "csv" = readr::read_csv(path),
      "tsv" = readr::read_tsv(path),
      "sav" = haven::read_spss(path),
      "xls" = readxl::read_excel(path, sheet = req(input$sheet_name)),
      "xlsx" = readxl::read_excel(path, sheet = req(input$sheet_name)),
    )
  })

  observe({
    updateVarSelectInput(session, "age_column", data = input_data())
    updateVarSelectInput(session, "education_column", data = input_data())
    updateVarSelectInput(session, "raw_tmt_a_columns", data = input_data())
    updateVarSelectInput(session, "raw_tmt_b_columns", data = input_data())
    updateVarSelectInput(session, "raw_rocf_acc_columns", data = input_data())
    updateVarSelectInput(session, "raw_rocf_dr_acc_columns", data = input_data())
    updateVarSelectInput(session, "raw_hvlt_a1_columns", data = input_data())
    updateVarSelectInput(session, "raw_hvlt_tr_columns", data = input_data())
    updateVarSelectInput(session, "raw_hvlt_a4_columns", data = input_data())
    updateVarSelectInput(session, "raw_hvlt_di_columns", data = input_data())
  })

  output_data <- reactive({
    age_column <- req(input$age_column)
    education_column <- req(input$education_column)
    output_name <- paste0("{.col}", req(input$output_suffix))
    input_data() %>%
      mutate(
        across(c(!!!input$raw_tmt_a_columns), adjust_TMTa,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        ),
        across(c(!!!input$raw_tmt_b_columns), adjust_TMTb,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        ),
        across(c(!!!input$raw_rocf_acc_columns), adjust_ROCF_Acc,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        ),
        across(c(!!!input$raw_rocf_dr_acc_columns), adjust_ROCF_DR_Acc,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        ),
        across(c(!!!input$raw_hvlt_a1_columns), adjust_HVLT_A1,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        ),
        across(c(!!!input$raw_hvlt_tr_columns), adjust_HVLT_TR,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        ),
        across(c(!!!input$raw_hvlt_a4_columns), adjust_HVLT_A4,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        ),
        across(c(!!!input$raw_hvlt_di_columns), adjust_HVLT_DI,
          age = !!age_column,
          education = !!education_column,
          .names = !!output_name
        )
      )
  })

  output$contents <- renderTable(output_data())
  output$download <- downloadHandler(
    filename = function() {
      name <- file_path_sans_ext(input$input_file$name)
      ext <- file_format_exts[[input$output_format]]
      paste0(name, "_adj", ext)
    },
    content = function(path) {
      switch(input$output_format,
        "csv" = readr::write_csv(output_data(), path),
        "tsv" = readr::write_tsv(output_data(), path),
        "spss" = haven::write_sav(output_data(), path),
        "xlsx" = writexl::write_xlsx(output_data(), path),
      )
    }
  )
}

shinyApp(ui, app)
