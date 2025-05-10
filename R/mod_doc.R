#' doc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_doc_ui <- function(id) {
  ns <- NS(id)
  tagList(

    h3("Soil methods"),
    tableOutput(ns("soil_methods_table"))

  )
}

#' doc Server Functions
#'
#' @noRd
mod_doc_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$soil_methods_table <- renderTable({
      read_excel(system.file("extdata", "data_template.xlsx", package = "soildiveragro"),
                 sheet = "Soil Methods",
                 skip = 1
                 )
    })

  })
}

## To be copied in the UI
# mod_doc_ui("doc_1")

## To be copied in the server
# mod_doc_server("doc_1")
