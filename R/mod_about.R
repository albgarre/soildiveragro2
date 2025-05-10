#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/2,
      card(
        class = "border-primary",
        tags$a(
          href = "https://docs.google.com/document/d/1JQ7n-8D-NPpI-jW8znUBNIqN7W-wut0thtNoIJLIVT8/edit?usp=sharing",
          "User manual"
        ),
        tags$a(
          "Report an Issue",
          href = "mailto:alberto.garre@upct.es?subject=%5BR4EU%5D%20SoilDiverAgroAppu&body=First%20Name%3A%0D%0ALast%20Name%3A%0D%0AOrganization%3A%0D%0ACountry%3A%0D%0ASoftware Version%3A%201.0.1%0D%0AFeedback%20or%20Issue%3A"
        )
      )
    ),

    layout_column_wrap(
      width = 1,
      card_image(
        file = system.file("extdata", "UE.png", package = "soildiveragro")
      )
    )

  )
}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
