#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme layout_column_wrap page_fluid page_sidebar sidebar page_navbar navset_card_pill
#'
#' @noRd
app_ui <- function(request) {
  tagList(

    golem_add_external_resources(),

    page_sidebar(
      window_title  = "soildiveragro",
      theme = bs_theme(bootswatch = "simplex",
                       primary = "#004b23",
                       secondary = "#91a773",
                       warning = "#744c28"
      ),
      footer = tagList(
        "asfasfas"
      ),
      sidebar = sidebar(
        div(
          class = "nav flex-column nav-pills",
          id = "mynav-tab",
          role = "tablist",
          `aria-orientation` = "vertical",
          tags$a(class = "nav-link active",
                 id = "input-tab",
                 `data-toggle` = "pill",
                 href = "#input-tab-content",
                 role = "tab",
                 `aria-controls` = "input-tab-content",
                 `aria-selected`= TRUE,
                 "1: Data upload"
          ),
          tags$a(class = "nav-link",
                 id = "check-tab",
                 `data-toggle` = "pill",
                 href = "#check-tab-content",
                 role = "tab",
                 `aria-controls` = "check-tab-content",
                 `aria-selected` = FALSE,
                 "2: Check upload"
          ),
          tags$a(class = "nav-link",
                 id = "out-tab",
                 `data-toggle` = "pill",
                 href = "#out-tab-content",
                 role = "tab",
                 `aria-controls` = "out-tab-content",
                 `aria-selected` = FALSE,
                 "3: Predictions"
          ),
          tags$a(class = "nav-link",
                 id = "about-tab",
                 `data-toggle` = "pill",
                 href = "#about-tab-content",
                 role = "tab",
                 `aria-controls` = "about-tab-content",
                 `aria-selected` = FALSE,
                 "About"
          )
        )),
      title = tagList(
        card_image(
          file = system.file("extdata", "banner.jpg", package = "soildiveragro")
        )
      ),
      div(  # Data upload ------------------------------------------------------
        class = "tab-content",
        id = "mynav-tab-content",
        div(class = "tab-pane fade show active",
            id = "input-tab-content",
            role = "tabpanel",
            `aria-labelledby` = "input-tab",
            card(
              card_footer(
                p(
                  paste("Please upload the soil data using the Excel template.",
                        "It can be downloaded using this link:")
                ),
                downloadButton("downloadData", "Download template")
              ),
              card_footer(
                layout_column_wrap(
                  width = 1,
                  p(
                    paste("Once you have filled it in, please uploaded it using the 'Browse' button and click the 'Load' button.")
                  ),
                  fileInput("excel_file", "Excel file"),
                )
                # layout_column_wrap(
                #   width = 1,
                #   actionButton("load_data", "Load",
                #                outline = TRUE, flat = FALSE,
                #                status = "primary"
                #   )
                # )
              )
            )
            # card(
            #   actionButton('nexttab', 'Next Tab')
            # )
        ),
        div(class = "tab-pane fade show",  # Data check ------------------------
            id = "check-tab-content",
            role = "tabpanel",
            `aria-labelledby` = "check-tab",
            layout_column_wrap(
              width = 1,
              navset_card_pill(
                full_screen = TRUE,
                # title = h3("Input data"),
                # header = card_body(
                #   p("Please use this information to check the data was uploaded correctly.")
                # ),
                nav_panel(
                  title = "Metadata",
                  tableOutput("summary_inputs")
                ),
                nav_panel(
                  title = "Physicochemical",
                  plotOutput("plot_physico") %>% withSpinner()
                ),
                nav_panel(
                  title = "Climate",
                  plotOutput("plot_climate") %>% withSpinner()
                )
              ),
            )
        ),
        div(class = "tab-pane fade show",  # Simulation output -----------------
            id = "out-tab-content",
            role = "tabpanel",
            `aria-labelledby` = "out-tab",
            layout_column_wrap(
              width = 1/2,
              card(
                full_screen = TRUE,
                card_header(h3("Yield prediction"), class = "bg-warning"),
                checkboxInput("yield_categories", "By category?"),
                conditionalPanel(
                  condition = "input.yield_categories",
                  selectInput("yield_selected", "",
                              choices = c("region", "tillage", "fertilization", "pesticides")
                              )
                ),
                plotlyOutput("yield_plot") %>% withSpinner()
              ),
              card(
                full_screen = TRUE,
                card_header(h3("Bacterial biodiversity"), class = "bg-warning"),
                checkboxInput("bacteria_categories", "By category?"),
                conditionalPanel(
                  condition = "input.bacteria_categories",
                  selectInput("bacteria_selected", "",
                              choices = c("region", "tillage", "fertilization", "pesticides")
                              )
                ),
                plotlyOutput("bacteria_plot") %>% withSpinner()
              )
            ),

            layout_column_wrap(
              width = 1/2,
              card(
                full_screen = TRUE,
                card_header(h3("Fungal biodiversity"), class = "bg-warning"),
                checkboxInput("fungal_categories", "By category?"),
                conditionalPanel(
                  condition = "input.fungal_categories",
                  selectInput("fungal_selected", "",
                              choices = c("region", "tillage", "fertilization", "pesticides")
                              )
                ),
                plotlyOutput("fungal_plot") %>% withSpinner()
              ),
              card(
                full_screen = TRUE,
                card_header(h3("Nematode biodiversity"), class = "bg-warning"),
                checkboxInput("nema_categories", "By category?"),
                conditionalPanel(
                  condition = "input.nema_categories",
                  selectInput("nema_selected", "",
                              choices = c("region", "tillage", "fertilization", "pesticides")
                              )
                ),
                plotlyOutput("nema_plot") %>% withSpinner()
              )
            ),

            layout_column_wrap(
              width = 1,
              card(
                full_screen = TRUE,
                card_header(class = "bg-warning",
                            h3("Numerical values")
                ),
                card_body(
                  h4("Percentiles"),
                  tableOutput("summary_percentages"),
                  h4("Percentiles per region"),
                  tableOutput("summary_percentages_region"),
                  h4("Raw values"),
                  tableOutput("out_predictions"),
                )
              )
            )
        ),
        div(class = "tab-pane fade show",  # About -----------------------------
            id = "about-tab-content",
            role = "tabpanel",
            `aria-labelledby` = "about-tab",
            layout_column_wrap(
              width = 1,
              card_image(
                file = system.file("extdata", "UE.png", package = "soildiveragro")
              )
            ),
            layout_column_wrap(
              width = 1/3,
              "Version 0.1.0",
              actionButton("documentation", "Documentation", icon = icon("book")),
              actionButton("issue", "Found an issue?")
            )
            # layout_column_wrap(
            #   width = 1/2,
            #   card(
            #     class = "border-primary",
            #     tags$a(
            #       href = "https://docs.google.com/document/d/1JQ7n-8D-NPpI-jW8znUBNIqN7W-wut0thtNoIJLIVT8/edit?usp=sharing",
            #       "User manual"
            #     ),
            #     tags$a(
            #       "Report an Issue",
            #       href = "mailto:alberto.garre@upct.es?subject=%5BR4EU%5D%20SoilDiverAgroAppu&body=First%20Name%3A%0D%0ALast%20Name%3A%0D%0AOrganization%3A%0D%0ACountry%3A%0D%0ASoftware Version%3A%201.0.1%0D%0AFeedback%20or%20Issue%3A"
            #     )
            #   )
            # ),
        )
      )
    )


  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "soildiveragro"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
