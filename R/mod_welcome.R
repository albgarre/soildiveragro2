#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_columns card_image card_title
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_columns(
      col_widths = c(4, 8),
      # min_height = "400px",
      card_image(
        file = system.file("extdata", "trigo.jpg", package = "soildiveragro")
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_body(
            class = "bg-secondary",
            p(paste0(
              "The Decision Support Tool (DST) is a free, web-based software developed through the SoilDiverAgro project to help end-users, including farmers and other stakeholders, optimize their wheat cropping systems and management practices."
            )),
            p(
              "The DST uses machine learning models developed from experimental data collected during the SoilDiverAgro project to predict agricultural soil properties. It takes into account environmental, physicochemical, and land-use parameters, as well as biodiversity indices. The tool guides users through a step-by-step process, asking for key information such as pedoclimatic conditions, crop location, agricultural challenges, and current cropping systems. Based on this input, the DST provides tailored recommendations to enhance productivity, profitability, and sustainability."
            ),
            h4(
              "Your tool to make more informed decisions and optimize your farming practices!"
            )
          )
        )
        )
      ),
    layout_columns(
      col_widths = 12,
      h3("HOW IT WORKS"),
      card(
        card_title("Step 1:"),
        p(
          "Once you enter in the DST platform you will need to download the excel template to complete the data referring your soil properties. It will ask you about…."
        ),
        p(
          "If you don’t have the data required you can always obtain the information depending on your region with the following initiatives. "
        ),
        h3("Imagen excel")
      ),
      card(
        card_title("Step 2:"),
        p(
          "Upload your excel file, selecting the correspondent sheet where you had inserted your data. "
        ),
        h3("Imagen para subir elexcel")
      ),
      card(
        card_title("Step 3:"),
        p(
          "Click on submit"
        ),
        h3("Imagen boton")
      ),
      card(
        card_title("Step 4:"),
        p(
          "Access to the dashboard where you can see the results."
        ),
        h3("Imagen dashboard"),
        p(
          "On the left hand side you can visualize three different results, depending on…"
        ),
        p(
          "On the right hand side you can…. "
        )
      )

    )


    # layout_column_wrap(
    #   width = 1/2,
    #   card(
    #     p(paste0(
    #       "The Decision Support Tool (DST) is a free, web-based software developed through the SoilDiverAgro project to help end-users, including farmers and other stakeholders, optimize their wheat cropping systems and management practices."
    #     )),
    #     p(
    #       "The DST uses machine learning models developed from experimental data collected during the SoilDiverAgro project to predict agricultural soil properties. It takes into account environmental, physicochemical, and land-use parameters, as well as biodiversity indices. The tool guides users through a step-by-step process, asking for key information such as pedoclimatic conditions, crop location, agricultural challenges, and current cropping systems. Based on this input, the DST provides tailored recommendations to enhance productivity, profitability, and sustainability."
    #     ),
    #     h4(
    #       "Your tool to make more informed decisions and optimize your farming practices"
    #     )
    #   ),
    #   layout_column_wrap(
    #     width = 1,
    #     card(
    #       card_header("HOW IT WORKS"),
    #       card_body(
    #         h5("Step 1:"),
    #         p(
    #           "Once you enter in the DST platform you will need to download the excel template to complete the data referring your soil properties. It will ask you about…."
    #         ),
    #         p(
    #           "If you don’t have the data required you can always obtain the information depending on your region with the following initiatives. "
    #         ),
    #         h3("Imagen excel")
    #       ),
    #       card_body(
    #         h5("Step 2:"),
    #         p(
    #           "Upload your excel file, selecting the correspondent sheet where you had inserted your data. "
    #         ),
    #         h3("Imagen para subir elexcel")
    #       ),
    #       card_body(
    #         h5("Step 3:"),
    #         p(
    #           "Click on submit"
    #         ),
    #         h3("Imagen boton")
    #       ),
    #       card_body(
    #         h5("Step 4:"),
    #         p(
    #           "Access to the dashboard where you can see the results."
    #         ),
    #         h3("Imagen dashboard"),
    #         p(
    #           "On the left hand side you can visualize three different results, depending on…"
    #         ),
    #         p(
    #           "On the right hand side you can…. "
    #         )
    #       )
    #
    #
    #     )
    #   )
    # )

  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
