#' prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom bslib card card_header card_body card_footer
#' @importFrom readxl excel_sheets read_excel
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_pill nav_panel navset_card_underline
#' @importFrom ggplot2 geom_hline geom_violin coord_flip geom_boxplot theme_gray labs
#' @importFrom rlang .data
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom ggsci scale_fill_flatui scale_colour_flatui
#'
mod_prediction_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # layout_column_wrap(
    #   width = 1,
    #   card(class = "bg-warning",
    #     p(paste0(
    #       "A short description",
    #       "of what we are actually predicting"
    #       ))
    #   )
    # ),

    layout_column_wrap(
      width = 1/2,
      layout_column_wrap(
        width = 1,
        card(
          fill = FALSE,
          card_header(h4("Uploading the soil data"), class = "bg-primary"),
          card_body(
            p(
              paste("Please upload the soil data using the Excel template.",
                    "It can be downloaded using this link:")
            ),
            downloadLink(NS(id, "downloadData"), "Download template"),
            p(
              paste("Once you have filled it in, please uploaded it using the 'Browse' button",
                    "and select the sheet where it is located from 'Sheet name'.",
                    "Finally, please click on the 'Load' button.")
            )
          ),
          card_footer(
            layout_column_wrap(
              width = 1,

              fileInput(NS(id, "excel_file"), "Excel file"),
              # selectInput(NS(id, "excel_sheet"), "Sheet name", choices = c())
            ),
            layout_column_wrap(
              width = 1,
              actionButton(NS(id, "load_data"), "Load",
                           outline = TRUE, flat = FALSE,
                           status = "primary"
              )
            )
          ))
      ),
      layout_column_wrap(
        width = 1,
        navset_card_underline(
          full_screen = TRUE,
          title = h3("Input data"),
          header = card_body(
            p("Please use this information to check the data was uploaded correctly.")
          ),
          nav_panel(
            title = "Metadata",
            tableOutput(NS(id, "summary_inputs"))
          ),
          nav_panel(
            title = "Physicochemical",
            plotOutput(NS(id, "plot_physico"))
          ),
          nav_panel(
            title = "Climate",
            plotOutput(NS(id, "plot_climate"))
          )
        ),
      )
    ),

    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = TRUE,
        card_header(h3("Yield prediction"), class = "bg-warning"),
        checkboxInput(NS(id, "yield_categories"), "By category?"),
        conditionalPanel(
          ns = NS(id),
          condition = "input.yield_categories",
          selectInput(NS(id, "yield_selected"), "", choices = c("region", "tillage", "fertilization", "pesticides"))
        ),
        plotlyOutput(NS(id, "yield_plot"))
      ),
      card(
        card_header(h3("Bacterial biodiversity"), class = "bg-warning"),
        checkboxInput(NS(id, "bacteria_categories"), "By category?"),
        conditionalPanel(
          ns = NS(id),
          condition = "input.bacteria_categories",
          selectInput(NS(id, "bacteria_selected"), "", choices = c("region", "tillage", "fertilization", "pesticides"))
        ),
        plotlyOutput(NS(id, "bacteria_plot"))
      )
    ),

    layout_column_wrap(
      width = 1/2,
      card(
        card_header(h3("Fungal biodiversity"), class = "bg-warning"),
        checkboxInput(NS(id, "fungal_categories"), "By category?"),
        conditionalPanel(
          ns = NS(id),
          condition = "input.fungal_categories",
          selectInput(NS(id, "fungal_selected"), "", choices = c("region", "tillage", "fertilization", "pesticides"))
        ),
        plotlyOutput(NS(id, "fungal_plot"))
      ),
      card(
        card_header(h3("Nematode biodiversity"), class = "bg-warning"),
        checkboxInput(NS(id, "nema_categories"), "By category?"),
        conditionalPanel(
          ns = NS(id),
          condition = "input.nema_categories",
          selectInput(NS(id, "nema_selected"), "", choices = c("region", "tillage", "fertilization", "pesticides"))
        ),
        plotlyOutput(NS(id, "nema_plot"))
      )
    ),

    layout_column_wrap(
      width = 1,
      card(
        card_header(class = "bg-warning",
                    h3("Numerical values")
        ),
        card_body(
          h4("Percentiles"),
          tableOutput(NS(id, "summary_percentages")),
          h4("Percentiles per region"),
          tableOutput(NS(id, "summary_percentages_region")),
          h4("Raw values"),
          tableOutput(NS(id, "out_predictions")),
        )
      )
    )
  )
}

#' prediction Server Functions
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom tibble column_to_rownames as_tibble tibble
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom magrittr %>%
#' @importFrom purrr imap_dfr map
#' @importFrom ggplot2 ggplot geom_col aes facet_wrap labs
#' @importFrom rlang set_names
#' @importFrom tidyr drop_na
#'
#' @noRd
mod_prediction_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## Downloading the template

    output$downloadData <- downloadHandler(
      filename = function() {
        "simulation_template.xlsx"
      },
      content = function(file) {
        my_file <- system.file("extdata", "data_template.xlsx", package = "soildiveragro")
        file.copy(my_file, file)
      }
    )

    ## Variables for the Excel file

    excelFile <- reactive({
      input$excel_file
    })

    # observeEvent(excelFile(), {  # Update the choices
    #
    #   validate(need(excelFile(), message = ""))
    #   updateSelectInput(session = session,
    #                     inputId = "excel_sheet",
    #                     choices = excel_sheets(excelFile()$datapath)
    #   )
    #
    # })

    ## Reactive values

    d_new <- reactiveVal()
    pred_new <- reactiveVal()

    ## Do stuff when loading the data

    observeEvent(input$load_data, {

      d_new(NULL)
      pred_new(NULL)

      default_names <- c("sample", "tillage", "fertilization", "pesticides",
                         "PedoClim (choose from list)", "pH_w", "TOC",
                         "Nt", "Pav", "SA", "SI", "CL", "Mgex", "Naex",
                         "Caex", "Kex", "EC",
                         "Mean annual temperature (T ºC) (average last 30 years)",
                         "Mean annual precipitation (P in mm) (average last 30 years)",
                         "mean T in ºC (in the previous 12 months to soil sampling)",
                         "total P in mm (in the previous 12 months to soil sampling)",
                         "average of lowest T ºC (in the previous 12 months to soil sampling)",
                         "average of highest T ºC (in the previous 12 months to soil sampling)"
                         )

      ## Load the data

      d <- read_excel(excelFile()$datapath,
                      # sheet = input$excel_sheet,
                      skip = 1,
                      sheet = "Template",
                      col_types = c(rep("text", 5),
                                    rep("numeric", 18)
                                    )
                      )

      ## Do a few checks

      #- Number of columns

      if (length(colnames(d)) != length(default_names)) {

        showModal(modalDialog(
          title = "Error loading the template",
          "The number of columns does not match. Did you delete/add any?",
          easyClose = TRUE,
          footer = NULL
        ))

        safeError("The number of columns is not right")
        return(NULL)

      }

      d <- set_names(d, default_names)

      #- Format of the columns

      old_nrow <- nrow(d)

      d <- drop_na(d)

      if (old_nrow != nrow(d)) {

        showModal(modalDialog(
          title = "Error loading the template",
          "There was a problem loading some rows. Did you use the wrong data format? Did you leave some cell empty?",
          easyClose = TRUE,
          footer = NULL
        ))

        safeError("The number of rows is not right")
        return(NULL)

      }

      ## Update the data reactive

      d_new(d)

      ## Scale

      x_new <- d %>%
        select(-"tillage", -"fertilization", -"pesticides", -`PedoClim (choose from list)`)

      data(scales)

      x_scaled <- x_new %>%
        pivot_longer(-sample) %>%
        left_join(scales) %>%
        mutate(x = (value - m)/s) %>%
        select(sample, name, x) %>%
        pivot_wider(names_from = name, values_from = x) %>%
        column_to_rownames("sample") %>%
        as.matrix()

      ## Rotate

      data(rotations)

      x_rotated <- x_scaled %*% rotations

      ## Put together with the categorical variables

      new_regress <- x_rotated %>%
        as_tibble(rownames = "sample") %>%
        left_join(.,
                  select(d, sample, tillage, fertilization, pesticides, `PedoClim (choose from list)`)
        )

      ## Make the prediction

      data(pred_models)

      out <- pred_models %>%
        map(.,
            ~ predict(., newdata = new_regress)
        ) %>%
        map(.,
            ~ tibble(sample = new_regress$sample,
                     value = .)
        ) %>%
        imap_dfr(., ~ mutate(.x, index = .y)) %>%
        pivot_wider(names_from = index, values_from = value) %>%
        mutate(yield = sq_yield^2) %>%
        select(-sq_yield) %>%
        select(-earth_prev)

      ## Save the results

      pred_new(out)

    })

    ## Showing the inputs

    output$summary_inputs <- renderTable({

      validate(need(d_new(), message = ""))

      d_new() %>%
        select(sample, tillage, fertilization, pesticides, starts_with("PedoC"))
    })

    output$plot_physico <- renderPlot({

      validate(need(d_new(), message = ""))

      # browser()

      d_new() %>%
        select(-tillage, -fertilization, -pesticides, -starts_with("PedoC"),
               -starts_with("Mean"), -starts_with("mean"), -starts_with("total"),
               -starts_with("average")) %>%
        pivot_longer(-sample) %>%
        ggplot() +
        geom_col(aes(x = sample, y = value, fill = sample)) +
        facet_wrap("name", scales = "free") +
        scale_fill_flatui() +
        theme_gray(base_size = 14) +
        labs(x = "", y = "")

    })

    output$plot_climate <- renderPlot({

      validate(need(d_new(), message = ""))

      d_new() %>%
        select(sample,
               starts_with("Mean"), starts_with("mean"), starts_with("total"),
               starts_with("average")) %>%
        pivot_longer(-sample) %>%
        ggplot() +
        geom_col(aes(x = sample, y = value, fill = sample)) +
        facet_wrap("name", scales = "free") +
        scale_fill_flatui() +
        theme_gray(base_size = 14) +
        labs(x = "", y = "")

    })

    ## Tables of the outputs

    output$out_predictions <- renderTable({

      validate(need(pred_new(), message = ""))

      pred_new() %>%
        set_names(c("Sample code", "Bacterial biodiversity", "Fungal biodiversity", "Nematode biodiversity", "Yield [UNIT]"))
    })

    output$summary_percentages <- renderTable({

      validate(need(pred_new(), message = ""))

      data(model_data)

      # browser()

      pred <- pred_new()

      pred %>%
        split(.$sample) %>%
        map(.,
            ~ tibble(
              bacteria = mean(.$bacteria > model_data$bacteria$chao1, na.rm = TRUE),
              fungal = mean(.$fungal > model_data$fungal$chao1, na.rm = TRUE),
              nematode = mean(.$nematode > model_data$nematode$chao1, na.rm = TRUE),
              yield = mean(.$yield > model_data$yield$yield, na.rm = TRUE)
              )
            ) %>%
        imap_dfr(., ~ mutate(.x, sample = .y)) %>%
        select(sample, everything()) %>%
        pivot_longer(-sample) %>%
        mutate(
          category = ifelse(value < .2, "low",
                         ifelse(value < .40, "low-medium",
                                ifelse(value < .6, "medium",
                                       ifelse(value < .8, "medium-high",
                                              "high"
                                              )
                                       )
                                )
                         )
        ) %>%
        mutate(value = paste0(round(value*100, 0), "% (", category, ")")) %>%
        select(-category) %>%
        pivot_wider() %>%
        set_names(c("Sample code", "Bacterial biodiversity", "Fungal biodiversity", "Nematode biodiversity", "Yield [UNIT]"))

    })

    output$summary_percentages_region <- renderTable({

      validate(need(pred_new(), message = ""))

      data(model_data)

      pred <- d_new() %>%
        select(sample, region = `PedoClim (choose from list)`) %>%
        left_join(pred_new())

      pred %>%
        split(.$sample) %>%
        map(.,
            ~ tibble(
              region = .$region,
              bacteria = mean(.$bacteria > dplyr::filter(model_data$bacteria, region == .$region)$chao1, na.rm = TRUE),
              fungal = mean(.$fungal > dplyr::filter(model_data$fungal, region == .$region)$chao1, na.rm = TRUE),
              nematode = mean(.$nematode > dplyr::filter(model_data$nematode, region == .$region)$chao1, na.rm = TRUE),
              yield = mean(.$yield > dplyr::filter(model_data$yield, region == .$region)$yield, na.rm = TRUE)
              )
            ) %>%
        imap_dfr(., ~ mutate(.x, sample = .y)) %>%
        select(sample, everything()) %>%
        pivot_longer(-c("sample", "region")) %>%
        mutate(
          category = ifelse(value < .2, "low",
                            ifelse(value < .40, "low-medium",
                                   ifelse(value < .6, "medium",
                                          ifelse(value < .8, "medium-high",
                                                 "high"
                                          )
                                   )
                            )
          )
        ) %>%
        mutate(value = paste0(round(value*100, 0), "% (", category, ")")) %>%
        select(-category) %>%
        pivot_wider() %>%
        set_names(c("Sample code", "PedoClim region", "Bacterial biodiversity", "Fungal biodiversity", "Nematode biodiversity", "Yield [UNIT]"))

    })

    ## Plots of the outputs

    output$yield_plot <- renderPlotly({

      validate(need(pred_new(), message = ""))

      # browser()

      data(model_data)

      if (!input$yield_categories) {
        p <- model_data$yield %>%
          ggplot() +
          geom_boxplot(aes(x = 1, y = yield)) +
          geom_hline(aes(yintercept = yield, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()
      } else {
        p <- model_data$yield %>%
          ggplot() +
          geom_boxplot(aes(x = .data[[input$yield_selected]], y = yield)) +
          geom_hline(aes(yintercept = yield, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()

      }

      p <- p + labs(y = "Yield [UNIT]", x = "")

      ggplotly(p)

    })

    output$bacteria_plot <- renderPlotly({

      validate(need(pred_new(), message = ""))

      # browser()

      data(model_data)

      if (!input$bacteria_categories) {
        p <- model_data$bacteria %>%
          ggplot() +
          geom_boxplot(aes(x = 1, y = chao1)) +
          geom_hline(aes(yintercept = bacteria, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()
      } else {
        p <- model_data$bacteria %>%
          ggplot() +
          geom_boxplot(aes(x = .data[[input$bacteria_selected]], y = chao1)) +
          geom_hline(aes(yintercept = bacteria, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()

      }

      p <- p + labs(y = "Biodiversity (Chao1)", x = "")

      ggplotly(p)

    })

    output$fungal_plot <- renderPlotly({

      validate(need(pred_new(), message = ""))

      # browser()

      data(model_data)

      if (!input$fungal_categories) {
        p <- model_data$fungal %>%
          ggplot() +
          geom_boxplot(aes(x = 1, y = chao1)) +
          geom_hline(aes(yintercept = fungal, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()

      } else {
        p <- model_data$fungal %>%
          ggplot() +
          geom_boxplot(aes(x = .data[[input$fungal_selected]], y = chao1)) +
          geom_hline(aes(yintercept = fungal, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()

      }

      p <- p + labs(y = "Biodiversity (Chao1)", x = "")

      ggplotly(p)

    })

    output$nema_plot <- renderPlotly({

      validate(need(pred_new(), message = ""))

      # browser()

      data(model_data)

      if (!input$nema_categories) {
        p <- model_data$nema %>%
          ggplot() +
          geom_boxplot(aes(x = 1, y = chao1)) +
          geom_hline(aes(yintercept = nematode, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()

      } else {
        p <- model_data$nema %>%
          ggplot() +
          geom_boxplot(aes(x = .data[[input$nema_selected]], y = chao1)) +
          geom_hline(aes(yintercept = nematode, colour = sample), data = pred_new(),
                     linetype = 2) +
          coord_flip() +
          scale_colour_flatui()

      }

      p <- p + labs(y = "Biodiversity (Chao1)", x = "")

      ggplotly(p)

    })

  })
}

## To be copied in the UI
# mod_prediction_ui("prediction_1")

## To be copied in the server
# mod_prediction_server("prediction_1")
