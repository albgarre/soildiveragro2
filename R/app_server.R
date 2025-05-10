#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT renderDT DTOutput
#' @importFrom shinycssloaders withSpinner
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # mod_welcome_server("welcome_1")
  # mod_prediction_server("prediction_1")
  # mod_doc_server("doc_1")
  # mod_about_server("about_1")

  ## Show the documentation

  observeEvent(input$documentation, {
    showModal(modalDialog(
      size = "xl",
      easyClose = TRUE,
      title = "Soil methods",
      DTOutput("soil_methods_table")
      ))

    })

  output$soil_methods_table <- renderDT({
    read_excel(system.file("extdata", "data_template.xlsx", package = "soildiveragro"),
               sheet = "Soil Methods",
               skip = 1
    )
  })

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

  observeEvent(excelFile(), {

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

    ## Show the notification

    showNotification(
      type = "message",
      "Excel file loaded"
    )

  })

  ## Showing the inputs

  output$summary_inputs <- renderTable({

    validate(need(d_new(), message = "Please upload an Excel file"))

    d_new() %>%
      select(sample, tillage, fertilization, pesticides, starts_with("PedoC"))
  })

  output$plot_physico <- renderPlot({

    validate(need(d_new(), message = "Please upload an Excel file"))

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

    validate(need(d_new(), message = "Please upload an Excel file"))

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

    validate(need(pred_new(), message = "Please upload an Excel file"))

    pred_new() %>%
      set_names(c("Sample code", "Bacterial biodiversity", "Fungal biodiversity", "Nematode biodiversity", "Yield [UNIT]"))
  })

  output$summary_percentages <- renderTable({

    validate(need(pred_new(), message = "Please upload an Excel file"))

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

    validate(need(pred_new(), message = "Please upload an Excel file"))

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

    validate(need(pred_new(), message = "Please upload an Excel file"))

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

    validate(need(pred_new(), message = "Please upload an Excel file"))

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

    validate(need(pred_new(), message = "Please upload an Excel file"))

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

    validate(need(pred_new(), message = "Please upload an Excel file"))

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


}
