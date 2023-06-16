12.2.4 <- function() {
    shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::h3(
        shiny::tags$a(
          href = "https://mastering-shiny.org/action-tidy.html#user-data",
          "12.2.4 User supplied data"
        )
      ),
      shiny::fileInput("data", "dataset", accept = ".tsv"),
      shiny::selectInput("var", "var", character()),
      shiny::numericInput("min", "min", 1, min = 0, step = 1),
      shiny::tableOutput("output"),
      shiny::verbatimTextOutput("vals")
    ),
    server = function(input, output, session) {
      data <- shiny::reactive({
                shiny::req(input$data)
              vroom::vroom(input$data$datapath)
            })
        shiny::observe({
          shiny::updateSelectInput(session, "var", choices = names(data()))
        }) |>
          shiny::bindEvent(data())

        shiny::observe({
          val <- data()[[input$var]]
          shiny::updateNumericInput(session, "min", value = min(val))
        }) |>
          shiny::bindEvent(input$var)

        output$output <- shiny::renderTable({
          shiny::req(input$var)

          data() %>%
            dplyr::filter(.data[[input$var]] > input$min) %>%
            dplyr::arrange(.data[[input$var]]) %>%
            head(10)
          })

      output$vals <- shiny::renderPrint(
        unlist(
          shiny::reactiveValuesToList(x = input, all.names = TRUE)
        )
      )
    }
  )
}
