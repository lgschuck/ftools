
#' Data Viz
#'
#' Function that generates App in Shiny for analysis of input dataset
#'
#' @param dataset dataset for analysis
#'
#' @examples
#' dataviz(mtcars)
#'
#' @export
#'
#' @importFrom data.table setDT
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @importFrom DT datatable formatCurrency DTOutput renderDT
#' @import dplyr
#' @import bslib
#' @import bsicons
#' @importFrom graphics abline hist
#' @importFrom stats median
#' @importFrom utils object.size

dataviz <- function(dataset){

  stopifnot(is.data.frame(dataset))
  setDT(dataset)

  var_num <- names(dataset[, sapply(dataset, is.numeric), with = FALSE])

  # close browser tab
  js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

  ui <- page_navbar(
    id = 'navbar',
    theme = bs_theme(
      bg = "#e6e5e3",
      fg = "#000",
      primary = "#0072B2",
      secondary = "#00aaB2"
      # success = "#009E73"
    ),
    title = "ftools - Data Viz",
    bg = '#536b8f',

    nav_panel("Summary",
              mainPanel(
                layout_column_wrap(
                  width = 1/3,
                  value_box(
                    title = "Rows",
                    value = nrow(dataset),
                    showcase = bs_icon("layout-text-sidebar-reverse"),
                    theme = "teal"
                  ),
                  value_box(
                    title = "Columns",
                    value = ncol(dataset),
                    showcase = bs_icon("layout-three-columns"),
                    theme = "teal"
                  ),
                  value_box(
                    title = "Size (MB)",
                    value = object.size(dataset)/1e6,
                    showcase = bs_icon("sd-card"),
                    theme = "teal"
                  )),
                card(full_screen = TRUE, max_height = 1200,
                     card_header(class = "bg-dark", "Variables"),
                     card_body(DTOutput('summary_t1'))
                ),
              )),
    nav_panel("Numeric",
              mainPanel(
                div(
                  fluidRow(
                    column(2, navlistPanel(
                      widths = c(12, 12), "Parameters",
                      tabPanel(selectInput('plots_sel_vars', 'Variable', choices = var_num )),
                      tabPanel(numericInput('plots_bins', 'Bins',
                                            value = 10, min = 5, step = 5)),
                      tabPanel(numericInput('plots_digits', 'Digits',
                                            value = 2, min = 0, max = 9, step = 1)),
                      tabPanel(numericInput('plots_percentile', 'Percentile',
                                            value = 50, min = 0, max = 100, step = 5))
                    )),
                    column(8, plotOutput('plots_g1')),
                    column(2, DTOutput('plots_t1')),
                  )),
                style = 'width:100%;')
    ),
    nav_panel(value = 'exit', title = "Exit",
              # exit js script
              tags$head(tags$script(HTML(js_exit))))
  )

  server <- function(input, output, session) {

    df <- reactiveValues()

    df$df <- dataset
    df$df_num <- subset(dataset, select = var_num)
    # summary page events -----------------------------------------------------
    summary_t1 <- reactive(
      datatable(
        data.frame(
          var = names(df$df),
          type = lapply(as.list(df$df), typeof) |> unlist(),
          class = lapply(as.list(df$df),
                          \(x) class(x) |> paste(collapse = '/')) |> unlist(),
          size = (lapply(as.list(df$df), object.size) |> unlist())/1e3
        ),
        options = list(dom = 'Bpf', pageLength = 10),
        rownames = F, colnames = c('Variable', 'type', 'class', 'Size (kB)'))
    )

    output$summary_t1 <- renderDT(
      summary_t1() |>
        formatCurrency('size', digits = 2, currency = '')
    )

    # numeric page events -----------------------------------------------------
    df_plots <- reactive(
      df$df_num |>
        select(input$plots_sel_vars)
    )

    value_plots <- reactive(df_plots() |> pull(input$plots_sel_vars))

    output$plots_g1 <- renderPlot(
      {
        hist(value_plots(), col = 'deepskyblue2', breaks = input$plots_bins,
             main = 'Histogram', xlab = '', ylab = 'Count')
        abline(v = pn(value_plots(), input$plots_percentile/100), col = 'red')
      }

    )

    plots_t1 <- reactive(
      datatable(
        data.frame(
          var = c('Minimum', 'Percentile 5', 'Percentile 25', 'Median', 'Mean',
                  'Percentile 75', 'Percentile 95', 'Maximum',
                  paste('Percentile', input$plots_percentile),
                  'Number of NAs'),
          value = c(mina(value_plots()),
                    pn(value_plots(), 0.05),
                    pn(value_plots(), 0.25),
                    median(value_plots(), na.rm = T),
                    mean(value_plots(), na.rm = T),
                    pn(value_plots(), 0.75),
                    pn(value_plots(), 0.95),
                    mana(value_plots()),
                    pn(value_plots(), input$plots_percentile/100),
                    length(value_plots()[is.na(value_plots())])
          )
        ),
        options = list(dom = 'B', pageLength = 20),
        rownames = F, colnames = NULL))

    output$plots_t1 <- renderDT(
      plots_t1() |>
        formatCurrency('value', digits = input$plots_digits, currency = '')
    )

    # exit app event ----------------------------------------------------------
    observe({
      if (input$navbar == 'exit') {

        session$sendCustomMessage(type = "closeWindow", message = "message")
        stopApp()
      }
    })
  } # end of server function

  ### Run App -----------------------------------------------------------------
  shinyApp(ui, server, options = list(launch.browser = T))
}
