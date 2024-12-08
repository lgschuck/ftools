
#' Data Viz
#'
#' Function that generates App in Shiny for analysis of input dataset
#'
#' @param dataset dataset for analysis
#'
#' @examples
#' if(interactive()) dataviz(mtcars)
#'
#' @export
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @import data.table
#' @importFrom dplyr select filter mutate pull
#' @import bslib
#' @import bsicons
#' @import DT
#' @importFrom graphics abline hist
#' @importFrom stats median
#' @importFrom utils object.size

dataviz <- function(dataset){

  stopifnot(is.data.frame(dataset))
  dataset_classes <- class(dataset)
  setDT(dataset)

  var_num <- names(dataset[, sapply(dataset, is.numeric), with = FALSE])

  # close browser tab
  js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

  main_box_values <- layout_columns(
    height = "15%",
    col_widths = c(3, 3, 3, 3),
    value_box(
      title = "Dataset Class",
      value = dataset_classes[1],
      showcase = bs_icon("file-binary", class = "custom-icon"),
      theme = "primary"
    ),
    value_box(
      title = "Rows",
      value = nrow(dataset) |> format(big.mark = ','),
      showcase = bs_icon("layout-text-sidebar-reverse"),
      theme = "primary"
    ),
    value_box(
      title = "Columns",
      value = ncol(dataset) |> format(big.mark = ','),
      showcase = bs_icon("layout-three-columns"),
      theme = "primary"
    ),
    # value_box(
    #   title = "Columns with NA's",
    #   value = sum(colSums(is.na(dataset)) > 0),
    #   showcase = bs_icon("question-circle"),
    #   theme = "primary"
    # ),
    value_box(
      title = "Size (MB)",
      value = (object.size(dataset)/1e6) |> as.numeric() |> round(2),
      showcase = bs_icon("sd-card"),
      theme = "primary"
    ),
  )

  ui <- page_navbar(
    id = 'navbar',
    theme = bs_theme(
      bg = "#f2f2f2",
      fg = "#000",
      primary = "#0072B2",
      secondary = "#00aaB2",
      success = "#009E73"
    ),
    title = "Banritools - Data Viz",
    bg = '#09679c',

    # useBusyIndicators(),
    # page summary ------------------------------------------------------------
    nav_panel("Summary", icon = bs_icon("info-square-fill"),
              useBusyIndicators(),
              main_box_values,
              card(full_screen = T, height = '625px',
                    card_header(class = "bg-primary", "Variables"),
                    card_body(DTOutput('summary_t1'))
              ),
    ),

    # page numeric ------------------------------------------------------------
    nav_panel("Numemic", icon = bs_icon("bar-chart-fill"),
      main_box_values,
      card(full_screen = T, card_body(
      layout_columns(col_widths = c(2, 7, 3), #row_heights = 600,
         card(card_header('Parameters', class = "bg-primary"),
              card_body(
                selectInput('plots_sel_vars', 'Variable', choices = var_num),
                selectInput('plots_sel_vars2', 'Variable 2', choices = var_num),
                numericInput('plots_percentile', 'Percentile',
                             value = 50, min = 0, max = 100, step = 5),
                checkboxInput('plots_outliers', 'Remove Outliers', value = F)
              )),
         navset_card_tab(#height = 600,
           full_screen = T,
           nav_panel("Histogram", full_screen = T,
                     card_body(plotOutput('plots_g1')),
                     card_footer(numericInput('plots_bins', 'Bins',
                                              value = 10, min = 5, step = 5),)),
           nav_panel("Boxplot", full_screen = T,
                     card_body(plotOutput('plots_g2'))),
           nav_panel("Scatter", full_screen = T,
                     card_body(plotOutput('plots_g3')),
                     card_footer(checkboxInput('plots_scatter_lm', 'Linear Model', value = F))
           ),
           nav_panel("Linear Model", full_screen = T,
                     verbatimTextOutput("plots_linear_model")),
         ),
         card(full_screen = T,
              card_header('Statistics', class = "bg-primary"),
              card_body(DTOutput('plots_t1')),
              card_footer(numericInput('plots_digits', 'Digits',
                                       value = 2, min = 0, max = 9, step = 1)))
      ))),
    ),

    # page exit ---------------------------------------------------------------
    nav_spacer(),
    nav_panel(value = 'exit', title = "Exit", icon = bs_icon('x-square-fill'),
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
          size = (lapply(as.list(df$df), object.size) |> unlist())/1e3,
          min = lapply(as.list(df$df), \(x) if(is.numeric(x)) mina(x) else NA) |> unlist(),
          max = lapply(as.list(df$df), \(x) if(is.numeric(x)) mana(x) else NA) |> unlist(),
          n_nas = sapply(df$df, \(x) length(x[is.na(x)])),
          nas = sapply(df$df, \(x) length(x[is.na(x)]))/sapply(df$df, length)
        ),
        extensions = 'ColReorder',
        options = list(dom = 'Bftp', pageLength = 6, colReorder = T),
        rownames = F, colnames = c('Variable', 'Type', 'Class', 'Size (kB)',
                                   'Min', 'Max', "NA's", "% NA's"))
    )

    output$summary_t1 <- renderDT(
      summary_t1() |>
        formatCurrency(c('size', 'min', 'max', 'n_nas'), digits = 2, currency = '') |>
        formatPercentage('nas', digits = 2)
    )

    # numeric page events -----------------------------------------------------
    df_plots <- reactive(
      {
        df_plots_temp <- df$df_num |>
          select(input$plots_sel_vars, input$plots_sel_vars2)

        if(input$plots_outliers){
          q1 <- p25(df_plots_temp |> pull(input$plots_sel_vars))
          q3 <- p75(df_plots_temp |> pull(input$plots_sel_vars))

          interquatile <- q3 - q1

          df_plots_temp <- df_plots_temp[get(input$plots_sel_vars) >= (q1 - 1.5 * interquatile) &
                                           get(input$plots_sel_vars) <= (q3 + 1.5 * interquatile), ]

        }

        df_plots_temp
      }
    )  |>
      bindCache(input$plots_sel_vars, input$plots_sel_vars2, input$plots_outliers)

    plots_var <- reactive(df_plots() |> pull(input$plots_sel_vars))
    plots_var2 <- reactive(df_plots() |> pull(input$plots_sel_vars2))

    output$plots_g1 <- renderPlot(
      {
        hist(plots_var(), col = 'deepskyblue', breaks = input$plots_bins,
             main = '', xlab = '', ylab = 'Count')
        abline(v = pn(plots_var(), input$plots_percentile/100), col = 'red')
      }
    )

    output$plots_g2 <- renderPlot(
      {
        boxplot(plots_var(), horizontal = T, col = 'deepskyblue')
        abline(v = pn(plots_var(), input$plots_percentile/100), col = 'red')
      }
    )

    lm_sample <- reactive(
      sample.int(length(plots_var()),
                 min(15e3, length(plots_var())), replace = F) |>
        sort()
    )

    plots_linear_model <- reactive(
      if(input$plots_scatter_lm){

        plots_x <- plots_var2()[lm_sample()]
        plots_y <- plots_var()[lm_sample()]
        lm(plots_y ~ plots_x)

      })

    output$plots_linear_model <- renderPrint({ summary(plots_linear_model()) })

    output$plots_g3 <- renderPlot(
      {
        plot(plots_var2(), plots_var(),
             type = 'p', col = 'deepskyblue',
             xlab = input$plots_sel_vars2,
             ylab = input$plots_sel_vars)
        if(input$plots_scatter_lm){
          lines(plots_var2()[lm_sample()],
                plots_linear_model()$fitted.values,
                col = 'brown3')
        }
      }
    )

    # metrics -----------------------------------------------------------------
    plots_metrics_obs <- reactive(length(plots_var()))
    plots_metrics_n_nas <- reactive(length(plots_var()[is.na(plots_var())]))
    plots_metrics_min <- reactive(mina(plots_var()))
    plots_metrics_q1 <- reactive(pn(plots_var(), 0.25))
    plots_metrics_median <- reactive(median(plots_var(), na.rm = T))
    plots_metrics_mean <- reactive(mean(plots_var(), na.rm = T))
    plots_metrics_q3 <- reactive(pn(plots_var(), 0.75))
    plots_metrics_max <- reactive(mana(plots_var()))
    plots_metrics_percentile <- reactive(pn(plots_var(), input$plots_percentile/100))
    plots_metrics_sd <- reactive(sd(plots_var(), na.rm = T))
    plots_metrics_correlation <- reactive(cor(plots_var(), plots_var2(),
                                              method = 'p', use = "na.or.complete"))

    plots_t1 <- reactive(
      datatable(
        data.frame(
          var = c(paste("% NA's (", plots_metrics_n_nas(), '/',
                        plots_metrics_obs(), ')'),
                  'Minimum',
                  'Percentile 25', 'Median', 'Mean', 'Percentile 75', 'Maximum',
                  paste('Percentile', input$plots_percentile),
                  'Standard Deviation', 'Pearson Correlation'),
          value = c(plots_metrics_n_nas()/plots_metrics_obs() * 100,
                    plots_metrics_min(),
                    plots_metrics_q1(),
                    plots_metrics_median(),
                    plots_metrics_mean(),
                    plots_metrics_q3(),
                    plots_metrics_max(),
                    plots_metrics_percentile(),
                    plots_metrics_sd(),
                    plots_metrics_correlation()
          )
        ),
        options = list(dom = 'B', pageLength = 20, ordering = F),
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
