
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

dataviz <- function(dataset) {
  stopifnot(is.data.frame(dataset))
  dataset_classes <- class(dataset)
  setDT(dataset)

  var_num <- names(dataset[, sapply(dataset, is.numeric), with = FALSE])

  # close browser tab
  js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

  main_box_values <- layout_columns(
    col_widths = c(3, 3, 3, 3),
    value_box(
      title = 'Dataset Class',
      value = dataset_classes[1],
      showcase = bs_icon('file-binary'),
      theme = 'primary'
    ),
    value_box(
      title = 'Rows / Columns',
      value = paste(
        nrow(dataset) |> f_num(dec = '.', big = ','),
        '/',
        ncol(dataset) |> f_num(dec = '.', big = ',')
      ),
      showcase = bs_icon('layout-text-sidebar-reverse'),
      theme = 'primary'
    ),
    value_box(
      title = "Columns with NA's",
      value = sum(colSums(is.na(dataset)) > 0),
      showcase = bs_icon("database-x"),
      theme = "primary"
    ),
    value_box(
      title = 'Size (MB)',
      value = (object.size(dataset) / 1e6) |> as.numeric() |> round(2),
      showcase = bs_icon('sd-card'),
      theme = 'primary'
    )
  )

  ui <- page_navbar(
    id = 'navbar',
    theme = bs_theme(
      bg = '#f2f2f2',
      fg = '#000',
      primary = '#02517d',
      secondary = '#0072B2',
      success = '#009E73'
    ),
    title = 'Data Viz',
    bg = '#02517d',

    # useBusyIndicators(),
    # page summary ------------------------------------------------------------
    nav_panel(
      'Summary',
      icon = bs_icon('info-square-fill'),
      useBusyIndicators(),
      layout_column_wrap(
        width = 1,
        min_height = '100px',
        main_box_values,
        height = '100px'
      ),
      card(
        card_body(
          style = 'background-color: #02517d;',
          navset_card_tab(
            height = '800px',
            full_screen = T,
            nav_panel(
              'Highlights', full_screen = T,
              card_body(
                layout_column_wrap(
                  value_box(
                    title = 'Numeric Vars',
                    value = sapply(dataset, is.numeric) |> sum(),
                    showcase = bs_icon('123'),
                    theme = 'warning'
                  ),
                  value_box(
                    title = 'Char Vars',
                    value = sapply(dataset, is.character) |> sum(),
                    showcase = bs_icon('alphabet'),
                    theme = 'secondary'
                  ),
                  value_box(
                    title = 'Factor Vars',
                    value = sapply(dataset, is.factor) |> sum(),
                    showcase = bs_icon('diagram-3'),
                    theme = 'success'
                  ),
                  value_box(
                    title = 'Date Vars',
                    value = sapply(dataset, \(x) inherits(x, 'Date')) |> sum(),
                    showcase = bs_icon('calendar3'),
                    theme = 'bg-purple'
                  )
                ),
                layout_column_wrap(
                  value_box(
                    title = "Var with most NA's",
                    value = textOutput('summary_var_most_nas'),
                    showcase = bs_icon('database-x'),
                    theme = 'bg-red'
                  ),
                  value_box(
                    title = "Var with max value",
                    value = textOutput('summary_var_max_value'),
                    showcase = bs_icon('graph-up-arrow'),
                    theme = 'bg-orange'
                  ),
                  value_box(
                    title = "Var with min value",
                    value = textOutput('summary_var_min_value'),
                    showcase = bs_icon('graph-down-arrow'),
                    theme = 'light'
                  ),
                )
              )
            ),
            nav_panel('Variables', full_screen = T, card_body(DTOutput('summary_t1'))),
          )
        )
      )
    ),

    # page numeric ------------------------------------------------------------
    nav_panel(
      'Numeric',
      icon = bs_icon('bar-chart-fill'),
      layout_column_wrap(
        width = 1,
        min_height = '100px',
        main_box_values,
        height = '100px'
      ),
      card(
        full_screen = T,
        card_body(
          style = 'background-color: #02517d;',
          layout_columns(
            col_widths = c(2, 7, 3),
            height = '800px',
            card(
              card_header('Parameters', class = 'bg-primary'),
              card_body(
                selectInput('num_sel_vars', 'Variable', choices = var_num),
                selectInput('num_sel_vars2', 'Variable 2', choices = var_num),
                numericInput(
                  'num_percentile',
                  'Percentile',
                  value = 50,
                  min = 0,
                  max = 100,
                  step = 5
                ),
                checkboxInput('num_outliers', 'Remove Outliers', value = F)
              )
            ),
            navset_card_tab(
              full_screen = T,
              nav_panel(
                'Histogram',
                full_screen = T,
                card_body(plotOutput('num_g1')),
                card_footer(numericInput(
                  'num_bins',
                  'Bins',
                  value = 10,
                  min = 5,
                  step = 5
                ))
              ),
              nav_panel('Boxplot', full_screen = T, card_body(plotOutput('num_g2'))),
              nav_panel(
                'Scatter',
                full_screen = T,
                card_body(plotOutput('num_g3')),
                card_footer(
                  checkboxInput('num_scatter_lm', 'Plot Linear Model', value = F)
                )
              ),
              nav_panel(
                'Linear Model',
                full_screen = T,
                card_body(verbatimTextOutput('num_linear_model')),
                card_footer(layout_columns(
                  numericInput('num_sample_size', 'Sample Size',
                               value = 100, min = 0, max = 100, step = 10),
                  actionButton(
                    'num_btn_scatter_lm_run',
                    'Run Linear Model',
                    icon = icon('gear')
                  ),
                  actionButton(
                    'num_btn_scatter_lm_clear',
                    'Clear Linear Model',
                    icon = icon('x')
                  )
                ))
              )
            ),
            card(
              full_screen = T,
              card_header('Statistics', class = 'bg-primary'),
              card_body(DTOutput('num_t1')),
              card_footer(
                numericInput(
                  'num_t1_digits',
                  'Digits',
                  value = 2,
                  min = 0,
                  max = 9,
                  step = 1
                )
              )
            )
          )
        )
      )
    ),

    # page exit ---------------------------------------------------------------
    nav_spacer(),
    nav_panel(
      value = 'exit',
      title = 'Exit',
      icon = bs_icon('x-square-fill'),
      tags$head(tags$script(HTML(js_exit)))
    )
  )

  server <- function(input, output, session) {
    df <- reactiveValues()

    df$df <- dataset
    df$df_num <- subset(dataset, select = var_num)
    # summary page events -----------------------------------------------------
    summary_t1 <- reactive(
      data.frame(
        var = names(df$df),
        type = lapply(df$df, typeof) |> unlist(),
        class = lapply(df$df, \(x) class(x) |> paste(collapse = '/')) |> unlist(),
        size = (lapply(df$df, object.size) |> unlist()) / 1e3,
        min = lapply(df$df, \(x) if (is.numeric(x))
          mina(x)
          else
            NA) |> unlist(),
        max = lapply(df$df, \(x) if (is.numeric(x))
          mana(x)
          else
            NA) |> unlist(),
        n_nas = sapply(df$df, \(x) length(x[is.na(x)])),
        nas = sapply(df$df, \(x) length(x[is.na(x)])) / sapply(df$df, length)))

    output$summary_t1 <- renderDT(
      summary_t1() |>
        datatable(
          extensions = 'ColReorder',
          rownames = F,
          colnames = c(
            'Variable',
            'Type',
            'Class',
            'Size (kB)',
            'Min',
            'Max',
            "NA's",
            "% NA's"
          ),
          options = list(
            dom = 'Bftp',
            pageLength = 10,
            colReorder = T,
            columnDefs = list(
              list(targets = 0, width = '400px'),
              list(targets = 1:2, width = '200px'),
              list(targets = 3:7, width = '100px')
            )
          )
        ) |>
        formatCurrency(
          c('size', 'min', 'max', 'n_nas'),
          digits = 2,
          currency = ''
        ) |>
        formatPercentage('nas', digits = 2)
    )

    output$summary_var_most_nas <- renderText(
      {
        if(summary_t1() |> filter(n_nas > 0) |> nrow() < 1) { 'None'
        } else { summary_t1() |> filter(n_nas > 0)|> pull(var) }
      }
    )

    output$summary_var_max_value <- renderText(
      summary_t1() |> arrange(-max) |> slice(1) |> pull(var)
    )

    output$summary_var_min_value <- renderText(
      summary_t1() |> arrange(min) |> slice(1) |> pull(var)
    )

    # numeric page events -----------------------------------------------------
    # df to plots page --------------------------------------------------------
    df_num_plots <- reactive({
      df_num_plots_temp <- df$df_num |>
        select(input$num_sel_vars, input$num_sel_vars2)

      if (input$num_outliers) {
        q1 <- p25(df_num_plots_temp |> pull(input$num_sel_vars))
        q3 <- p75(df_num_plots_temp |> pull(input$num_sel_vars))

        interquatile <- q3 - q1

        df_num_plots_temp <- df_num_plots_temp[
          get(input$num_sel_vars) >= (q1 - 1.5 * interquatile) &
            get(input$num_sel_vars) <= (q3 + 1.5 * interquatile), ]
      }
      df_num_plots_temp
    })

    # values to numeric page --------------------------------------------------
    num_var <- reactive(df_num_plots() |> pull(input$num_sel_vars))
    num_var2 <- reactive(df_num_plots() |> pull(input$num_sel_vars2))

    # render histogram --------------------------------------------------------
    output$num_g1 <- renderPlot({
      hist(
        num_var(),
        col = 'steelblue2',
        breaks = input$num_bins,
        main = '',
        xlab = '',
        ylab = 'Count'
      )
      abline(v = pn(num_var(), input$num_percentile / 100), col = 'red')
    }) |> bindCache(num_var(), input$num_bins, input$num_percentile)
    # render boxplot ----------------------------------------------------------
    output$num_g2 <- renderPlot({
      boxplot(num_var(),
              horizontal = T,
              col = 'steelblue2')
      abline(v = pn(num_var(), input$num_percentile / 100), col = 'red')
    }) |> bindCache(num_var(), input$num_percentile)

    # render scatter plot -----------------------------------------------------
    output$num_g3 <- renderPlot({
      if (input$num_scatter_lm &
          num_linear_model$y_name == input$num_sel_vars &
          num_linear_model$x_name == input$num_sel_vars2) {
        plot(
          num_var2(),
          num_var(),
          type = 'p',
          col = 'steelblue2',
          xlab = input$num_sel_vars2,
          ylab = input$num_sel_vars
        )
        lines(
          num_linear_model$x,
          num_linear_model$y,
          col = 'brown3',
          lty = 'dotdash'
        )
      } else {
        plot(
          num_var2(),
          num_var(),
          type = 'p',
          col = 'steelblue2',
          xlab = input$num_sel_vars2,
          ylab = input$num_sel_vars
        )
      }
    }) |> bindCache(
      input$num_scatter_lm,
      num_linear_model$y_name,
      num_linear_model$x_name,
      input$num_sel_vars,
      input$num_sel_vars2,
      num_var2(),
      num_var(),
      num_linear_model$x,
      num_linear_model$y
    )
    # linear model ------------------------------------------------------------
    num_linear_model <- reactiveValues(
      model = NULL,
      x = NULL,
      y = NULL,
      x_name = '',
      y_name = ''
    )

    observe({
      if (input$num_sel_vars == input$num_sel_vars2) {
        showNotification('Choose diferent variables for X and Y.',
                         duration = 2.5,
                         type = 'message')
      } else {
        num_linear_model$y_name <- input$num_sel_vars
        num_linear_model$x_name <- input$num_sel_vars2

        num_var_size <- length(num_var())
        num_sample_size <- min(num_var_size,
                               floor(num_var_size * min(1, input$num_sample_size/100)))

        lm_sample <- sample.int(num_var_size, num_sample_size, replace = F) |>
          sort()
        var_y <- num_var()[lm_sample]
        var_x <- num_var2()[lm_sample]
        num_linear_model$model <- lm(var_y ~ var_x, model = F)
        num_linear_model$x <- var_x
        num_linear_model$y <- num_linear_model$model$fitted.values
        showNotification('Lm model completed.', duration = 1.5, type = 'message')
      }
    }) |> bindEvent(input$num_btn_scatter_lm_run)

    observe({
      num_linear_model$model <- NULL
      num_linear_model$x <- NULL
      num_linear_model$y <- NULL
      num_linear_model$x_name <-
        num_linear_model$y_name <- ''
      showNotification('Lm model cleared.', duration = 1.5, type = 'message')
    }) |> bindEvent(input$num_btn_scatter_lm_clear)

    # print linear model ------------------------------------------------------
    output$num_linear_model <- renderPrint({
      list(
        'Formula' = paste(
          num_linear_model$y_name,
          '~',
          num_linear_model$x_name
        ),
        'Model' = summary(num_linear_model$model)
      )
    }) |> bindCache(num_linear_model$y_name,
                    num_linear_model$x_name,
                    num_linear_model$model)

    # metrics -----------------------------------------------------------------
    num_metrics_obs <- reactive(length(num_var()))
    num_metrics_n_nas <- reactive(length(num_var()[is.na(num_var())]))
    num_metrics_min <- reactive(mina(num_var()))
    num_metrics_q1 <- reactive(pn(num_var(), 0.25))
    num_metrics_median <- reactive(median(num_var(), na.rm = T))
    num_metrics_mean <- reactive(mean(num_var(), na.rm = T))
    num_metrics_q3 <- reactive(pn(num_var(), 0.75))
    num_metrics_max <- reactive(mana(num_var()))
    num_metrics_percentile <- reactive(pn(num_var(), input$num_percentile / 100))
    num_metrics_sd <- reactive(sd(num_var(), na.rm = T))
    num_metrics_correlation <- reactive(cor(
      num_var(),
      num_var2(),
      method = 'p',
      use = 'na.or.complete'
    ))

    num_t1 <- reactive(datatable(
      data.frame(
        var = c(
          paste(
            "% NA's (",
            num_metrics_n_nas(),
            '/',
            num_metrics_obs(),
            ')'
          ),
          'Minimum',
          'Percentile 25',
          'Median',
          'Mean',
          'Percentile 75',
          'Maximum',
          paste('Percentile', input$num_percentile),
          'Standard Deviation',
          'Pearson Correlation'
        ),
        value = c(
          num_metrics_n_nas() / num_metrics_obs() * 100,
          num_metrics_min(),
          num_metrics_q1(),
          num_metrics_median(),
          num_metrics_mean(),
          num_metrics_q3(),
          num_metrics_max(),
          num_metrics_percentile(),
          num_metrics_sd(),
          num_metrics_correlation()
        )
      ),
      options = list(
        dom = 'B',
        pageLength = 20,
        ordering = F
      ),
      rownames = F,
      colnames = NULL
    ))

    output$num_t1 <- renderDT(num_t1() |>
                                  formatCurrency(
                                    'value',
                                    digits = input$num_t1_digits,
                                    currency = ''
                                  ))

    # exit app event ----------------------------------------------------------
    observe({
      if (input$navbar == 'exit') {
        session$sendCustomMessage(type = 'closeWindow', message = 'message')
        stopApp()
      }
    })
  } # end of server function

  ### Run App -----------------------------------------------------------------
  shinyApp(ui, server, options = list(launch.browser = T))
  }
