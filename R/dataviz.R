
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
#' @importFrom dplyr arrange filter mutate pull select
#' @import bslib
#' @import bsicons
#' @import DT
#' @importFrom graphics abline hist
#' @importFrom stats median
#' @importFrom utils object.size
#' @importFrom graphics boxplot lines
#' @importFrom stats cor lm sd var

dataviz <- function(dataset) {
  stopifnot(is.data.frame(dataset))
  dataset_classes <- class(dataset)
  setDT(dataset)

  var_num_names <- names(dataset[, sapply(dataset, is.numeric), with = FALSE])

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
      value = (object.size(dataset) / 2^20) |> as.numeric() |> round(2),
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
      success = '#009E73',
      font_scale = 1
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
                    title = 'Character Vars',
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
                    value = sapply(
                      dataset,
                      \(x) inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))) |>
                      sum(),
                    showcase = bs_icon('calendar3'),
                    theme = 'bg-purple'
                  )
                ),
                layout_column_wrap(
                  value_box(
                    title = "Var with most NA's",
                    value = textOutput('summary_var_most_nas'),
                    showcase = bs_icon('database-x'),
                    theme = 'bg-red',
                    p(textOutput('summary_var_most_nas_n', inline = T), ' rows')
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                  value_box(
                    title = "Var with biggest % of NA's",
                    value = textOutput('summary_var_biggest_perc_nas'),
                    showcase = bs_icon('percent'),
                    theme = 'light',
                    p(textOutput('summary_var_biggest_perc_nas_perc', inline = T), ' %')
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                  value_box(
                    title = 'Var with max value',
                    value = textOutput('summary_var_max_value', inline = T),
                    showcase = bs_icon('graph-up-arrow', placement = 'top'),
                    theme = 'pink',
                    p('Max value:', textOutput('summary_max_value', inline = T)),
                    hr(),
                    p(bs_icon('graph-down-arrow'), 'Var with min value'),
                    p(textOutput('summary_var_min_value', inline = T)),
                    p('Min value:', textOutput('summary_min_value', inline = T))
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                  value_box(
                    title = "Var with biggest size",
                    value = textOutput('summary_var_biggest_size'),
                    showcase = bs_icon('sd-card'),
                    theme = 'teal',
                    p(textOutput('summary_var_biggest_size_size', inline = T), 'kB')
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top')
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
            navset_card_tab(
              full_screen = T,
              nav_panel('Parameters',
                        selectInput('num_sel_vars', 'Variable', var_num_names),
                        selectInput('num_sel_vars2', 'Variable 2', var_num_names, var_num_names[2])),
              nav_panel('Filters', checkboxInput('num_outliers', 'Remove Outliers', F))
            ),
            navset_card_tab(
              full_screen = T,
              nav_panel(
                'Distribution',
                full_screen = T,
                card_body(plotOutput('num_g_dist')),
                card_footer(
                  layout_column_wrap(
                    radioButtons('num_radio_dist_plot', 'Plot type:',
                                 c('Dots' = 'dots',
                                   'Histogram' = 'hist',
                                   'Boxplot' = 'boxplot'), inline = T),
                    numericInput('num_var_percentile', 'Percentile', 50, 0, 100, 5),
                    numericInput('num_bins', 'Bins', 10, 5, step = 10) |>
                      tooltip('Only for Histrograms')
                  )
                )
              ),
              nav_panel(
                'Scatter',
                full_screen = T,
                card_body(plotOutput('num_g_scatter')),
                card_footer(
                  checkboxInput('num_scatter_lm', 'Plot Linear Model', F) |>
                    tooltip('Show the line only if LM model was created')
                )
              ),
              nav_panel(
                'Linear Model',
                full_screen = T,
                navset_card_tab(
                  nav_panel(
                    'Parameters',
                    sliderInput('num_sample_size', 'Sample Size (%)', 0, 100, 100) |>
                      tooltip('Applied only if valid values are greater than 10.000'),
                    actionButton('num_btn_scatter_lm_run', 'Run Linear Model', icon('gear')),
                    actionButton('num_btn_scatter_lm_clear', 'Clear Linear Model', icon('x'))),
                  nav_panel('Output', verbatimTextOutput('num_linear_model')),
                  nav_panel(
                    'Residuals',
                    plotOutput('num_g_lm_resid'),
                    card_footer(
                      layout_column_wrap(
                        radioButtons('num_radio_lm_resid', 'Plot type:',
                                     c('Dots' = 'dots',
                                       'Histogram' = 'hist',
                                       'Boxplot' = 'boxplot'), inline = T),
                        actionButton('num_btn_lm_resid', 'Plot residuals'))
                    )
                  ),
                )),
            ),
            navset_card_tab(
              nav_panel(
                'Stats',
                full_screen = T,
                card_body(DTOutput('num_t1')),
                card_footer(numericInput('num_t1_digits', 'Digits', 2, 0, 9, 1))
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
    df$df_num <- subset(dataset, select = var_num_names)
    # summary page events -----------------------------------------------------
    summary_t1 <- reactive(
      data.frame(
        var = names(df$df),
        type = lapply(df$df, typeof) |> unlist(),
        class = lapply(df$df, \(x) class(x) |> paste(collapse = '/')) |> unlist(),
        size = (lapply(df$df, object.size) |> unlist()) / 2^10,
        min = lapply(df$df, \(x) if (is.numeric(x))
          mina(x)
          else
            NA) |> unlist(),
        max = lapply(df$df, \(x) if (is.numeric(x))
          mana(x)
          else
            NA) |> unlist(),
        n_nas = sapply(df$df, \(x) length(x[is.na(x)])),
        perc_nas = sapply(df$df, \(x) length(x[is.na(x)])) / sapply(df$df, length)))

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
        formatPercentage('perc_nas', digits = 2)
    )

    output$summary_var_most_nas <- renderText(
      {
        if(summary_t1() |> filter(n_nas > 0) |> nrow() < 1) { 'None'
        } else {
          summary_t1() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$summary_var_most_nas_n <- renderText(
      {
        if(summary_t1() |> filter(n_nas > 0) |> nrow() < 1) { '0'
        } else {
          summary_t1() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(n_nas) |> f_num()}
      }
    )

    output$summary_var_biggest_perc_nas <- renderText(
      {
        if(summary_t1() |> filter(perc_nas > 0) |> nrow() < 1) { 'None'
        } else {
          summary_t1() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$summary_var_biggest_perc_nas_perc <- renderText(
      {
        if(summary_t1() |> filter(perc_nas > 0) |> nrow() < 1) { '0'
        } else {
          summary_t1() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(perc_nas) * 100 }
      }
    )

    output$summary_var_max_value <- renderText(
      summary_t1() |> arrange(-max) |> head(1) |> pull(var)
    )

    output$summary_max_value <- renderText(
      summary_t1() |> arrange(-max) |> head(1) |> pull(max) |> f_num(dig = 3)
    )

    output$summary_var_min_value <- renderText(
      summary_t1() |> arrange(min) |> head(1) |> pull(var)
    )

    output$summary_min_value <- renderText(
      summary_t1() |> arrange(min) |> head(1) |> pull(min) |> f_num(dig = 3)
    )

    output$summary_var_biggest_size <- renderText(
      summary_t1() |> arrange(-size) |> head(1) |> pull(var)
    )

    output$summary_var_biggest_size_size <- renderText(
      summary_t1() |> arrange(-size) |> head(1) |> pull(size) |> round(2)
    )
    # numeric page events -----------------------------------------------------
    # df to plots page --------------------------------------------------------
    df_num_plots <- reactive({
      df_num_plots_temp <- df$df_num |>
        select(input$num_sel_vars, input$num_sel_vars2)

      if (input$num_outliers) {
        q1 <- p25(df_num_plots_temp |> pull(input$num_sel_vars))
        q3 <- p75(df_num_plots_temp |> pull(input$num_sel_vars))

        dist_interquatile <- 1.5 * (q3 - q1)

        df_num_plots_temp <- df_num_plots_temp[
          get(input$num_sel_vars) >= (q1 - dist_interquatile) &
            get(input$num_sel_vars) <= (q3 + dist_interquatile), ]
      }
      df_num_plots_temp
    })

    # values to numeric page --------------------------------------------------
    num_var <- reactive(df_num_plots() |> pull(input$num_sel_vars))
    num_var2 <- reactive(df_num_plots() |> pull(input$num_sel_vars2))
    num_var_percentile <- reactive(pn(num_var(), input$num_var_percentile / 100))

    # render histogram --------------------------------------------------------
    output$num_g_dist <- renderPlot({
      if(input$num_radio_dist_plot == 'hist'){
        hist(
          num_var(),
          col = 'steelblue2',
          breaks = input$num_bins,
          main = '',
          xlab = '',
          ylab = 'Count'
        )
        abline(v = num_var_percentile(), col = 'brown3')
      } else if (input$num_radio_dist_plot == 'boxplot'){
        boxplot(num_var(),
                horizontal = T, col = 'steelblue2')
        abline(v = num_var_percentile(), col = 'brown3')
      } else if (input$num_radio_dist_plot == 'dots'){
        plot(num_var(), col = 'steelblue2',
             ylab = 'Values')
        abline(h = num_var_percentile(), col = 'brown3')
      }
    }) |> bindCache(num_var(), input$num_radio_dist_plot, input$num_bins,
                    input$num_var_percentile)
    # render scatter plot -----------------------------------------------------
    output$num_g_scatter <- renderPlot({
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
        mtext(paste('Adjusted R Squared:',
                    summary(num_linear_model$model)$r.squared |> round(4)),
              side = 3)
      } else {
        plot(
          num_var2(),
          num_var(),
          type = 'p',
          col = 'steelblue2',
          xlab = input$num_sel_vars2,
          ylab = input$num_sel_vars
        )
        mtext(paste('Pearson Correlation:', num_stats_correlation() |> round(4)))
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

        if(num_var_size < 10e3) {
          var_y <- num_var()
          var_x <- num_var2()
        } else {
          num_sample_size <- min(num_var_size,
                                 floor(num_var_size * min(1, max(0, input$num_sample_size/100))))
          lm_sample <- sample.int(num_var_size, num_sample_size, replace = F) |>
            sort()
          var_y <- num_var()[lm_sample]
          var_x <- num_var2()[lm_sample]
        }

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

    # plot linear model residuals ---------------------------------------------
    output$num_g_lm_resid <- renderPlot({

      if(!isTruthy(num_linear_model$model)){
        showNotification('Create an Lm model', type = 'message')
      } else {
        if(input$num_radio_lm_resid == 'hist'){
          hist(num_linear_model$model$residuals,
               col = 'steelblue2',
               main = '',
               xlab = '',
               ylab = 'Count')
        } else if (input$num_radio_lm_resid == 'boxplot'){
          boxplot(num_linear_model$model$residuals,
                  horizontal = T, col = 'steelblue2')
        } else if (input$num_radio_lm_resid == 'dots'){
          plot(num_linear_model$model$residuals, col = 'steelblue2',
               ylab = 'Residuals')
          abline(h = 0, col = 'brown3', lty = 'dotdash')
        }
      }
    }) |> bindEvent(input$num_btn_lm_resid)

    # metrics -----------------------------------------------------------------
    num_stats_obs <- reactive(length(num_var()))
    num_stats_n_nas <- reactive(length(num_var()[is.na(num_var())]))
    num_stats_min <- reactive(mina(num_var()))
    num_stats_q1 <- reactive(pn(num_var(), 0.25))
    num_stats_median <- reactive(median(num_var(), na.rm = T))
    num_stats_mean <- reactive(mean(num_var(), na.rm = T))
    num_stats_q3 <- reactive(pn(num_var(), 0.75))
    num_stats_max <- reactive(mana(num_var()))
    num_stats_sd <- reactive(sd(num_var(), na.rm = T))
    num_stats_correlation <- reactive(cor(
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
            num_stats_n_nas(),
            '/',
            num_stats_obs(),
            ')'
          ),
          'Minimum',
          'Percentile 25',
          'Median',
          'Mean',
          'Percentile 75',
          'Maximum',
          paste('Percentile', input$num_var_percentile),
          'Standard Deviation',
          'Pearson Correlation'
        ),
        value = c(
          num_stats_n_nas() / num_stats_obs() * 100,
          num_stats_min(),
          num_stats_q1(),
          num_stats_median(),
          num_stats_mean(),
          num_stats_q3(),
          num_stats_max(),
          num_var_percentile(),
          num_stats_sd(),
          num_stats_correlation()
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
