
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

  # ds_names <- names(dataset[, sapply(dataset, is.numeric), with = FALSE])
  ds_names <- names(dataset)

  empty_plot <- function(msg = 'No plot', c = 2){
    plot(1:10, 1:10, type = 'n', xlab = '', ylab = '')
    text(5, 5, msg, cex = c)
  }

  # close browser tab
  js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

  main_value_box <- layout_columns(
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
        uiOutput('pS_value_box'),
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
                    value = textOutput('pS_var_most_nas'),
                    showcase = bs_icon('database-x'),
                    theme = 'bg-red',
                    p(textOutput('pS_var_most_nas_n', inline = T), ' rows')
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                  value_box(
                    title = "Var with biggest % of NA's",
                    value = textOutput('pS_var_biggest_perc_nas'),
                    showcase = bs_icon('percent'),
                    theme = 'light',
                    p(textOutput('pS_var_biggest_perc_nas_perc', inline = T), ' %')
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                  value_box(
                    title = 'Var with max value',
                    value = textOutput('pS_var_max_value', inline = T),
                    showcase = bs_icon('graph-up-arrow', placement = 'top'),
                    theme = 'pink',
                    p('Max value:', textOutput('pS_max_value', inline = T)),
                    hr(),
                    p(bs_icon('graph-down-arrow'), 'Var with min value'),
                    p(textOutput('pS_var_min_value', inline = T)),
                    p('Min value:', textOutput('pS_min_value', inline = T))
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top'),
                  value_box(
                    title = "Var with biggest size",
                    value = textOutput('pS_var_biggest_size'),
                    showcase = bs_icon('sd-card'),
                    theme = 'teal',
                    p(textOutput('pS_var_biggest_size_size', inline = T), 'kB')
                  ) |> tooltip("Showing 1, there may be ties.", placement = 'top')
                )
              )
            ),
            nav_panel('Variables', full_screen = T, card_body(DTOutput('pS_t1'))),
            
          )
        )
      )
    ),

    # page edit ----------------------------------------------------------------
    nav_panel(
      'Edit',
      icon = bs_icon('funnel'),
      layout_column_wrap(
        width = 1,
        min_height = '100px',
        uiOutput('pE_value_box'),
        height = '100px'
      ),
      card(
        full_screen = T,
        card_body(
          style = 'background-color: #02517d;',
          layout_columns(
            # col_widths = c(2, 7, 3),
            height = '800px',
            navset_card_tab(
              full_screen = T,
              nav_panel(
                'Filter', full_screen = T,
                card_body(
                  selectInput('pE_transform_sel_vars', 'Variable', ds_names),
                  selectInput('pE_transform_operator', 'Operator', 
                              c('==', '>', '>=', '<', '<=', '!=', 'is.na', 'not.na')),
                  textInput('pE_transform_value', 'Value'),
                  actionButton('pE_btn_transform_apply', 'Apply filters', icon('check'))))),
      )))
    ),

    # page analysis -----------------------------------------------------------
    nav_panel(
      'Analysis',
      icon = bs_icon('bar-chart-fill'),
      layout_column_wrap(
        width = 1,
        min_height = '100px',
        uiOutput('pA_value_box'),
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
                        selectInput('pA_sel_vars', 'Variable', ds_names),
                        selectInput('pA_sel_vars2', 'Variable 2', ds_names, ds_names[2])),
              nav_panel('Filters', checkboxInput('pA_outliers', 'Remove Outliers', F) |>
                          tooltip('Only for numeric vars', placement = 'top'))
            ),
            navset_card_tab(
              full_screen = T,
              nav_panel(
                'Distribution',
                full_screen = T,
                card_body(plotOutput('pA_g_dist')),
                card_footer(
                  layout_column_wrap(
                    radioButtons('pA_radio_dist_plot', 'Plot type:',
                                 c('Dots' = 'dots',
                                   'Histogram' = 'hist',
                                   'Boxplot' = 'boxplot',
                                   'Barplot' = 'barplot'), inline = T),
                    numericInput('pA_var_percentile', 'Percentile', 50, 0, 100, 5),
                    numericInput('pA_bins', 'Bins', 10, 5, step = 10) |>
                      tooltip('Only for Histrograms')
                  )
                )
              ),
              nav_panel(
                'Scatter',
                full_screen = T,
                card_body(plotOutput('pA_g_scatter')),
                card_footer(
                  layout_column_wrap(
                    checkboxInput('pA_scatter_lm', 'Plot Linear Model', F) |>
                      tooltip('Show the line only if LM model was created'),
                    actionButton('pA_btn_scatter', 'Plot', icon('gear'))
                  )
                )
              ),
              nav_panel(
                'Linear Model',
                full_screen = T,
                navset_card_tab(
                  nav_panel(
                    'Parameters',
                    sliderInput('pA_sample_size', 'Sample Size (%)', 0, 100, 100) |>
                      tooltip('Applied only if valid values are greater than 10.000'),
                    actionButton('pA_btn_scatter_lm_run', 'Run Linear Model', icon('gear')),
                    actionButton('pA_btn_scatter_lm_clear', 'Clear Linear Model', icon('x'))),
                  nav_panel('Output', verbatimTextOutput('pA_linear_model')),
                  nav_panel(
                    'Residuals',
                    plotOutput('pA_g_lm_resid'),
                    card_footer(
                      layout_column_wrap(
                        radioButtons('pA_radio_lm_resid', 'Plot type:',
                                     c('Dots' = 'dots',
                                       'Histogram' = 'hist',
                                       'Boxplot' = 'boxplot'), inline = T),
                        actionButton('pA_btn_lm_resid', 'Plot residuals'))
                    )
                  ),
                )),
            ),
            navset_card_tab(
              nav_panel(
                'Stats',
                full_screen = T,
                card_body(DTOutput('pA_t1')),
                card_footer(numericInput('pA_t1_digits', 'Digits', 2, 0, 9, 1))
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
    df$df_als <- copy(dataset)

    # main value boxes ---------------------------------------------------------
    main_value_box <- reactive({
      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = 'Dataset Class',
            value = df$df |> class() |> head(1),
            showcase = bs_icon('file-binary'),
            theme = 'primary'
          ),
          value_box(
            title = 'Rows / Columns',
            value = paste(
              nrow(df$df) |> f_num(dec = '.', big = ','),
              '/',
              ncol(df$df) |> f_num(dec = '.', big = ',')
            ),
            showcase = bs_icon('layout-text-sidebar-reverse'),
            theme = 'primary'
          ),
          value_box(
            title = "Columns with NA's",
            value = sum(colSums(is.na(df$df)) > 0),
            showcase = bs_icon("database-x"),
            theme = "primary"
          ),
          value_box(
            title = 'Size (MB)',
            value = (object.size(df$df) / 2^20) |> as.numeric() |> round(2),
            showcase = bs_icon('sd-card'),
            theme = 'primary'
          )
        )
      )
    })

    output$pS_value_box <- renderUI({ main_value_box() })
    output$pE_value_box <- renderUI({ main_value_box() })
    output$pA_value_box <- renderUI({ main_value_box() })

    # summary page events -----------------------------------------------------
    pS_t1 <- reactive(
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

    output$pS_t1 <- renderDT(
      pS_t1() |>
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

    output$pS_var_most_nas <- renderText(
      {
        if(pS_t1() |> filter(n_nas > 0) |> nrow() < 1) { 'None'
        } else {
          pS_t1() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$pS_var_most_nas_n <- renderText(
      {
        if(pS_t1() |> filter(n_nas > 0) |> nrow() < 1) { '0'
        } else {
          pS_t1() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(n_nas) |> f_num()}
      }
    )

    output$pS_var_biggest_perc_nas <- renderText(
      {
        if(pS_t1() |> filter(perc_nas > 0) |> nrow() < 1) { 'None'
        } else {
          pS_t1() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$pS_var_biggest_perc_nas_perc <- renderText(
      {
        if(pS_t1() |> filter(perc_nas > 0) |> nrow() < 1) { '0'
        } else {
          pS_t1() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(perc_nas) * 100 }
      }
    )

    output$pS_var_max_value <- renderText(
      pS_t1() |> arrange(-max) |> head(1) |> pull(var)
    )

    output$pS_max_value <- renderText(
      pS_t1() |> arrange(-max) |> head(1) |> pull(max) |> f_num(dig = 3)
    )

    output$pS_var_min_value <- renderText(
      pS_t1() |> arrange(min) |> head(1) |> pull(var)
    )

    output$pS_min_value <- renderText(
      pS_t1() |> arrange(min) |> head(1) |> pull(min) |> f_num(dig = 3)
    )

    output$pS_var_biggest_size <- renderText(
      pS_t1() |> arrange(-size) |> head(1) |> pull(var)
    )

    output$pS_var_biggest_size_size <- renderText(
      pS_t1() |> arrange(-size) |> head(1) |> pull(size) |> round(2)
    )


    # summary transform tab ---------------------------------------------------

    observe({
      showNotification('To be implemented', type = 'message', duration = 3)
    }) |>  bindEvent(input$pE_btn_transform_apply)

    # analysis page events ----------------------------------------------------
    pA_outliers_index <- reactive({
      v <- df$df |> pull(input$pA_sel_vars)
      if(input$pA_outliers & is.numeric(v)) {
        q1 <- p25(v)
        q3 <- p75(v)
        dist_interquatile <- 1.5 * (q3 - q1)
        v >= (q1 - dist_interquatile) & v <= (q3 + dist_interquatile)
      } else {
        rep(T, length(v))
      }
    })

    # values to analysis page -------------------------------------------------
    pA_var <- reactive({
      n <- df$df |> pull(input$pA_sel_vars)
      n[pA_outliers_index()]
    })

    pA_var2 <- reactive({
      n <- df$df |> pull(input$pA_sel_vars2)
      n[pA_outliers_index()]
    })

    pA_var_percentile <- reactive(
      if(is.numeric(pA_var())){
        pn(pA_var(), input$pA_var_percentile / 100)
      } else { NA }
    )

    # render histogram --------------------------------------------------------
    output$pA_g_dist <- renderPlot({
      if(input$pA_radio_dist_plot == 'hist'){
        if(!is.numeric(pA_var())){
          empty_plot('Value must be numeric')
        } else {
          hist(pA_var(),
               col = 'steelblue2',
               breaks = input$pA_bins,
               main = '',
               xlab = '',
               ylab = 'Count')
          abline(v = pA_var_percentile(), col = 'brown3')
        }
      } else if (input$pA_radio_dist_plot == 'boxplot'){
        if(!is.numeric(pA_var())){
          empty_plot('Value must be numeric')
        } else {
          boxplot(pA_var(), horizontal = T, col = 'steelblue2')
          abline(v = pA_var_percentile(), col = 'brown3')
        }
      } else if (input$pA_radio_dist_plot == 'dots'){
        if(!is.numeric(pA_var())){
          empty_plot('Value must be numeric')
        } else {
          plot(pA_var(), col = 'steelblue2', ylab = 'Values')
          abline(h = pA_var_percentile(), col = 'brown3')
        }
      } else if (input$pA_radio_dist_plot == 'barplot'){
        if(!is.numeric(pA_var())){
          barplot(table(pA_var()), col = 'steelblue2')
        } else {
          empty_plot('Value can not be numeric')
        }
      }
    }) |> bindCache(pA_var(), input$pA_radio_dist_plot, input$pA_bins,
                    input$pA_var_percentile)
    # render scatter plot -----------------------------------------------------
    output$pA_g_scatter <- renderPlot({
      if (input$pA_scatter_lm &
          pA_linear_model$y_name == input$pA_sel_vars &
          pA_linear_model$x_name == input$pA_sel_vars2) {
        plot(
          pA_var2(),
          pA_var(),
          type = 'p',
          col = 'steelblue2',
          xlab = input$pA_sel_vars2,
          ylab = input$pA_sel_vars
        )
        lines(
          pA_linear_model$x,
          pA_linear_model$y,
          col = 'brown3',
          lty = 'dotdash'
        )
        mtext(paste('Adjusted R Squared:',
                    summary(pA_linear_model$model)$r.squared |> round(4)),
              side = 3)
      } else {
        plot(
          pA_var2(),
          pA_var(),
          type = 'p',
          col = 'steelblue2',
          xlab = input$pA_sel_vars2,
          ylab = input$pA_sel_vars
        )
        mtext(paste('Pearson Correlation:', pA_stats_correlation() |> round(4)))
      }
    }) |> bindCache(
      input$pA_scatter_lm,
      pA_linear_model$y_name,
      pA_linear_model$x_name,
      input$pA_sel_vars,
      input$pA_sel_vars2,
      pA_var2(),
      pA_var(),
      pA_linear_model$x,
      pA_linear_model$y
    ) |> bindEvent(input$pA_btn_scatter)

    # linear model ------------------------------------------------------------
    pA_linear_model <- reactiveValues(
      model = NULL,
      x = NULL,
      y = NULL,
      x_name = '',
      y_name = ''
    )

    observe({
      if (input$pA_sel_vars == input$pA_sel_vars2) {
        showNotification('Choose diferent variables for X and Y.',
                         duration = 2.5,
                         type = 'message')
      } else {
        pA_linear_model$y_name <- input$pA_sel_vars
        pA_linear_model$x_name <- input$pA_sel_vars2

        pA_var_size <- length(pA_var())

        if(pA_var_size < 10e3) {
          var_y <- pA_var()
          var_x <- pA_var2()
        } else {
          pA_sample_size <- min(pA_var_size,
                                 floor(pA_var_size * min(1, max(0, input$pA_sample_size/100))))
          lm_sample <- sample.int(pA_var_size, pA_sample_size, replace = F) |>
            sort()
          var_y <- pA_var()[lm_sample]
          var_x <- pA_var2()[lm_sample]
        }

        pA_linear_model$model <- lm(var_y ~ var_x, model = F)
        pA_linear_model$x <- var_x
        pA_linear_model$y <- pA_linear_model$model$fitted.values
        showNotification('Lm model completed.', duration = 1.5, type = 'message')
      }
    }) |> bindEvent(input$pA_btn_scatter_lm_run)

    observe({
      pA_linear_model$model <- NULL
      pA_linear_model$x <- NULL
      pA_linear_model$y <- NULL
      pA_linear_model$x_name <-
        pA_linear_model$y_name <- ''
      showNotification('Lm model cleared.', duration = 1.5, type = 'message')
    }) |> bindEvent(input$pA_btn_scatter_lm_clear)

    # print linear model ------------------------------------------------------
    output$pA_linear_model <- renderPrint({
      list(
        'Formula' = paste(
          pA_linear_model$y_name,
          '~',
          pA_linear_model$x_name
        ),
        'Model' = summary(pA_linear_model$model)
      )
    }) |> bindCache(pA_linear_model$y_name,
                    pA_linear_model$x_name,
                    pA_linear_model$model)

    # plot linear model residuals ---------------------------------------------
    output$pA_g_lm_resid <- renderPlot({

      if(!isTruthy(pA_linear_model$model)){
        empty_plot('No residuals to plot')
      } else {
        if(input$pA_radio_lm_resid == 'hist'){
          hist(pA_linear_model$model$residuals,
               col = 'steelblue2',
               main = '',
               xlab = '',
               ylab = 'Count')
        } else if (input$pA_radio_lm_resid == 'boxplot'){
          boxplot(pA_linear_model$model$residuals,
                  horizontal = T, col = 'steelblue2')
        } else if (input$pA_radio_lm_resid == 'dots'){
          plot(pA_linear_model$model$residuals, col = 'steelblue2',
               ylab = 'Residuals')
          abline(h = 0, col = 'brown3', lty = 'dotdash')
        }
      }
    }) |> bindEvent(input$pA_btn_lm_resid)

    # metrics -----------------------------------------------------------------
    pA_stats_obs <- reactive(length(pA_var()))
    pA_stats_n_nas <- reactive(length(pA_var()[is.na(pA_var())]))
    pA_stats_min <- reactive(if(is.numeric(pA_var())) mina(pA_var()) else NA)
    pA_stats_q1 <- reactive(if(is.numeric(pA_var())) pn(pA_var(), 0.25) else NA)
    pA_stats_median <- reactive(if(is.numeric(pA_var())) median(pA_var(), na.rm = T) else NA)
    pA_stats_mean <- reactive(if(is.numeric(pA_var())) mean(pA_var(), na.rm = T) else NA)
    pA_stats_q3 <- reactive(if(is.numeric(pA_var())) pn(pA_var(), 0.75) else NA)
    pA_stats_max <- reactive(if(is.numeric(pA_var())) mana(pA_var()) else NA)
    pA_stats_sd <- reactive(if(is.numeric(pA_var())) sd(pA_var(), na.rm = T) else NA)
    pA_stats_correlation <- reactive(
      if(is.numeric(pA_var()) & is.numeric(pA_var2())){
        cor(pA_var(), pA_var2(), method = 'p', use = 'na.or.complete')
      } else { NA }
    )
    # stats table -------------------------------------------------------------
    pA_t1 <- reactive(datatable(
      data.frame(
        var = c(
          paste(
            "% NA's (",
            pA_stats_n_nas(),
            '/',
            pA_stats_obs(),
            ')'
          ),
          'Minimum',
          'Percentile 25',
          'Median',
          'Mean',
          'Percentile 75',
          'Maximum',
          paste('Percentile', input$pA_var_percentile),
          'Standard Deviation',
          'Pearson Correlation'
        ),
        value = c(
          pA_stats_n_nas() / pA_stats_obs() * 100,
          pA_stats_min(),
          pA_stats_q1(),
          pA_stats_median(),
          pA_stats_mean(),
          pA_stats_q3(),
          pA_stats_max(),
          pA_var_percentile(),
          pA_stats_sd(),
          pA_stats_correlation()
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

    output$pA_t1 <- renderDT(pA_t1() |>
                                formatCurrency(
                                  'value',
                                  digits = input$pA_t1_digits,
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
