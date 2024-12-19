
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
#' @importFrom utils object.size head
#' @importFrom graphics boxplot lines barplot mtext text
#' @importFrom stats cor lm sd var

dataviz <- function(dataset) {
  stopifnot(is.data.frame(dataset))
  dataset_classes <- class(dataset)
  setDT(dataset)
  gc()

  fn_empty_plot <- function(msg = 'No plot', c = 2){
    plot(1:10, 1:10, type = 'n', xlab = '', ylab = '')
    text(5, 5, msg, cex = c)
  }

  filter_operators <- c('== (Equal)' = '==',
                        '!= (Not Equal)' = '!=',
                        '> (Greater)' = '>',
                        '>= (Greater or Equal)' = '>=',
                        '< (Less)' = '<',
                        '<= (Less or Equal)' = '<=',
                        'Is NA (is.na)' = 'is_na',
                        'Not NA (! is.na)' = 'not_na',
                        'In (%in%)' = 'in',
                        'Not In (! %in%)' = 'not_in',
                        'Between' = 'between',
                        'Not Between' = 'not_between')

  # close browser tab
  js_exit <- "Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"

  ### -------------------------------------------------------------------------
  # UI
  ### -------------------------------------------------------------------------
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
      card(full_screen = T,
           card_body(
             style = 'background-color: #02517d;',
             navset_card_tab(
               height = '800px',
               nav_panel(
                 'Highlights',
                 card_body(
                   layout_column_wrap(
                     value_box(
                       title = 'Numeric Vars',
                       value = sapply(dataset, is.numeric) |> sum(),
                       showcase = bs_icon('123'),
                       theme = 'bg-gradient-yellow-orange'
                     ),
                     value_box(
                       title = 'Character Vars',
                       value = sapply(dataset, is.character) |> sum(),
                       showcase = bs_icon('alphabet'),
                       theme = 'bg-gradient-blue-indigo'
                     ),
                     value_box(
                       title = 'Factor Vars',
                       value = sapply(dataset, is.factor) |> sum(),
                       showcase = bs_icon('diagram-3'),
                       theme = 'bg-gradient-green-indigo'
                     ),
                     value_box(
                       title = 'Date Vars',
                       value = sapply(
                         dataset,
                         \(x) inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))) |>
                         sum(),
                       showcase = bs_icon('calendar3'),
                       theme = 'bg-gradient-purple-indigo'
                     )
                   ),
                   layout_column_wrap(
                     value_box(
                       title = "Var with most NA's",
                       value = textOutput('pS_var_most_nas'),
                       showcase = bs_icon('database-x'),
                       theme = 'bg-gradient-red-indigo',
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
                       theme = 'bg-gradient-pink-indigo',
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
                       theme = 'bg-gradient-teal-indigo',
                       p(textOutput('pS_var_biggest_size_size', inline = T), 'kB')
                     ) |> tooltip("Showing 1, there may be ties.", placement = 'top')
                   )
                 )
               ),
               nav_panel('Variables', card_body(DTOutput('pS_var_t'))),
               nav_panel(
                 'Overview',
                 card_body(
                   radioButtons('pS_over_radio_sample', NULL,
                                c('First' = 'first', 'Sample' = 'sample'),
                                inline = T),
                   numericInput('pS_over_size_sample', NULL, 100, 100, 1e4, 100),
                   DTOutput('pS_over_t'))
               ),
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
      card(full_screen = T,
           card_body(
             style = 'background-color: #02517d;',
             layout_columns(
               height = '800px',
               navset_card_tab(
                 nav_panel(
                   'Filter',
                   layout_column_wrap(
                     card(
                       card_header('Filter Rows', class = 'bg-primary'),
                       card_body(
                         uiOutput('pE_filter_ui_var_filter'),
                         selectInput('pE_filter_operator',
                                     'Operator', c('', filter_operators)),
                         selectizeInput(
                           'pE_filter_value', 'Value',
                           choices = NULL,
                           multiple = T,
                           options = list(create = T)
                         ) |>
                           tooltip('Text should not be in quotes'),
                         verbatimTextOutput('pE_filter_txt_preview', placeholder = T)),
                       actionButton('pE_filter_btn_filter', 'Apply filters', icon('check'))
                     ),
                     card(
                       card_header('Select Columns', class = 'bg-primary'),
                       card_body(
                         uiOutput('pE_filter_ui_var_sel'),
                         radioButtons('pE_filter_radio_var_sel', NULL,
                                      c('Drop' = 'drop', 'Keep' = 'keep'), inline = T),
                         actionButton('pE_filter_btn_sel', 'Apply selection', icon('check')))
                     )
                   ),
                 ),
                 nav_panel(
                   'Convert',
                   layout_column_wrap(
                     card(
                       card_header('Conversions', class = 'bg-primary'),
                       uiOutput('pE_convert_ui_var_sel'),
                       p('Current Type / Class'),
                       verbatimTextOutput('pE_convert_current_format', placeholder = T),
                       selectInput('pE_convert_sel_format', 'Select the new format',
                                   c('', 'as.numeric', 'as.integer',
                                     'as.character', 'as.Date', 'as.factor',
                                     'as.double', 'as.raw', 'as.complex')),
                       actionButton('pE_convert_btn_apply', 'Apply conversion', icon('hammer'))
                     ),
                     card(
                       card_header('Preview', class = 'bg-primary'),
                       actionButton('pE_convert_btn_preview_sample', 'Show new sample', icon('rotate-right')),
                       DTOutput('pE_convert_preview_t1')
                     )
                   )
                 ),
                 nav_panel(
                   'Variables',
                   card_body(
                     DTOutput('pE_var_t')),
                   actionButton('pE_convert_btn_var_update', 'Update', icon('rotate-right'))
                 ),
                 nav_panel(
                   'Overview',
                   card_body(
                     radioButtons('pE_over_radio_sample', NULL,
                                  c('First' = 'first', 'Sample' = 'sample'),
                                  inline = T),
                     numericInput('pE_over_size_sample', NULL, 100, 100, 1e4, 100),
                     DTOutput('pE_t1'))),
                 nav_panel(
                   'Export',
                   layout_column_wrap(
                     card(
                       layout_column_wrap(
                         textInput('pE_export_file_name', 'File name', value = 'dataset'),
                         radioButtons('pE_export_radio_format', 'File format',
                                      c('csv', 'RDS', 'RDS Compressed'), inline = T)
                       ),
                       card(
                         card_header('Csv Parameters', class = 'bg-primary'),
                         checkboxInput('pE_export_x_rownames', 'Save row names'),
                         layout_column_wrap(
                           radioButtons('pE_export_radio_separator', 'Separator',
                                        c('Comma' = ',', 'Semicolon' = ';'), inline = T),
                           radioButtons('pE_export_radio_decimal', 'Decimal Mark',
                                        c('Dot' = '.', 'Comma' = ','), inline = T)
                         ),
                         layout_column_wrap(
                           textInput('pE_export_txt_na', 'Missing (NA) substitute', value = ''),
                           radioButtons('pE_export_radio_scientific', 'Scientific Notation',
                                        c('No' = 999999999, 'Allow' = 0), inline = T)
                         )
                       ),
                       downloadButton('pE_export_down', 'Export Active Dataset', icon('download'))
                     )
                   )
                 )
               )
             )

           ),
           card_footer(
             layout_column_wrap(
               actionButton('pE_btn_reset', 'Reset Dataset', icon('arrow-rotate-right'),
                            style = '--bs-btn-bg: #f2f2f2;') |>
                 tooltip('Restore the original dataset to the Active dataset', placement = 'top'),
               actionButton('pE_export_btn_bkp', 'Create Backup', icon('cloud-arrow-up'),
                            style = '--bs-btn-bg: #f2f2f2;') |>
                 tooltip('Create a copy of the Active dataset', placement = 'top'),
               actionButton('pE_export_btn_restore', 'Restore Backup', icon('cloud-arrow-down'),
                            style = '--bs-btn-bg: #f2f2f2;') |>
                 tooltip('Restore a previously created backup', placement = 'top'),
               actionButton('pE_export_btn_clear_bkp', 'Clear Backup', icon('trash'),
                            style = '--bs-btn-bg: #f2f2f2;margin: 0px') |>
                 tooltip('Erase the backup', placement = 'top'),
             ),
             style = 'background-color: #02517d;')
      )
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
                uiOutput('pA_ui_var_names'),
                uiOutput('pA_ui_var_names2')
              ),
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
                  layout_columns(
                    col_widths = c(8, 2, 2),
                    radioButtons('pA_radio_dist_plot', 'Plot type:',
                                 c('Dots' = 'dots',
                                   'Histogram' = 'hist',
                                   'Boxplot' = 'boxplot',
                                   'Boxplot by Groups' = 'boxplot_group',
                                   'Barplot' = 'barplot'), inline = T),
                    numericInput('pA_var_percentile', 'Percentile', 50, 0, 100, 5),
                    numericInput('pA_bins', 'Bins', 10, 5, step = 10) |>
                      tooltip('Only for Histrograms', placement = 'top')
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
                    actionButton('pA_btn_scatter', 'Generate Plot', icon('gear'))
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
                    layout_column_wrap(
                      actionButton('pA_btn_scatter_lm_run', 'Run Linear Model', icon('gear')),
                      actionButton('pA_btn_scatter_lm_clear', 'Clear Linear Model', icon('trash'))
                    )
                  ),
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
      tags$head(tags$script(HTML(js_exit))),

      # config style
      tags$head(
        tags$style(HTML(
          "
          /* change color of navbar */
          .navbar {
            background: linear-gradient(to right, #1d3f52, #033854, #02517d, #317aa3);
          }

          /* change size of nav panel */
          .nav-link {font-size: 18px; }

          "))
      )
    )
  )

  ### -------------------------------------------------------------------------
  # Server
  ### -------------------------------------------------------------------------
  server <- function(input, output, session) {
    df <- reactiveValues(
      df = copy(dataset),
      df_active = copy(dataset),
      df_classes = dataset_classes,
      df_backup = NULL
    )
    gc()
    df_active_names <- reactive(df$df_active |> names())

    # main value boxes ---------------------------------------------------------
    fn_main_value_box <- function(df, df_name = 'Original', df_classes = class(df)){
      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = paste(df_name, 'Dataset Class'),
            value = df_classes |> head(1),
            showcase = bs_icon('file-binary'),
            theme = 'bg-gradient-blue-indigo'
          ),
          value_box(
            title = 'Rows / Columns',
            value = paste(
              nrow(df) |> f_num(dec = '.', big = ','),
              '/',
              ncol(df) |> f_num(dec = '.', big = ',')
            ),
            showcase = bs_icon('layout-text-sidebar-reverse'),
            theme = 'bg-gradient-blue-indigo'
          ),
          value_box(
            title = "Columns with NA's",
            value = sum(colSums(is.na(df)) > 0),
            showcase = bs_icon("database-x"),
            theme = 'bg-gradient-blue-indigo'
          ),
          value_box(
            title = 'Size (MB)',
            value = (object.size(df) / 2^20) |> as.numeric() |> round(2),
            showcase = bs_icon('sd-card'),
            theme = 'bg-gradient-blue-indigo'
          )
        )
      )
    }

    output$pS_value_box <- renderUI({ fn_main_value_box(df$df, df_classes = df$df_classes) })

    main_value_box_active <- reactive(fn_main_value_box(df$df_active, 'Active'))
    output$pE_value_box <- renderUI({ main_value_box_active() })
    output$pA_value_box <- renderUI({ main_value_box_active() })

    # summary page events -----------------------------------------------------
    pS_var_t <- reactive(
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
        perc_nas = sapply(df$df, \(x) length(x[is.na(x)])) / sapply(df$df, length))
      )

    output$pS_var_t <- renderDT(
      pS_var_t() |>
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
              list(targets = 0, width = '400px', className = 'dt-left'),
              list(targets = 1:2, width = '200px', className = 'dt-left'),
              list(targets = 3:7, width = '100px', className = 'dt-right')
            )
          )
        ) |>
        formatCurrency(
          c('size', 'min', 'max', 'n_nas'),
          digits = 2,
          currency = ''
        ) |>
        formatPercentage('perc_nas', digits = 2) |>
        formatStyle(
          'type',
          fontWeight = 'bold',
          backgroundColor = styleEqual(
            c('double', 'integer', 'character', 'logical', 'complex', 'raw'),
            c(rep('#fcc932', 2), '#75bbf5', '#eba881' , rep('#be6d81', 2))
          )
        ) |>
        formatStyle(
          'size',
          background = styleColorBar(range(pS_var_t()$size) + range(pS_var_t()$size)/100 * c(-1, 1), '#00bf7f'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'min',
          background = styleColorBar(
            range(pS_var_t()$min[!is.na(pS_var_t()$min)]) + range(pS_var_t()$min[!is.na(pS_var_t()$min)])/100 * c(-1, 1), '#d867b2'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'max',
          background = styleColorBar(
            range(pS_var_t()$max[!is.na(pS_var_t()$max)]) + range(pS_var_t()$max[!is.na(pS_var_t()$max)])/100 * c(-1, 1), '#bf007f'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'n_nas',
          background = styleColorBar(range(pS_var_t()$n_nas) + range(pS_var_t()$n_nas)/100 * c(-1, 1), '#b62020'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'perc_nas',
          background = styleColorBar(c(-0.001, 1.05), '#919191'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top')
    )

    # overview -----------------------
    output$pS_over_t <- renderDT(
      {
        req(input$pS_over_size_sample)
        pS_over_n_show <- max(1, input$pS_over_size_sample)
        pS_over_n_show <- min(pS_over_n_show, nrow(df$df))

        if(input$pS_over_radio_sample == 'first'){
          pS_over_idx <- 1:pS_over_n_show
        } else if (input$pS_over_radio_sample == 'sample'){
          pS_over_idx <- sample.int(nrow(df$df), pS_over_n_show, replace = F)
        }

        df$df[pS_over_idx, ] |>
          datatable(
            extensions = 'ColReorder',
            rownames = F,
            options = list(dom = 'Bftp', pageLength = 5, colReorder = T,
                           columnDefs = list(list(targets = '_all', className = 'dt-right')))
          )
      }
    )

    # value for boxes -----------------------
    output$pS_var_most_nas <- renderText(
      {
        if(pS_var_t() |> filter(n_nas > 0) |> nrow() < 1) { 'None'
        } else {
          pS_var_t() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$pS_var_most_nas_n <- renderText(
      {
        if(pS_var_t() |> filter(n_nas > 0) |> nrow() < 1) { '0'
        } else {
          pS_var_t() |> filter(n_nas > 0)|> arrange(-n_nas, -perc_nas) |>
            head(1) |> pull(n_nas) |> f_num()}
      }
    )

    output$pS_var_biggest_perc_nas <- renderText(
      {
        if(pS_var_t() |> filter(perc_nas > 0) |> nrow() < 1) { 'None'
        } else {
          pS_var_t() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(var) }
      }
    )

    output$pS_var_biggest_perc_nas_perc <- renderText(
      {
        if(pS_var_t() |> filter(perc_nas > 0) |> nrow() < 1) { '0'
        } else {
          pS_var_t() |> filter(perc_nas > 0)|> arrange(-perc_nas, -n_nas) |>
            head(1) |> pull(perc_nas) * 100 }
      }
    )

    output$pS_var_max_value <- renderText(
      pS_var_t() |> arrange(-max) |> head(1) |> pull(var)
    )

    output$pS_max_value <- renderText(
      pS_var_t() |> arrange(-max) |> head(1) |> pull(max) |> f_num(dig = 3)
    )

    output$pS_var_min_value <- renderText(
      pS_var_t() |> arrange(min) |> head(1) |> pull(var)
    )

    output$pS_min_value <- renderText(
      pS_var_t() |> arrange(min) |> head(1) |> pull(min) |> f_num(dig = 3)
    )

    output$pS_var_biggest_size <- renderText(
      pS_var_t() |> arrange(-size) |> head(1) |> pull(var)
    )

    output$pS_var_biggest_size_size <- renderText(
      pS_var_t() |> arrange(-size) |> head(1) |> pull(size) |> round(2)
    )

    # edit page events -------------------------------------------------------
    pE_var_t <- reactive(
      data.frame(
        var = names(df$df_active),
        type = lapply(df$df_active, typeof) |> unlist(),
        class = lapply(df$df_active, \(x) class(x) |> paste(collapse = '/')) |> unlist(),
        size = (lapply(df$df_active, object.size) |> unlist()) / 2^10,
        min = lapply(df$df_active, \(x) if (is.numeric(x))
          mina(x)
          else
            NA) |> unlist(),
        max = lapply(df$df_active, \(x) if (is.numeric(x))
          mana(x)
          else
            NA) |> unlist(),
        n_nas = sapply(df$df_active, \(x) length(x[is.na(x)])),
        perc_nas = sapply(df$df_active, \(x) length(x[is.na(x)])) / sapply(df$df_active, length))
    ) |> bindEvent(input$pE_convert_btn_var_update)

    output$pE_var_t <- renderDT({
      req(pE_var_t())
      pE_var_t() |>
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
            pageLength = 6,
            colReorder = T,
            columnDefs = list(
              list(targets = 0, width = '400px', className = 'dt-left'),
              list(targets = 1:2, width = '200px', className = 'dt-left'),
              list(targets = 3:7, width = '100px', className = 'dt-right')
            )
          )
        ) |>
        formatCurrency(
          c('size', 'min', 'max', 'n_nas'),
          digits = 2,
          currency = ''
        ) |>
        formatPercentage('perc_nas', digits = 2) |>
        formatStyle(
          'type',
          fontWeight = 'bold',
          backgroundColor = styleEqual(
            c('double', 'integer', 'character', 'logical', 'complex', 'raw'),
            c(rep('#fcc932', 2), '#75bbf5', '#eba881' , rep('#be6d81', 2))
          )
        ) |>
        formatStyle(
          'size',
          background = styleColorBar(range(pE_var_t()$size) + range(pE_var_t()$size)/100 * c(-1, 1), '#00bf7f'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'min',
          background = styleColorBar(
            range(pE_var_t()$min[!is.na(pE_var_t()$min)]) + range(pE_var_t()$min[!is.na(pE_var_t()$min)])/100 * c(-1, 1), '#d867b2'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'max',
          background = styleColorBar(
            range(pE_var_t()$max[!is.na(pE_var_t()$max)]) + range(pE_var_t()$max[!is.na(pE_var_t()$max)])/100 * c(-1, 1), '#bf007f'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'n_nas',
          background = styleColorBar(range(pE_var_t()$n_nas) + range(pE_var_t()$n_nas)/100 * c(-1, 1), '#b62020'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top') |>
        formatStyle(
          'perc_nas',
          background = styleColorBar(c(-0.001, 1.05), '#919191'),
          backgroundSize = '100% 20%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'top')
      }
    )

    # filter events ---------------------------
    output$pE_filter_ui_var_filter <- renderUI(
      selectInput('pE_filter_vars_filter', 'Variable', c('', df_active_names()))
    )

    output$pE_filter_ui_var_sel <- renderUI(
      selectInput('pE_filter_vars_sel', 'Variable', c('', df_active_names()), multiple = T)
    )

    pE_filter_value_temp <- reactive({
      if(df$df_active[[input$pE_filter_vars_filter]] |> is.numeric()){
        unlist(input$pE_filter_value) |> as.numeric()
      } else if (inherits(df$df_active[[input$pE_filter_vars_filter]], 'Date')){
        unlist(input$pE_filter_value) |> as.Date()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is.raw()){
        unlist(input$pE_filter_value) |> as.raw()
      } else if (df$df_active[[input$pE_filter_vars_filter]] |> is.complex()){
        unlist(input$pE_filter_value) |> as.complex()
      } else {
        input$pE_filter_value
      }
    })

    # update selectinput to show pertinent operators
    observe({
      updateSelectInput(
        session, 'pE_filter_operator',
        label = 'Operator',
        choices =
          if(df$df_active[[input$pE_filter_vars_filter]] |> is.factor() |
             df$df_active[[input$pE_filter_vars_filter]] |> is.character()){
            c('',
              '== (Equal)' = '==',
              '!= (Not Equal)' = '!=',
              'Is NA (is.na)' = 'is_na',
              'Not NA (! is.na)' = 'not_na',
              'In (%in%)' = 'in',
              'Not In (! %in%)' = 'not_in')
          } else { c('', filter_operators) }
      )
    }) |> bindEvent(input$pE_filter_vars_filter)

    output$pE_filter_txt_preview <- renderPrint({
      req(input$pE_filter_value)
      cat("Preview value: ")
      print(pE_filter_value_temp())
    })

    # filter rows
    observe({
      if(length(pE_filter_value_temp()) > 1 & input$pE_filter_operator %in%
         c('==', '!=', '>', '>=', '<', '<=')){
        showNotification('Operator requires value of length 1', type = 'error')
      } else if(length(pE_filter_value_temp()) != 2 & input$pE_filter_operator %in%
                 c('between', 'not_between')){
        showNotification('Operator requires value of length 2', type = 'error')
      } else {
        if(input$pE_filter_operator == '=='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) == pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '!='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) != pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '>'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) > pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '>='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) >= pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '<'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) < pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == '<='){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) <= pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'is_na'){
          df$df_active <-
            df$df_active[is.na(get(input$pE_filter_vars_filter)), ]
        } else if(input$pE_filter_operator == 'not_na'){
          df$df_active <-
            df$df_active[!is.na(get(input$pE_filter_vars_filter)), ]
        } else if(input$pE_filter_operator == 'in'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) %in% pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'not_in'){
          df$df_active <-
            df$df_active[!get(input$pE_filter_vars_filter) %in% pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'between'){
          df$df_active <-
            df$df_active[get(input$pE_filter_vars_filter) %between% pE_filter_value_temp(), ]
        } else if(input$pE_filter_operator == 'not_between'){
          df$df_active <-
            df$df_active[!(get(input$pE_filter_vars_filter) %between% pE_filter_value_temp()), ]
        }

        showNotification('Filter rows: OK', type = 'message', duration = 2)
      }
    }) |> bindEvent(input$pE_filter_btn_filter)

    # clear value after click in button
    observe({
      updateSelectizeInput(
        session,
        'pE_filter_value',
        label = 'Value updated',
        choices = NULL,
        selected = ''
      )
    }) |> bindEvent(input$pE_filter_btn_filter)

    # select cols ---------------------------
    observe({
      if(input$pE_filter_radio_var_sel == 'keep') {
        if(input$pE_filter_vars_sel |> length() == 0){
          showNotification('Select at least 1 variable', type = 'message', duration = 2)
        } else {
          df$df_active <- subset(df$df_active, select = input$pE_filter_vars_sel)
          showNotification('Select columns: OK', type = 'message', duration = 2)
        }
      } else if (input$pE_filter_radio_var_sel == 'drop'){
        if(identical(input$pE_filter_vars_sel, names(df$df_active))){
          showNotification('Leave at least 1 variable', type = 'message', duration = 2)
        } else {
          df$df_active <- subset(df$df_active,
                                 select = setdiff(names(df$df_active), input$pE_filter_vars_sel))
          showNotification('Select columns: OK', type = 'message', duration = 2)
        }
      }
    }) |> bindEvent(input$pE_filter_btn_sel)

    # convert events ---------------------------
    output$pE_convert_ui_var_sel <- renderUI(
      selectInput('pE_convert_vars_sel', 'Variable', c('', df_active_names()))
    )

    output$pE_convert_current_format <- renderPrint(
      {
        req(input$pE_convert_vars_sel)
        paste('Type: [', df$df_active[[input$pE_convert_vars_sel]] |> typeof(), '] |',
              'Class: [',
              paste(df$df_active[[input$pE_convert_vars_sel]] |> class(), collapse = '/'),
              ']')
      }
    )

    # sample to preview conversion
    pE_convert_preview_sample_trigger <- reactiveVal(1)
    pE_convert_preview_sample <- reactive({
      pE_convert_preview_sample_trigger()
      if(nrow(df$df_active) < 5) {
        rep(TRUE, nrow(df$df_active))
      } else {
        sample(nrow(df$df_active), 5, replace = F)
      }
    }) 
    
    # update sample in button click
    observe({
      pE_convert_preview_sample_trigger(pE_convert_preview_sample_trigger() + 1)
    }) |> bindEvent(input$pE_convert_btn_preview_sample)

    pE_convert_preview_df <- reactive({
      req(input$pE_convert_vars_sel)
      req(input$pE_convert_sel_format)

      pE_convert_preview_df_temp <- subset(
        df$df_active[pE_convert_preview_sample(), ],
        select = input$pE_convert_vars_sel)
      
      if(input$pE_convert_sel_format == 'as.numeric'){
        pE_convert_preview_df_temp[, preview := as.numeric(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.integer'){
        pE_convert_preview_df_temp[, preview := as.integer(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.character'){
        pE_convert_preview_df_temp[, preview := as.character(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.Date'){
        pE_convert_preview_df_temp[, preview := as.Date(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.factor'){
        pE_convert_preview_df_temp[, preview := as.factor(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.double'){
        pE_convert_preview_df_temp[, preview := as.double(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.raw'){
        pE_convert_preview_df_temp[, preview := as.raw(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.complex'){
        pE_convert_preview_df_temp[, preview := as.complex(get(input$pE_convert_vars_sel))]
      }
    })

    output$pE_convert_preview_t1 <- renderDT({      
      datatable(pE_convert_preview_df(),
                rownames = F,
                options = list(dom = 'Bt', pageLength = 5,
                               columnDefs = list(
                               list(targets = 0:1, width = '200px', className = 'dt-center')))
          )
    })
   
    observe({
      if(input$pE_convert_sel_format == 'as.numeric'){
        df$df_active[, input$pE_convert_vars_sel := as.numeric(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.integer'){
        df$df_active[, input$pE_convert_vars_sel := as.integer(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.character'){
        df$df_active[, input$pE_convert_vars_sel := as.character(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.Date'){
        df$df_active[, input$pE_convert_vars_sel := as.Date(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.factor'){
        df$df_active[, input$pE_convert_vars_sel := as.factor(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.double'){
        df$df_active[, input$pE_convert_vars_sel := as.double(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.raw'){
        df$df_active[, input$pE_convert_vars_sel := as.raw(get(input$pE_convert_vars_sel))]
      } else if(input$pE_convert_sel_format == 'as.complex'){
        df$df_active[, input$pE_convert_vars_sel := as.complex(get(input$pE_convert_vars_sel))]
      }
      showNotification('Conversion applied', type = 'message')
    }) |> bindEvent(input$pE_convert_btn_apply)

    # reset df active ---------------------------
    observe({
      df$df_active <- copy(df$df)
      gc()
      showNotification('Active Dataset Reseted', type = 'message', duration = 2)
    }) |> bindEvent(input$pE_btn_reset)

    # create backup ---------------------------
    observe({
      df$df_backup <- copy(df$df_active)
      gc()
      showNotification('Backup created', type = 'message', duration = 2)
    }) |> bindEvent(input$pE_export_btn_bkp)

    # restore backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        showNotification('No backup to restore', type = 'message', duration = 2)
      } else {
        df$df_active <- copy(df$df_backup)
        gc()
        showNotification('Backup restored', type = 'message', duration = 2)
      }
    }) |> bindEvent(input$pE_export_btn_restore)

    # clear backup ---------------------------
    observe({
      if(is.null(df$df_backup)){
        showNotification('No backup to clear', type = 'message', duration = 2)
      } else {
        df$df_backup <- NULL
        gc()
        showNotification('Backup cleared', type = 'message', duration = 2)
      }
    }) |> bindEvent(input$pE_export_btn_clear_bkp)

    # overview ----------------------------------
    output$pE_t1 <- renderDT(
      {
        req(input$pE_over_size_sample)
        pE_over_n_show <- max(1, input$pE_over_size_sample)
        pE_over_n_show <- min(pE_over_n_show, nrow(df$df_active))

        if(input$pE_over_radio_sample == 'first'){
          pE_over_idx <- 1:pE_over_n_show
        } else if (input$pE_over_radio_sample == 'sample'){
          pE_over_idx <- sample.int(nrow(df$df_active), pE_over_n_show, replace = F)
        }

        df$df_active[pE_over_idx, ] |>
          datatable(
            extensions = 'ColReorder',
            rownames = F,
            options = list(dom = 'Bftp', pageLength = 5, colReorder = T,
                           columnDefs = list(list(targets = '_all', className = 'dt-right')))
          )
      }
    )

    # export ----------------------------------------------------
    output$pE_export_down <- downloadHandler(

      filename = function() {
        paste(input$pE_export_file_name,
              if(input$pE_export_radio_format == 'csv'){
                '.csv'
              } else if (input$pE_export_radio_format == 'RDS' |
                         input$pE_export_radio_format == 'RDS Compressed'){
                '.RDS'
              })
      },
      content = function(file) {
        if(input$pE_export_radio_format == 'csv'){
          fwrite(df$df_active, file,
                 row.names = input$pE_export_x_rownames,
                 sep = input$pE_export_radio_separator,
                 dec = input$pE_export_radio_decimal,
                 na = input$pE_export_txt_na,
                 scipen = as.integer(input$pE_export_radio_scientific)
          )
        } else if (input$pE_export_radio_format == 'RDS'){
          saveRDS(df$df_active, file, compress = F)
        } else if (input$pE_export_radio_format == 'RDS Compressed') {
          saveRDS(df$df_active, file, compress = T)
        }
      }
    )

    # analysis page events ----------------------------------------------------
    output$pA_ui_var_names <- renderUI(
      selectInput('pA_sel_vars', 'Main Variable', df_active_names()) |>
        tooltip('Dependent Variable', placement = 'top')
    )

    output$pA_ui_var_names2 <- renderUI(
      selectInput('pA_sel_vars2', 'Variable 2', df_active_names(), df_active_names()[2]) |>
        tooltip('Independent Variable', placement = 'top')
    )

    pA_outliers_index <- reactive({
      v <- df$df_active[[input$pA_sel_vars]]
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
      req(input$pA_sel_vars)
      df$df_active[[input$pA_sel_vars]][pA_outliers_index()]
    })

    pA_var2 <- reactive({
      req(input$pA_sel_vars2)
      df$df_active[[input$pA_sel_vars2]][pA_outliers_index()]
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
          fn_empty_plot('Value must be numeric')
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
          fn_empty_plot('Value must be numeric')
        } else {
          boxplot(pA_var(), horizontal = T, col = 'steelblue2')
          abline(v = pA_var_percentile(), col = 'brown3')
        }
      } else if (input$pA_radio_dist_plot == 'boxplot_group'){
        if(!is.numeric(pA_var())){
          fn_empty_plot('Value must be numeric')
        } else {
          if((unique(pA_var2()) |> length()) > 5){
            fn_empty_plot('Too many groups (max 5)')
          } else {

            pA_g_dist_boxg_col <- colors()[sample.int(
              colors() |> length(), unique(pA_var2()) |> length(), replace = F)]

            boxplot(pA_var() ~ pA_var2(), horizontal = T,
                    col = pA_g_dist_boxg_col, xlab = '', ylab = '')
            abline(v = pA_var_percentile(), col = 'brown3')
          }
        }
      } else if (input$pA_radio_dist_plot == 'dots'){
        if(!is.numeric(pA_var())){
          fn_empty_plot('Value must be numeric')
        } else {
          plot(pA_var(), col = 'steelblue2', ylab = 'Values')
          abline(h = pA_var_percentile(), col = 'brown3')
        }
      } else if (input$pA_radio_dist_plot == 'barplot'){
        if(!is.numeric(pA_var())){
          barplot(table(pA_var()), col = 'steelblue2')
        } else {
          fn_empty_plot('Value can not be numeric')
        }
      }
    }) |> bindCache(pA_var(), pA_var2(), input$pA_radio_dist_plot, input$pA_bins,
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
      if(!is.numeric(pA_var())){
        showNotification('The Dependent variable must be numeric', duration = 2.5, type = 'message')
      } else if (input$pA_sel_vars == input$pA_sel_vars2) {
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
        fn_empty_plot('No residuals to plot')
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
        gc()
        session$sendCustomMessage(type = 'closeWindow', message = 'message')
        stopApp()
      }
    })
  } # end of server function

  ### Run App -----------------------------------------------------------------
  shinyApp(ui, server, options = list(launch.browser = T))
}