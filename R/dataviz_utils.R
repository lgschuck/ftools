
# info abaout dataset variables -------------------------------------------
df_info <- function(df){
  data.frame(
    var = names(df),
    type = sapply(df, typeof),
    class = sapply(df, \(x) class(x) |> paste(collapse = '/')),
    size = sapply(df, object.size) / 2^10,
    min = sapply(df, \(x) if (is.numeric(x)) mina(x) else NA),
    max = sapply(df, \(x) if (is.numeric(x)) mana(x) else NA),
    n_valid = sapply(df, \(x) length(x[!is.na(x)])),
    perc_valid = sapply(df, \(x) length(x[!is.na(x)])) / sapply(df, length),
    n_nas = sapply(df, \(x) length(x[is.na(x)])),
    perc_nas = sapply(df, \(x) length(x[is.na(x)])) / sapply(df, length)
  )
}

# format bars in DT -------------------------------------------------------
format_color_bar <- function(DF, NAME, VALUES, COLOR){

  VALUES <- if(VALUES |> length() == 0) 0 else VALUES
  DT::formatStyle(
    table = DF,
    columns = NAME,
    background = DT::styleColorBar(data = range(VALUES) * c(-1.1, 1.1), color = COLOR),
    backgroundSize = '100% 20%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'top')
}

# print DT of df_info -----------------------------------------------------
df_info_print <- function(df){
  df |>
    DT::datatable(
      extensions = 'ColReorder',
      rownames = F,
      colnames = c('Variable', 'Type', 'Class', 'Size (kB)', 'Min',
                   'Max', 'Valid', '% Valid', "NA's", "% NA's"),
      options = list(
        dom = 'Bftp',
        pageLength = 10,
        colReorder = T,
        columnDefs = list(
          list(targets = 0, width = '300px', className = 'dt-left'),
          list(targets = 1:2, width = '200px', className = 'dt-left'),
          list(targets = 3:9, width = '100px', className = 'dt-right')
        )
      )
    ) |>
    DT::formatCurrency(c('size', 'min', 'max', 'n_valid', 'n_nas'), digits = 2, currency = '') |>
    DT::formatPercentage(c('perc_valid', 'perc_nas'), digits = 2) |>
    DT::formatStyle(
      'type',
      fontWeight = 'bold',
      backgroundColor = DT::styleEqual(
        c('double', 'integer', 'character', 'logical', 'complex', 'raw'),
        c(rep('#fcc932', 2), '#75bbf5', '#eba881' , rep('#be6d81', 2))
      )
    ) |>
    format_color_bar('size', df$size, '#00bf7f') |>
    format_color_bar('min', df$min[!is.na(df$min)], '#d867b2') |>
    format_color_bar('max', df$max[!is.na(df$max)], '#bf007f') |>
    format_color_bar('n_valid', df$n_valid[!is.na(df$n_valid)], '#0cb0a8') |>
    format_color_bar('n_nas', df$n_nas, '#b62020') |>
    DT::formatStyle(
      'perc_valid',
      background = DT::styleColorBar(c(-0.001, 1.05), '#05a17c'),
      backgroundSize = '100% 20%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'top') |>
    DT::formatStyle(
      'perc_nas',
      background = DT::styleColorBar(c(-0.001, 1.05), '#919191'),
      backgroundSize = '100% 20%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'top')
}

# empty plot function -----------------------------------------------------
fn_empty_plot <- function(msg = 'No plot', c = 2){
  plot(1:10, 1:10, type = 'n', xlab = '', ylab = '')
  text(5, 5, msg, cex = c)
}

# bslib btn task ----------------------------------------------------------
btn_task <- function(ID, LABEL, ICON = NULL){
  bslib::input_task_button(id = ID, label = LABEL, icon = ICON, class = 'btn-task')
}

# function to generate value boxes on top of pages ------------------------
fn_main_value_box <- function(df, df_name = 'Original', df_classes = class(df)){
  tagList(
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box(
        title = paste(df_name, 'Dataset Class'),
        value = df_classes |> head(1),
        showcase = bsicons::bs_icon('file-binary'),
        theme = 'bg-gradient-blue-indigo',
        class = 'main-value-box'
      ),
      bslib::value_box(
        title = 'Rows / Columns',
        value = paste(nrow(df) |> f_num(dec = '.', big = ','), '/',
                      ncol(df) |> f_num(dec = '.', big = ',')),
        showcase = bsicons::bs_icon('layout-text-sidebar-reverse'),
        theme = 'bg-gradient-blue-indigo',
        class = 'main-value-box'
      ),
      bslib::value_box(
        title = "Columns with NA's",
        value = sum(colSums(is.na(df)) > 0),
        showcase = bsicons::bs_icon("database-x"),
        theme = 'bg-gradient-blue-indigo',
        class = 'main-value-box'
      ),
      bslib::value_box(
        title = 'Size (MB)',
        value = (object.size(df) / 2^20) |> as.numeric() |> round(2),
        showcase = bsicons::bs_icon('sd-card'),
        theme = 'bg-gradient-blue-indigo',
        class = 'main-value-box'
      )
    )
  )
}

# messages - shownotification ---------------------------------------------

msg <- function(TEXT, DURATION = 2){
  showNotification(ui = TEXT, duration = DURATION, type = 'message')
}

msg_error <- function(TEXT, DURATION = 2){
  showNotification(ui = TEXT, duration = DURATION, type = 'error')
}

# is date function --------------------------------------------------------
is_date <- function(x){
  inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))
}

# try convert -------------------------------------------------------------
convert <- function(x, fun){
  tryCatch(fun(x),
           error = function(e) rep(NA, x |> length()),
           warning = function(w) rep(NA, x |> length()))
}

# format conversion ----------------------------------------------------------
fn_convert <- function(x, type, date_format = '%Y-%m-%d', date_origin = '1970-01-01'){

  if(x |> is.raw()) x <- as.numeric(x)

  if(type == 'as.numeric'){
    if(x |> is.character()) rep(NA, length(x)) else as.numeric(x)
  } else if(type == 'as.integer'){
    if(x |> is.character()) rep(NA, length(x)) else as.integer(x)
  } else if(type == 'as.character'){
    as.character(x)
  } else if(type == 'as.Date'){
    if(is.numeric(x)){
      as.Date(x, origin = date_origin)
    } else if (is.raw(x) || is.complex(x)){
      as.Date(x |> as.numeric(), origin = date_origin)
    } else as.Date(x, format = date_format)
  } else if(type == 'as.factor'){
    as.factor(x)
  } else if(type == 'as.double'){
    if(x |> is.character()) rep(NA, length(x)) else as.double(x)
  # } else if(type == 'as.raw'){
  #   as.raw(x)
  } else if(type == 'as.complex'){
    if(x |> is.character()) rep(NA, length(x)) else as.complex(x)
  }
}
