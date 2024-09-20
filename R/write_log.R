#' Function for write messages in text file
#' @export write_log
#'
#' @param file log file
#' @param message message to be saved in the log file
#'
#' @examples
#'
#' write_log('A Simple Test.', file = 'log_file.txt')
#' readLines('log_file.txt')
#'
write_log <- function(message = '', file = 'log.txt') {
  message <- paste(format(Sys.time(), "%Y-%m-%d %X"),
                   '|',
                   Sys.getenv('COMPUTERNAME'),
                   '|',
                   Sys.getenv('USERNAME'),
                   '|',
                   message,
                   '|')
  write(message, file = file, append = T)

}
