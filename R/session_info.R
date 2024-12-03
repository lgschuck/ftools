#' Function for info about current R session
#' @export session_info
#'
#' @param r_version R version
#' @param system OS
#' @param user User name
#' @param machine Computer name
#' @param time time
#' @param libs Libs folder
#' @param env_obj Objects in the environment
#' @param sep Separator
#' @param limit position to start text next to the parameters
#'
#' @examples
#' session_info()
#'
session_info <- function(r_version = getRversion(),
                         system = paste(c(Sys.info()['sysname'],
                                          Sys.info()['release'],
                                          '-', Sys.info()['version']),
                                        collapse = ' '),
                         user = Sys.getenv('USERNAME'),
                         machine = Sys.getenv('COMPUTERNAME'),
                         time = format(Sys.time(), "%Y/%m/%d - %X"),
                         libs = .libPaths(),
                         env_obj = ls(all.names = T, envir = globalenv()),
                         sep = paste0("\n", paste0(rep('=', 60), collapse = ''), "\n"),
                         limit = 12) {
  space <- '         '
  limit <- min(limit, 12)

  writeLines(
    paste0(
      sep,
      substr(paste0("R Version: ", space), 1, limit - 1),
      r_version,
      substr(paste0("\nSystem: ", space), 1, limit),
      system,
      substr(paste0("\nUser: ", space), 1, limit),
      user,
      substr(paste0("\nMachine:", space), 1, limit),
      machine,
      substr(paste0("\nTime:", space), 1, limit),
      time,
      sep,
      "Lib folders:\n\t",
      paste(libs, collapse = "\n\t"),
      sep,
      "Global Env Objects:\n\t",
      paste(env_obj, collapse = "\n\t"),
      sep
    )
  )

}
