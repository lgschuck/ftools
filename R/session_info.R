#' Function for info about current R session
#' @export session_info
#'
#' @examples
#' session_info()
#'
session_info <- function(r_version = getRversion(),
                         system = osVersion,
                         user = Sys.getenv('USERNAME'),
                         machine = Sys.getenv('COMPUTERNAME'),
                         time = format(Sys.time(), "%Y/%m/%d - %X"),
                         libs = .libPaths(),
                         env_obj = ls(all.names = T, envir = globalenv()),
                         header = paste0("\n", paste0(rep('=', 60), collapse = ''), "\n"),
                         limit = 12) {
  space <- '         '
  limit <- min(limit, 12)

  writeLines(
    paste0(
      header,
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
      header,
      "Libraries:\n\t",
      paste(libs, collapse = "\n\t"),
      header,
      "Global Env Objects:\n\t",
      paste(env_obj, collapse = "\n\t"),
      header
    )
  )

}
