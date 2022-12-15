#' Function for info about current R session
#' @export session_info
#'
#' @examples
#' session_info()
#'
session_info <- function(){
  r_version <- getRversion()
  system <- sessionInfo()$running
  user <- Sys.getenv('USERNAME')
  machine <- Sys.getenv('COMPUTERNAME')
  time <- format(Sys.time(), "%Y/%m/%d - %X")
  libs <- .libPaths()
  env_obj <- ls(all.names = T, envir = globalenv())

  header <- "\n=========================================\n"

  writeLines(paste0(header,
                    "R Version:\t",
                    r_version,
                    "\nSystem:\t",
                    system,
                    "\nUser:\t",
                    user,
                    "\nMachine:\t",
                    machine,
                    "\nTime:\t",
                    time, "\n",
                    header,
                    "Libraries:\n\t",
                    paste(libs, collapse = "\n\t"),
                    header,
                    "Global Env Objects:\n\t",
                    paste(env_obj, collapse = "\n\t"),
                    header
                    ))

}
