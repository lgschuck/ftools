#' Function for info about current R session
#' @export session_info
#' session_info()
#'
session_info <- function(){
  r_version <- getRversion()
  user <- Sys.getenv('USERNAME')
  machine <- Sys.getenv('COMPUTERNAME')
  time <- format(Sys.time(), "%Y/%m/%d - %X")
  libs <- .libPaths()
  env_obj <- ls(all.names = T, envir = globalenv())
  search <- search()

  header = "\n===========================================\n"

  writeLines(paste0(header,
                    "R Version:\t",
                    r_version,
                    header,
                    "User:\t",
                    user,
                    header,
                    "Machine:\t",
                    machine,
                    header,
                    "Time:\t",
                    time,
                    header,
                    "Libraries:\n\t",
                    paste(libs, collapse = "\n\t"),
                    header,
                    "Global Env Objects:\n\t",
                    paste(env_obj, collapse = "\n\t"),
                    header,
                    "Search:\n\t",
                    paste(search, collapse = "\n\t"),
                    header

                    ))

}
