#' Function for get dependencies recursively, excluding R base packages.
#' @export search_dependencies
#'
#' @param packages vector with packages names
#' @param type type of dependencies. Like package_dependencies from tools package.
#' @examples
#' search_dependencies(c('dplyr', 'shiny', 'data.table'))
#'
search_dependencies <- function(packages,
                                type = 'Imports') {
  dependencies <- tools::package_dependencies(packages,
                                              recursive = T,
                                              which = type)

  # Remove dependencies that are base R packages
  base_r_packages <- rownames(installed.packages(priority = "base"))
  dependencies <-
    unlist(dependencies[!(dependencies %in% base_r_packages)],
           use.names = F)

  return(dependencies)
}
