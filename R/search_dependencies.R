#' Function for get dependencies recursively, excluding R base packages.
#' @export search_dependencies
#'
#' @param packages vector with packages names
#' @param type type of dependencies. Like package_dependencies from tools package.
#'
#' @examples
#'
#' options(repos = c(CRAN = 'https://cran.rstudio.com/'))
#' search_dependencies(c('dplyr', 'shiny', 'data.table'))
#'
#' @importFrom tools package_dependencies
#' @importFrom utils installed.packages
#'
search_dependencies <- function(packages,
                                type = 'Imports') {
  dependencies <- tools::package_dependencies(packages,
                                              recursive = T,
                                              which = type)

  # Remove dependencies that are base R packages
  base_r_packages <- rownames(utils::installed.packages(priority = "base"))

  dependencies <- unique(unlist(dependencies))

  dependencies <- dependencies[!(dependencies %in% base_r_packages)]

  dependencies <- sort(dependencies)

  return(dependencies)
}
