#' Function for difference between dates
#' @export dif_month
#'
#' @param date1 object in Date format
#' @param date2 object in Date format
#'
#' @examples
#'
#' #ftools::dif_month(as.Date('2022-12-15'),as.Date('2022-03-15'))
#'
#' #ftools::dif_month(as.Date('2022-12-15'),as.Date('2023-03-15'))
#'
dif_month <- function(date1, date2){

  m_seq1 <- as.integer(strftime(date1, format='%Y'))*12 +
    as.integer(strftime(date1, format='%m'))

  m_seq2 <- as.integer(strftime(date2, format='%Y'))*12 +
    as.integer(strftime(date2, format='%m'))

  dif_month <- m_seq1 - m_seq2
  return(dif_month)
}


