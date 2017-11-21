#' Downloads and read inflation data from github
#'
#' Inflation data is available at git repo 'msperlin/GetITRData_auxiliary'
#'
#' @inheritParams gdfpd.GetDFPData
#'
#' @return A dataframe with inflation data
#' @export
#'
#' @examples
#'
#' \dontrun{ # keep cran check fast
#' df.inflation <- gdfpd.get.inflation.data('IPCA')
#' str(df.inflation)
#' }
gdfpd.get.inflation.data <- function(inflation.index, do.cache) {

  # error checking
  possible.values <- c('dollar', 'IPCA')
  if ( !(inflation.index %in% possible.values) ) {
    stop('Input inflation.index should be one of:\n\n', paste0(possible.values, collapse = '\n'))
  }

  # check if cache file exists
  my.f.rdata <- file.path(tempdir(),paste0('df_inflation_', inflation.index, '.RData') )

  if (file.exists(my.f.rdata)) {
    cat('\n\tFound cache file. Loading data..')
    load(my.f.rdata)
    return(df.inflation)
  }

  # get data from github

  my.cols <- readr::cols(
    Date = readr::col_date(format = ""),
    Value = readr::col_double()
  )

  link.file <- switch(inflation.index,
                      'IPCA' = 'https://raw.githubusercontent.com/msperlin/GetitrData_auxiliary/master/Inflation-IPCA.csv',
                      'dollar' = 'https://raw.githubusercontent.com/msperlin/GetitrData_auxiliary/master/Inflation-dollar.csv')

  df.inflation <- readr::read_csv(link.file, col_types = my.cols)

  if (do.cache) {

    cat('\n\tCaching inflation RDATA into tempdir()')
    save('df.inflation', file = my.f.rdata)

  }

  return(df.inflation)

}
