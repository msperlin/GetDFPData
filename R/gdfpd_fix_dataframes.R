#' Fix dataframe for version issues and inflation measures (internal)
#'
#' @param df.in A dataframe with financial statements
#' @inheritParams gdfpd.GetDFPData
#' @param df.inflation Dataframe with inflation data
#' @return The fixed data.frame
#' @export
#'
#' @examples#'
#' # get example data from RData file
#' my.f <- system.file('extdata/Example_DFP_Report_Petrobras.RData', package = 'GetDFPData')
#' load(my.f)
#'
#' df.assets <- df.reports$fr.assets[[1]]
#'
#' df.assets.fixed <- gdfpd.fix.dataframes(df.assets,
#'                                         inflation.index = 'none',
#'                                         df.inflation = data.frame())
gdfpd.fix.dataframes <- function(df.in, inflation.index, df.inflation, max.levels = 3) {

  # if empty df
  if (nrow(df.in) == 0) {
    return(df.in)
  }

  # fix encoding
  df.in$acc.desc <- as.character(df.in$acc.desc)
  Encoding(df.in$acc.desc) <- 'UTF-8'

  # fix .00 in acc.number
  df.in$acc.number <- stringr::str_replace_all(df.in$acc.number, '.00', '')

  # fix change: 1.03 -> 1.02
  #browser()

  # fix names of acc.desc using latest info
  df.in$ref.date <- as.Date(df.in$ref.date)
  max.date <- max(df.in$ref.date)

  # fix names for cashflow statements (from 4.01 to 6.01)
  if (any(stringr::str_sub(df.in$acc.number, 1, 1) == '4') ) {
    substr(df.in$acc.number, 1, 1) <- "6"
  }

  # remove according to max.levels
  my.count <- function(x) {
    splitted <- stringr::str_split(x, stringr::fixed('.') )[[1]]
    return(length(splitted))
  }

  idx <- sapply(df.in$acc.number, my.count) <= max.levels
  df.in <- df.in[idx, ]

  # get reference table for substitution
  ref.table <- unique(df.in[df.in$ref.date == max.date, c('acc.number', 'acc.desc')])
  ref.table <- unique(df.in[ , c('acc.number', 'acc.desc', 'ref.date')])

  my.fct <- function(x, ref.table) {
    temp <- ref.table[ref.table$acc.number == x, ]

    idx <- which.max(temp$ref.date)
    return(temp$acc.desc[idx])
  }

  desc.to.use <- sapply(X = unique(df.in$acc.number), FUN = my.fct, ref.table = ref.table)

  # replace all
  idx <- match( df.in$acc.number, names(desc.to.use))
  df.in$acc.desc <- desc.to.use[idx]

  # fix inflation

  if (inflation.index == 'IPCA') {

    # get accumulated inflation index
    df.inflation$cum <- cumprod(df.inflation$Value/100 +1)

    # filter df.inflation for dates
    df.inflation <- df.inflation[df.inflation$Date<=max(df.in$ref.date), ]

    # use base date as last available date in df.inflation
    base.value <- df.inflation$cum[which.max(df.inflation$Date)]
    df.inflation$inflator <- df.inflation$cum/base.value

    # match time periods
    idx <- match(format(df.in$ref.date, '%Y-%m'), format(df.inflation$Date, '%Y-%m'))
    df.in$acc.value.infl.adj <- df.in$acc.value/df.inflation$inflator[idx]
  }

  if (inflation.index == 'dollar') {

    # find closest date for dollar
    match.neardate <- function(date.in, table.dates) {
      idx <- which.min(abs(date.in - table.dates))
      return(idx)
    }

    idx <- sapply(X = df.in$ref.date, FUN = match.neardate, table.dates = df.inflation$Date)

    df.in$acc.value.infl.adj <- df.in$acc.value/df.inflation$Value[idx]
  }

  # fix cols order
  my.col <- c("company.name","ref.date", "acc.number", "acc.desc",
              "acc.value", "acc.value.infl.adj")
  df.in <- df.in[ , my.col]

  return(df.in)
}

