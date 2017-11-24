#' Reads up to date information about Bovespa companies from a github file
#'
#' A csv file with information about available companies, file links and time periods is read from github.
#' This file is manually updated by the author. When run for the first time in a R session, a .RDATA file
#' containing the output of the function is saved for caching.
#'
#' @param type.data A string that sets the type of information to be returned ('companies' or 'companies_files').
#' If 'companies', it will return a dataframe with several information about companies, but without download links.
#' @inheritParams gdfpd.GetDFPData
#'
#' @return A dataframe with several information about Bovespa companies
#' @export
#'
#' @examples
#'
#' \dontrun{ # keep cran check fast
#' df.info <- gdfpd.get.info.companies()
#' str(df.info)
#' }
gdfpd.get.info.companies <- function(type.data = 'companies_files', cache.folder = 'DFP Cache Folder') {

  # error checking
  possible.values <- c('companies_files', 'companies')
  if ( !(type.data %in% possible.values) ) {
    stop('Input type.data should be one of:\n\n', paste0(possible.values, collapse = '\n'))
  }

  # create folder
  if (!dir.exists(cache.folder)) dir.create(cache.folder)

  # check if cache file exists
  my.f.rdata <- file.path(cache.folder,paste0('df_info_CACHED_', type.data,
                                              '_', Sys.Date(), '.RData') )

  if (file.exists(my.f.rdata)) {
    cat('Found cache file. Loading data..')
    load(my.f.rdata)
    return(df.info)
  }

  # get data from github
  cat('\nReading info file from github')
  link.github <- 'https://raw.githubusercontent.com/msperlin/GetitrData_auxiliary/master/InfoBovespaCompanies.csv'

  my.cols <- readr::cols(
    id.company = readr::col_integer(),
    name.company = readr::col_character(),
    main.sector = readr::col_character(),
    sub.sector = readr::col_character(),
    segment = readr::col_character(),
    listing.segment = readr::col_character(),
    tickers = readr::col_character(),
    id.file = readr::col_integer(),
    dl.link = readr::col_character(),
    id.date = readr::col_date(),
    id.type = readr::col_character(),
    type.fin.report = readr::col_character(),
    situation = readr::col_character()
  )


  df.info <- readr::read_csv(link.github, col_types = my.cols)

  # remove rows without id for dates or situation
  idx <- (!is.na(df.info$id.date))&(!is.na(df.info$situation))
  df.info <- df.info[idx, ]

  n.actives <- sum(unique(df.info[ ,c('name.company', 'situation')])$situation == 'ATIVO')
  n.inactives <- sum(unique(df.info[ ,c('name.company', 'situation')])$situation != 'ATIVO' )

  cat('\nFound', nrow(df.info), 'lines for', length(unique(df.info$name.company)), 'companies ',
      '[Actives = ', n.actives, ' Inactives = ', n.inactives, ']')

  my.last.update <- readLines('https://raw.githubusercontent.com/msperlin/GetitrData_auxiliary/master/LastUpdate.txt')
  cat('\nLast file update: ', my.last.update)

  if (type.data == 'companies') {

    my.cols <- my.cols <- c("name.company","id.company", "situation", "listing.segment",
                            "main.sector", "tickers")
    df.info.agg <- unique(df.info[, my.cols])

    my.fun <- function(df) {
      return(c(min(df$id.date), max(df$id.date)))
    }
    out <- by(data = df.info, INDICES = df.info$name.company, FUN = my.fun)

    df.temp <- data.frame(name.company = names(out),
               first.date = sapply(out, FUN = function(x) as.character(x[1])),
               last.date = sapply(out, FUN = function(x) as.character(x[2])),
    stringsAsFactors = F )

    df.info.agg <- merge(df.info.agg, df.temp, by = 'name.company')
    df.info.agg$first.date <- as.Date(df.info.agg$first.date)
    df.info.agg$last.date <- as.Date(df.info.agg$last.date)

    df.info <- df.info.agg
  }

  cat('\nCaching RDATA into tempdir()')
  save('df.info', file = my.f.rdata)

  return(df.info)

}
