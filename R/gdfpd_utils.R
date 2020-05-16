#' Converts a dataframe from gdfpd_GetDFPData to the wide format
#'
#' @param data.in Data frame with financial information
#' @param data.in.cols Which data to go in rows values ('original' or 'inflation adjusted')
#'
#' @return A dataframe in the wide format
#' @export
#'
#' @examples
#'
#' # get example data from RData file
#' my.f <- system.file('extdata/Example_DFP_Report_Petrobras.RData', package = 'GetDFPData')
#' load(my.f)
#'
#' df.assets <- df.reports$fr.assets[[1]]
#' df.assets.wide <- gdfpd.convert.to.wide(df.assets)
gdfpd.convert.to.wide <- function(data.in, data.in.cols = 'original') {

  possible.types <- c('original','inflation adjusted')
  if ( !any(data.in.cols %in% possible.types) ) {
    stop('ERROR: input data.in.cols must be either "original" or "inflation adjusted"')
  }

  if (!any('data.frame' %in% class(data.in))) {
    stop('input data.in does not seems to be a dataframe..')
  }

  value.var <- switch(data.in.cols,
                      'original' = 'acc.value',
                      'inflation adjusted' =  'acc.value.infl.adj')

  df.wide <- reshape2::dcast(data = data.in,
                             formula = acc.number + acc.desc + name.company  ~ ref.date,
                             value.var = value.var, fill = 0)

  return(df.wide)

}

#' Helps users search for a company name
#'
#' @param char.to.search Character for partial matching
#' @inheritParams gdfpd.GetDFPData
#'
#' @return Names of found companies
#' @export
#'
#' @examples
#'
#' \dontrun{ # dontrun: keep cran check fast
#' gdfpd.search.company('GERDAU')
#' }
gdfpd.search.company <- function(char.to.search, cache.folder = 'DFP Cache Folder') {

  df.info <- gdfpd.get.info.companies(type.data = 'companies_files', cache.folder )

  df.info <- df.info[df.info$type.fin.report == 'dfp', ]

  unique.names <- unique(df.info$name.company)
  char.target <- iconv(stringr::str_to_lower(unique.names),to='ASCII//TRANSLIT')
  char.to.search <- iconv(stringr::str_to_lower(char.to.search),to='ASCII//TRANSLIT')

  idx <- stringr::str_detect(char.target, pattern = stringr::fixed(char.to.search))

  char.out <- stats::na.omit(unique.names[idx])

  temp.df <- unique(df.info[df.info$name.company %in% char.out, c('name.company', 'id.date', 'situation')])

  cat('\n\nFound', length(char.out), 'companies:')

  for (i.company in char.out) {

    temp.df <- df.info[which(df.info$name.company == i.company), ]

    first.date <- min(stats::na.omit(temp.df$id.date))
    last.date  <- max(stats::na.omit(temp.df$id.date))

    cat(paste0('\n', paste0(i.company, paste0(rep(' ', max(nchar(char.out)) - nchar(i.company)),
                                              collapse = '' ),
                            ' | situation = ', temp.df$situation[1],
                            ' | first date = ', first.date,
                            ' | last date - ',  last.date) ) )
  }

  cat('\n\n')

  return(char.out)

}

#' Reads FWF file from bovespa (internal)
#'
#' @param my.f File to be read
#' @param flag.thousands A flag for thousands values
#' @return A dataframe with data
#' @export
#' @examples
#'
#' my.f <- system.file('extdata/DFPBPAE.001', package = 'GetDFPData')
#'
#' df.assets <- gdfpd.read.fwf.file(my.f, flag.thousands = FALSE)
gdfpd.read.fwf.file <- function(my.f, flag.thousands) {

  if (length(my.f) == 0) {
    warning('Warning: my.f is of length 0')

    df.out <- data.frame(acc.number= NA,
                         acc.desc = NA,
                         acc.value = NA)
    return(df.out)

  }

  if (is.na(my.f)) {

    warning('Warning:  my.f is NA!')

    df.out <- data.frame(acc.number= NA,
                         acc.desc = NA,
                         acc.value = NA)
    return(df.out)
  }

  if (file.size(my.f) ==0 ) {
    df.out <- data.frame(acc.number= NA,
                         acc.desc = NA,
                         acc.value = NA)
    return(df.out)
  }

  # set cols for fwf

  my.col.types <- readr::cols(
    acc.number = readr::col_character(),
    acc.desc = readr::col_character(),
    acc.value1 = readr::col_integer(),
    acc.value2 = readr::col_integer(),
    acc.value = readr::col_integer()
  )

  my.col.names<-  c('acc.number', 'acc.desc', 'acc.value1','acc.value2','acc.value')
  my.pos <- readr::fwf_positions(start = c(15, 28, 74,89,89+14+1), end = c(27, 67, 82,97,112),
                                 col_names = my.col.names)

  df.out <- readr::read_fwf(my.f, my.pos,
                            locale = readr::locale(encoding = 'Latin1'), col_types =  my.col.types)


  df.out <- df.out[, c('acc.number', 'acc.desc', 'acc.value')]

  # fix for flag.thousands
  if (flag.thousands) df.out$acc.value <- df.out$acc.value/1000

  # fix for empty data
  if (nrow(df.out) == 0) {
    df.out <- tibble::tibble(acc.number = NA,
                             acc.desc = NA,
                             acc.value = NA)
  }

  return(df.out)

}


#' Downalods files from the internet
#'
#' @param dl.link Link to file
#' @param dest.file = Destination, as local file
#' @inheritParams gdfpd.GetDFPData
#' @return Nothing
#' @export
#' @examples
#'
#' my.url <- paste0('http://www.rad.cvm.gov.br/enetconsulta/',
#'                   'frmDownloadDocumento.aspx?CodigoInstituicao=2',
#'                   '&NumeroSequencialDocumento=46133')
#'
#' \dontrun{ # keep CHECK fast
#' dl.status <- gdfpd.download.file(my.url, 'tempfile.zip', 10)
#' }
gdfpd.download.file <- function(dl.link, dest.file, max.dl.tries) {

  Sys.sleep(1)

  for (i.try in seq(max.dl.tries)) {

    try({
      # old code. See issue 11: https://github.com/msperlin/GetDFPData/issues/11
      # utils::download.file(url = dl.link,
      #                      destfile = dest.file,
      #                      quiet = T,
      #                      mode = 'wb')

      # fix for issue 13: https://github.com/msperlin/GetDFPData/issues/13
      my.OS <- tolower(Sys.info()["sysname"])
      if (my.OS == 'windows') {
        utils::download.file(url = dl.link,
                             destfile = dest.file,
                             #method = 'wget',
                             #extra = '--no-check-certificate',
                             quiet = T,
                             mode = 'wb')
      } else {
        # new code (only works in linux)

        # change https to https (or vice versa)? (leave it for future reference)
        #dl.link <- stringr::str_replace(dl.link, stringr::fixed('https'), 'http' )
        #dl.link <- stringr::str_replace(dl.link, stringr::fixed('http'), 'https' )

        utils::download.file(url = dl.link,
                             destfile = dest.file,
                             method = 'wget',
                             extra = "--ciphers 'DEFAULT:!DH' --no-check-certificate", # use unsecure dl
                             quiet = T,
                             mode = 'wb')
      }



    })

    if (file.size(dest.file) < 10  ){
      cat(paste0('\n\t\t\tError in downloading. Attempt ',i.try,'/', max.dl.tries))
      Sys.sleep(1)
    } else {
      return(TRUE)
    }

  }

  return(FALSE)


}

# set new cols, remove duplicate information and fix order
my.fix.cols <- function(df.in, name.company, ref.date, do.fre.register = FALSE) {

  if (is.null(df.in)) {
    #df.in <- data.frame(flag.NODATA = TRUE)
    return(data.frame())
  }

  if (!is.data.frame(df.in)) return(df.in)

  if (nrow(df.in) ==0) return(data.frame())

  old.names <- names(df.in)
  df.in$name.company <- name.company
  df.in$ref.date <- ref.date

  my.cols <- c('name.company', 'ref.date', old.names)
  df.in <- df.in[ ,my.cols]

  # force Encoding
  my.fct <- function(col.in) {
    if (is.factor(col.in)) {
      col.in <- as.character(col.in)
    }

    if (is.numeric(col.in)) return(col.in)

    if (is.character(col.in)) {
      Encoding(col.in) <- 'UTF-8'
    }

    return(col.in)
  }

  if (do.fre.register) {
    # make sure that the dates in FRE are explicit in dataset
    df.in <- tibble::add_column(.data = df.in, year.fre = lubridate::year(ref.date) + 1,
                                .after = 'ref.date')
  }

  df.in <- as.data.frame(lapply(X = df.in, my.fct), stringsAsFactors = FALSE)

  return(df.in)
}


#' Merges (row wise) dataframes from different list, using names of dataframes as index
#'
#' @param l.1 First dataframe
#' @param l.2 Second dataframe
#'
#' @return A list with binded dataframes (same names as l.1)
#' @export
#'
#' @examples
#'
#' l.1 <- list(x = data.frame(runif(10)) )
#' l.2 <- list(x = data.frame(runif(10)) )
#'
#' l <- my.merge.dfs.lists(l.1, l.2)
#'
my.merge.dfs.lists <- function(l.1, l.2) {
  names.1 <- names(l.1)
  names.2 <- names(l.2)

  if (is.null(names.1)) return(l.2)

  if (is.null(names.2)) return(l.1)

  if (!all(names.1 == names.2)) {
    stop('Cant bind dataframes. Names in lists dont match!')
  }

  n.elem <- length(l.1)

  l.out <- list()
  for (i.l in seq(n.elem)) {

    l.out[[i.l]] <- dplyr::bind_rows(l.1[[i.l]], l.2[[i.l]])

  }

  names(l.out) <- names(l.2)
  return(l.out)

}
