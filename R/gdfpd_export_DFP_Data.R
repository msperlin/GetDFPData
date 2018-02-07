#' Export tibble to an excel or csv (zipped) file
#'
#' Export information from gdfpd_GetDFPData() to an excel file or csv. In the csv case, all tables are exported as csv
#' files and zipped in a single zip file.
#'
#' @param df.reports Tibble with financial information (output of gdfpd.GetDFPData)
#' @param base.file.name The basename of excel file (make sure you dont include the file extension)
#' @param type.export The extension of the desired format: 'xlsx' (default) or 'csv'
#'
#' @return TRUE, if successfull (invisible)
#' @export
#'
#' @examples
#'
#' # get example data from RData file
#' my.f <- system.file('extdata/Example_DFP_Report_Petrobras.RData', package = 'GetDFPData')
#' load(my.f)
#'
#' \dontrun{ # dontrun: keep cran check time short
#' gdfpd.export.DFP.data(df.reports, base.file.name = 'MyExcelFile', format.data = 'wide')
#' }
gdfpd.export.DFP.data <- function(df.reports,
                                  base.file.name = paste0('GetDFPData_Export_',Sys.Date()),
                                  type.export = 'xlsx') {

  # check args
  possible.exports <- c('xlsx', 'csv')
  if (any(!(type.export %in% type.export))) {
    stop('input type.export should be "xlsx"')
  }

  # possible.formats <- c('wide', 'long')
  # if (any(!(type.export %in% type.export))) {
  #   stop('input format.data should be "wide" or "long"')
  # }

  f.out <- paste0(base.file.name, switch(type.export,
                                         'xlsx' = '.xlsx',
                                         'csv' = '.zip'))

  if (file.exists(f.out)) {
    cat('File ', f.out, ' already exists. Deleting it..')
    file.remove(f.out)
  }

  # set dir for csv files
  csv.dir <- file.path(tempdir(), 'CSV-DIR')

  # empty csv dir
  my.f <- list.files(path = csv.dir, full.names = TRUE)
  file.remove(my.f)

  # copy metadata
  df.to.copy <- df.reports[ ,c("company.name", "company.code",
                               "min.date", "max.date", "n.periods")]

  # get full dataframes
  cols.to.copy <- names(df.reports)[11:ncol(df.reports)]

  suppressWarnings({
    my.l <- lapply(df.reports[, cols.to.copy],
                   function(x) do.call(what = dplyr::bind_rows, args = x))
  })

  # start copying

  cat(paste0('\nStart Copying Dataframes to ', f.out))

  # copy metadata table
  cat(paste0('\n\tCopying table ', 'metadata', ' - ', nrow(df.to.copy), ' rows, ', ncol(df.to.copy), ' columns'))

  # copy

  my.copy.fct(df.in = df.to.copy,
              name.df = 'metadata',
              base.file.name = base.file.name,
              type.export = type.export)

  for (i.df in seq(length(my.l)) ) {

    name.df <- names(my.l)[i.df]
    current.df <- my.l[[i.df]]
    test.fr <- stringr::str_detect(name.df, stringr::fixed('fr.' ) )

    # check if it is financial report and wheter we want wide format
    # if (test.fr) {
    #   if (format.data == 'wide') {
    #     current.df = gdfpd.convert.to.wide(current.df)
    #   }
    # }

    # copy
    my.copy.fct(df.in = current.df,
                name.df = name.df,
                base.file.name = base.file.name,
                type.export = type.export,
                csv.dir = file.path(tempdir(), 'CSV-DIR'))

  }


  if (type.export == 'csv') {

    files.to.zip <- list.files(path = csv.dir, pattern = '*.csv', full.names = T)

    cat('\n\n')
    utils::zip(zipfile = paste0(base.file.name,'.zip'), files = files.to.zip, flags = '-j')

  }

  cat(paste0('\n\nExport sucessful. Data available at file ',
             f.out, '\n\n'))

  invisible(TRUE)

}


#' Copies data to external file
#'
#' @param df.in Dataframe to be copied
#' @param name.df Name of dataframe to be copied
#' @param csv.dir Location where to save csv files prior to zipping (default = tempdir())
#' @inheritParams gdfpd.export.DFP.data
#'
#' @return TRUE (invisible), if successfull
#' @export
#'
#' @examples
#'
#' test.data <- data.frame(test.data = runif(100))
#' name.df <- 'TestData'
#' base.file.name <- 'TestData'
#' type.export <- 'csv'
#'
#' my.copy.fct(df.in = test.data, name.df, base.file.name, type.export)
my.copy.fct <- function(df.in, name.df, base.file.name,
                        type.export = 'xlsx',
                        csv.dir = tempdir()) {

  # sanity check
  possible.values <- c('csv', 'xlsx')
  if ( !(any(type.export %in% possible.values)) ){
    stop('Input type.export should be "csv" or "xlsx"')
  }

  possible.values <-

  cat(paste0('\n\tCopying table ', name.df, ' - ', nrow(df.in), ' rows, ', ncol(df.in), ' columns'))

  if (nrow(df.in) == 0) df.in <- data.frame(NODATA = 'No DATA')

  if (type.export == 'xlsx') {
    xlsx::write.xlsx(x = df.in,
                     file = paste0(base.file.name,'.',type.export),
                     sheetName = name.df,
                     append = T )
  }

  if (type.export == 'csv') {


    # create new dir for csv files
    if (!dir.exists(csv.dir)) dir.create(csv.dir)

    temp.file <- file.path(csv.dir,
                           paste0(stringr::str_replace_all(name.df,
                                                           pattern = stringr::fixed('.'), '_'), '.csv'))

    utils::write.csv(df.in,
                     file = temp.file,
                     row.names = FALSE,
                     fileEncoding = 'UTF-8')
  }

  invisible(TRUE)

}
