#' Downloads and reads financial reports from B3's DFP/FRE/FCA system
#'
#' Annual data for financial reports and corporate events are downloaded from B3 for a combination of companies and time period.
#' This function gathers data into a single tibble object and organizes it in a tabular/long format.
#'
#' The easiest way to get started with gdfpd.GetDFPData is looking for the official name of traded companies using function gdfpd.search.company('nametolookfor').
#' Alternatively, you can use function gdfpd.get.info.companies('companies') to import a dataframe with information for all available companies and time periods.
#'
#' @param name.companies Official names of companies to get financial reports (e.g. 'ELETROPAULO METROPOLITANA EL.S.PAULO S.A').
#' Names of companies can be found using function gdfpd.search.company('nametolookfor') or gdfpd.get.info.companies('companies')
#' @param first.date First date (YYYY-MM-DD) to get data. Character or Date. E.g. first.date = '2010-01-01'.
#' @param last.date Last date (YYYY-MM-DD) to get data. Character or Date. E.g. last.date = '2017-01-01'.
#' @param type.info Type of financial statements, 'individual' (default) or 'consolidated'. Argument can be a single value or a vector with the same
#' length as name.companies. The individual type only includes financial statements from the company itself, while consolidated statements adds information
#' about controlled companies
#' @param inflation.index Sets the inflation index to use for finding inflation adjusted values of all reports. Possible values: 'dollar' (default) or 'IPCA', the brazilian main inflation index.
#' When using 'IPCA', the base date is set as the last date found in the DFP dataset.
#' @param max.levels Sets the maximum number of levels of accounting items in financial reports (default = 3)
#' @param folder.out Folder where to download and manipulate the zip files. Default = tempdir()
#' @param do.cache Logical for controlling to whether to use a cache system or not. Default = TRUE
#' @param cache.folder Folder to cache (save) all processed information. Default = file.path(getwd(),'DFP Cache Folder')
#' @param max.dl.tries Maximum number of attempts for dowloading files
#'
#' @return A tibble object with all gathered financial statements, with each company as a row
#' @export
#'
#' @examples
#'
#' \dontrun{ #dontrun: keep cran check time short
#' name.companies <- 'ELETROPAULO METROPOLITANA EL.S.PAULO S.A'
#' first.date <- '2005-01-01'
#' last.date <-  '2006-01-01'
#'
#' df.statements <- gdfpd.GetDFPData(name.companies = name.companies,
#'                                   first.date = first.date,
#'                                   last.date = last.date)
#'  }
gdfpd.GetDFPData <- function(name.companies,
                             first.date = Sys.Date()-12*30,
                             last.date = Sys.Date(),
                             type.info = 'individual',
                             inflation.index = 'dollar',
                             max.levels = 3,
                             folder.out = tempdir(),
                             do.cache = TRUE,
                             cache.folder = 'DFP Cache Folder',
                             max.dl.tries = 10) {

  # sanity check
  possible.values <- c('individual', 'consolidated')
  if ( !(any(type.info %in% possible.values)) ){
    stop('Input type.info should be "individual" or "consolidated"')
  }

  if (length(type.info) == 1) {
    type.info <- rep(type.info, length(name.companies))
  }

  if (length(type.info) != length(name.companies)) {
    stop('Length of type.info does not match the length of name.companies')
  }

  if (!dir.exists(folder.out)) dir.create(folder.out)

  if (!dir.exists(cache.folder)) dir.create(cache.folder)

  # check input inflation.index
  possible.values <- c('IPCA', 'dollar')
  if ( !(any(inflation.index %in% possible.values)) ) {
    stop(paste0('Input inflation.index should be one of:\n' , paste0(possible.values, collapse = '\n') ) )
  }

  if (max.levels < 1) {
    stop('Input max.levels should be higher than one')
  }

  # check do.cache

  if (class(do.cache) != 'logical') {
    stop('Input do.cache should be logical (TRUE or FALSE)')
  }

  # check internet
  if (!curl::has_internet()) {
    stop('You need an active internet connection to download files from Bovespa.')
  }

  # get data from github
  df.info <- gdfpd.get.info.companies(type.data = 'companies_files',
                                      cache.folder = cache.folder)
  unique.names <- unique(df.info$name.company)

  idx <- !(name.companies %in% unique.names)
  if (any( idx)) {
    stop(paste0('Name of companies: \n\n ', paste0(name.companies[idx], collapse = '\n'), '\n\n',
                'not found in registry. Use df.info <- gdfpd.get.info.companies() to find the names of all available companies.'))
  }

  # check dates
  first.date <- as.Date(first.date)
  last.date <- as.Date(last.date)

  if ( (class(first.date) != 'Date')|(class(last.date) != 'Date') )  {
    stop('Inputs first.date or last.date does not seem to be dates. Use format YYYY-MM-DD')
  }

  if (last.date < first.date) {
    stop('Your last.date is older than first.date. Did you mix them up?')
  }

  # find available dates for selected companies
  idx <- (df.info$name.company %in% name.companies)&
    (df.info$id.date >= first.date)&(df.info$id.date <= last.date)

  df.to.process <- df.info[idx, ]

  # remove duplicates/NA and filter for type.data
  idx <- !duplicated(df.to.process[, c('id.company', 'id.date', 'type.fin.report')])
  df.to.process <- df.to.process[idx, ]

  idx <- !is.na(df.to.process$id.company)
  df.to.process <- df.to.process[idx, ]

  idx <- !is.na(df.to.process$name.company)
  df.to.process <- df.to.process[idx, ]

  if (nrow(df.to.process) == 0){
    stop('Cannot find any dates related to companies in registry. You should try different dates and companies.')
  }

  # msg to prompt
  if (length(unique(type.info))==1){
    msg.reach <- type.info[1]
  } else {
    # find most frequent
    tbl <- sort(table(type.info), decreasing = TRUE)
    msg.reach <- paste0('mostly ', names(tbl)[1])
  }

  cat(paste0('\n\nDownloading data for ', length(name.companies), ' companies',
             '\nType of financial reports: ', msg.reach,
             '\nFirst Date: ',first.date,
             '\nLaste Date: ',last.date,
             '\nInflation index: ', inflation.index,
             '\n\n') )

  cat(paste0('Downloading inflation data' ))

  # download inflation data using BETS

  df.inflation <- gdfpd.get.inflation.data(inflation.index, do.cache)

  cat('\tDone\n\n')

  # try to find company's names
  idx <- !name.companies %in% unique(df.to.process$name.company)

  if (any(idx)) {
    cat(paste0('\nWARNING: Cant find available dates for ', paste0(name.companies[idx], collapse = ', ')))
  }

  # warn user for lack of cash flow data
  if (any(df.to.process$id.date < as.Date('2009-01-01'))) {
    cat('\nWARNING: Cash flow statements are not available before 2009, the \n\n')
  }

  # start processing
  cat(paste0('Inputs looking good! Starting download of files:\n' ) )

  for (i.company in unique(df.to.process$name.company) ) {

    idx <- (df.to.process$name.company == i.company)&
      (df.to.process$type.fin.report == 'dfp')
    temp.df <- df.to.process[idx, ]

    cat(paste0('\n', i.company) )
    cat(paste0('\n\tAvailable periods: ', paste0(temp.df$id.date, collapse = '\t')) )

  }

  cat('\n\n')

  tibble.out <- tibble::tibble()

  for (i.company in unique(df.to.process$name.company)) {

    my.id <- temp.df$id.company[1]
    cat(paste0('\nProcessing ', my.id, ' - ',  i.company) )

    idx <- (df.to.process$name.company == i.company)&
      (df.to.process$type.fin.report == 'dfp')
    temp.df <- df.to.process[idx,  ]

    # get data from Bovespa site
    cat(paste0('\n\tFinding info from Bovespa') )

    # do cache
    #   check if name wont be a problem
    my.cache.dir.company <- stringr::str_trim(paste0(my.id,'_',
                                                     stringr::str_sub(i.company, 1,8)))
    my.cache.dir.company <- stringr::str_replace_all(my.cache.dir.company, '/', '')

    #   set dirs
    my.cache.dir <- file.path(cache.folder, my.cache.dir.company)

    if (!dir.exists(my.cache.dir)) dir.create(my.cache.dir)

    f.cache <- file.path(my.cache.dir, paste0('GetDFPData_BOV_cache_',
                                              my.id,'_',
                                              stringr::str_sub(i.company, 1,4),'.rds' ) )


    if (file.exists(f.cache)&(do.cache)) {
      cat(paste0('\n\t\tFound cache file ', f.cache) )

      l.out.bov <- readRDS(f.cache)
    } else {
      cat(' | downloading and reading data')

      l.out.bov <- gdfpd.get.bovespa.data(my.id)

      if (do.cache) {
        cat(' | saving cache')

        saveRDS(object = l.out.bov, file = f.cache)
      }

    }

    # fix cols
    my.fct <- function(df.in) {
      df.in$name.company = i.company
      return(df.in)
    }

    l.out.bov <- lapply(l.out.bov, my.fix.cols, name.company = i.company, ref.date = 'Current')

    current.stock.holders <- l.out.bov$df.stock.holders
    current.stock.composition <- l.out.bov$df.stock.composition
    df.dividends <- l.out.bov$df.dividends
    company.segment <- l.out.bov$company.segment

    type.info.now <- type.info[which(i.company == name.companies)]
    df.assets <- data.frame()
    df.liabilities <- data.frame()
    df.income <- data.frame()
    df.cashflow <- data.frame()
    df.fre.stock.holders <- data.frame()
    df.fre.capital <- data.frame()
    df.fre.stock.values <- data.frame()
    df.fre.mkt.value <-  data.frame()
    df.fre.increase.capital <- data.frame()
    df.fre.compensation <- data.frame()
    df.fre.compensation.summary <- data.frame()
    df.fre.transactions.related <-  data.frame()
    df.fre.other.events <- data.frame()
    df.fre.stock.repurchases <- data.frame()
    df.fre.debt.composition <- data.frame()
    df.fre.governance.listings <- data.frame()
    df.fre.capital.reduction <- data.frame()
    df.fre.board.composition <- data.frame()
    df.fre.committee.composition <- data.frame()
    df.fre.family.relations <- data.frame()
    for (i.date in as.character(temp.df$id.date) ) {

      temp.df2 <- temp.df[temp.df$id.date == i.date,  ]

      # cases for more than one file per quarter
      if (nrow(temp.df2)> 1) {
        # find id with highest value (most recent file)
        temp.df2 <- temp.df2[which.max(temp.df2$id.file), ]
      }

      cat(paste0('\n\tProcessing ', my.id, ' - ', i.company, ' | date ', i.date  ) )

      # get dfp data
      dl.link <- temp.df2$dl.link

      # fix file names for latin characters
      my.filename <- iconv(temp.df2$name.company, to = 'ASCII//TRANSLIT')
      my.filename <- stringr::str_replace_all(my.filename, stringr::fixed('?'), '_')

      temp.file = file.path(folder.out, paste0('DFP_',
                                               temp.df2$id.company, '_',
                                               stringr::str_sub(my.filename,1,4), '_',
                                               i.date, '.zip') )


      cat(paste0('\n\t\tAcessing DFP data') )

      # do cache
      f.cache <- file.path(my.cache.dir,
                           paste0('GetDFPData_DFP_cache_',
                                  my.id,'_',
                                  type.info.now, '_',
                                  stringr::str_sub(i.company, 1,4), '_',
                                  i.date, '.rds'))

      if (file.exists(f.cache)&(do.cache)) {
        cat(paste0(' | Found DFP cache file') )

        l.out <- readRDS(f.cache)
      } else {

        if (file.exists(temp.file)) {
          cat(' | file exists (no dl)')
        } else {
          cat(' | downloading file')

          dl.status <- gdfpd.download.file(dl.link = dl.link,
                                           dest.file = temp.file,
                                           max.dl.tries = max.dl.tries)
        }

        cat(' | reading file')

        suppressWarnings({
          l.out <- gdfpd.read.dfp.zip.file(my.zip.file = temp.file, folder.to.unzip = tempdir(),
                                       id.type = temp.df2$id.type)
        })

        cat(' | saving cache')

        if (do.cache) {
          saveRDS(object = l.out, file = f.cache)
        }
      }


      if (type.info.now == 'individual') {
        out.df <- l.out$ind.dfs
      }

      if (type.info.now == 'consolidated') {
        out.df <- l.out$cons.dfs
      }

      # set some cols for long format
      out.df$df.assets$ref.date <- as.Date(i.date)
      out.df$df.assets$company.name <- i.company
      out.df$df.liabilities$ref.date <- as.Date(i.date)
      out.df$df.liabilities$company.name <- i.company
      out.df$df.income$ref.date <- as.Date(i.date)
      out.df$df.income$company.name <- i.company
      out.df$df.cashflow$company.name <- i.company
      out.df$df.cashflow$ref.date <- as.Date(i.date)

      df.assets <- rbind(df.assets, out.df$df.assets)
      df.liabilities <- rbind(df.liabilities, out.df$df.liabilities)
      df.income <- rbind(df.income, out.df$df.income)
      df.cashflow <- rbind(df.cashflow, out.df$df.cashflow)

      # get data from FRE

      cat(paste0('\n\t\tAcessing FRE data') )

      idx <- (df.to.process$name.company == i.company)&
        (df.to.process$type.fin.report == 'fre')&
        (format(df.to.process$id.date, '%Y') == format(temp.df2$id.date, '%Y'))

      temp.df.fca <- df.to.process[idx,  ]

      if (nrow(temp.df.fca) == 0) {
        cat(' | No FRE/FCA file available..')
        next()

      }

      temp.file = file.path(folder.out, paste0('FRE_', temp.df2$id.company, '_',
                                               stringr::str_sub(my.filename,1,4), '_',
                                               i.date, '.zip') )


      dl.link <- temp.df.fca$dl.link



      # do cache
      f.cache <- file.path(my.cache.dir,
                           paste0('GetDFPData_FRE_cache_',
                                  my.id,'_',
                                  stringr::str_sub(i.company, 1,4), '_',
                                  i.date, '.rds'))

      if (file.exists(f.cache)) {
        cat(paste0(' | Found FRE cache file') )

        l.out.FRE <- readRDS(f.cache)
      } else {

        if (file.exists(temp.file)) {
          cat(' | file exists (no dl)')
        } else {
          cat(' | downloading file')

          dl.status <- gdfpd.download.file(dl.link = dl.link,
                                           dest.file = temp.file,
                                           max.dl.tries = max.dl.tries)
        }

        cat(' | reading file')

        suppressWarnings({
          l.out.FRE <- gdfpd.read.fre.zip.file(my.zip.file = temp.file,
                                               folder.to.unzip = tempdir())

        })

        cat(' | saving cache')

        saveRDS(object = l.out.FRE, file = f.cache)
      }


      # get data from FCA

      cat(paste0('\n\t\tAcessing FCA data') )

      idx <- (df.to.process$name.company == i.company)&
        (df.to.process$type.fin.report == 'fca')&
        (format(df.to.process$id.date, '%Y') == format(temp.df2$id.date, '%Y'))

      temp.df.fre <- df.to.process[idx,  ]

      if (nrow(temp.df.fre) == 0) {
        cat(' | No FCA file available..')

        next()

      }

      temp.file = file.path(folder.out, paste0('FCA_', temp.df2$id.company, '_',
                                               stringr::str_sub(my.filename,1,4), '_',
                                               i.date, '.zip') )


      dl.link <- temp.df.fre$dl.link



      # do cache
      f.cache <- file.path(my.cache.dir,
                           paste0('GetDFPData_FCA_cache_',
                                  my.id,'_',
                                  stringr::str_sub(i.company, 1,4), '_',
                                  i.date, '.rds'))

      if (file.exists(f.cache)) {
        cat(paste0(' | Found FCA cache file') )

        l.out.FCA <- readRDS(f.cache)
      } else {

        if (file.exists(temp.file)) {
          cat(' | file exists (no dl)')
        } else {
          cat(' | downloading file')

          dl.status <- gdfpd.download.file(dl.link = dl.link,
                                           dest.file = temp.file,
                                           max.dl.tries = max.dl.tries)
        }

        cat(' | reading file')

        suppressWarnings({
          l.out.FCA <- gdfpd.read.fca.zip.file(my.zip.file = temp.file,
                                               folder.to.unzip = tempdir())

        })

        cat(' | saving cache')

        saveRDS(object = l.out.FCA, file = f.cache)
      }

      # fix all dfs
      l.out.FRE <- lapply(l.out.FRE, my.fix.cols, name.company = i.company, ref.date = temp.df2$id.date)
      l.out.FCA <- lapply(l.out.FCA, my.fix.cols, name.company = i.company, ref.date = temp.df2$id.date)

      # save temporary dataframes
      suppressWarnings({
        df.fre.stock.holders    <- dplyr::bind_rows(df.fre.stock.holders, l.out.FRE$df.stockholders)
        df.fre.capital          <- dplyr::bind_rows(df.fre.capital, l.out.FRE$df.capital)
        df.fre.stock.values     <- dplyr::bind_rows(df.fre.stock.values, l.out.FRE$df.stock.values)
        df.fre.mkt.value        <- dplyr::bind_rows(df.fre.mkt.value, l.out.FRE$df.mkt.value)
        df.fre.increase.capital <- dplyr::bind_rows(df.fre.increase.capital, l.out.FRE$df.increase.capital)
        df.fre.stock.repurchases <- dplyr::bind_rows(df.fre.stock.repurchases, l.out.FRE$df.stock.repurchases)
        df.fre.other.events     <- dplyr::bind_rows(df.fre.other.events, l.out.FRE$df.other.events)
        df.fre.compensation     <- dplyr::bind_rows(df.fre.compensation, l.out.FRE$df.compensation)
        df.fre.compensation.summary <- dplyr::bind_rows(df.fre.compensation.summary, l.out.FRE$df.compensation.summary)
        df.fre.transactions.related <- dplyr::bind_rows(df.fre.transactions.related, l.out.FRE$df.transactions.related)
        df.fre.debt.composition <- dplyr::bind_rows(df.fre.debt.composition, l.out.FRE$df.debt.composition)
        df.fre.governance.listings <- dplyr::bind_rows(df.fre.governance.listings, l.out.FCA$df.governance.listings)
        df.fre.capital.reduction <- dplyr::bind_rows(df.fre.capital.reduction, l.out.FRE$df.capital.reduction)
        df.fre.board.composition <- dplyr::bind_rows(df.fre.board.composition, l.out.FRE$df.board.composition)
        df.fre.committee.composition <- dplyr::bind_rows(df.fre.committee.composition, l.out.FRE$df.committee.composition)
        df.fre.family.relations <- dplyr::bind_rows(df.fre.family.relations, l.out.FRE$df.family.relations)
      })
    }

    # clean up fr dataframes before saving
    df.assets <-      gdfpd.fix.dataframes(stats::na.omit(df.assets),
                                           inflation.index, df.inflation,max.levels)
    df.liabilities <- gdfpd.fix.dataframes(stats::na.omit(df.liabilities),
                                           inflation.index, df.inflation,max.levels)
    df.income <-      gdfpd.fix.dataframes(stats::na.omit(df.income),
                                           inflation.index, df.inflation, max.levels)
    df.cashflow <-    gdfpd.fix.dataframes(stats::na.omit(df.cashflow),
                                           inflation.index, df.inflation, max.levels)

    # only get unique values of increase capital and other events (it repeats in the  FRE system)

    my.fct.remove.dup <- function(df.in) {

      if (nrow(df.in) == 0) return(data.frame())

      idx <- !duplicated(df.in[, 3:ncol(df.in)])
      df.in <- df.in[idx, ]

      return(df.in)
    }


    df.fre.increase.capital <- my.fct.remove.dup(df.fre.increase.capital)
    df.fre.other.events <- my.fct.remove.dup(df.fre.other.events)
    df.fre.stock.repurchases <- my.fct.remove.dup(df.fre.stock.repurchases)
    df.fre.capital.reduction <- my.fct.remove.dup(df.fre.capital.reduction)

    # save it all
    tibble.company <- tibble::tibble(company.name = i.company,
                                     company.code = temp.df$id.company[1],
                                     company.tickers = temp.df$tickers[1],
                                     type.info = type.info.now,
                                     min.date = min(temp.df$id.date),
                                     max.date = max(temp.df$id.date),
                                     n.periods = length(temp.df$id.date),
                                     company.segment = company.segment,
                                     current.stockholders = list(current.stock.holders),
                                     current.stock.composition = list(current.stock.composition),
                                     fr.assets = list(df.assets),
                                     fr.liabilities = list(df.liabilities),
                                     fr.income = list(df.income),
                                     fr.cashflow = list(df.cashflow),
                                     history.dividends = list(df.dividends),
                                     history.stockholders = list(df.fre.stock.holders),
                                     history.capital.issues = list(df.fre.capital),
                                     #history.stock.values = list(df.fre.stock.values),
                                     history.mkt.value = list(df.fre.mkt.value),
                                     history.capital.increases = list(df.fre.increase.capital),
                                     history.capital.reductions = list(df.fre.capital.reduction),
                                     history.stock.repurchases = list(df.fre.stock.repurchases),
                                     history.other.stock.events = list(df.fre.other.events),
                                     history.compensation = list(df.fre.compensation),
                                     history.compensation.summary = list(df.fre.compensation.summary),
                                     history.transactions.related = list(df.fre.transactions.related),
                                     history.debt.composition = list(df.fre.debt.composition),
                                     history.governance.listings = list(df.fre.governance.listings),
                                     history.board.composition = list(df.fre.board.composition),
                                     history.committee.composition = list(df.fre.committee.composition),
                                     history.family.relations = list(df.fre.family.relations)
                                     )

    # bind for final df
    suppressWarnings({
      tibble.out <- dplyr::bind_rows(tibble.out, tibble.company)
    })

  }

  return(tibble.out)
}
