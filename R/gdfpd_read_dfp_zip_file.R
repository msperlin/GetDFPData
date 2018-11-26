#' Reads a single zip file downloaded from Bovespa
#'
#' @param my.zip.file Full path to zip file
#' @param folder.to.unzip Folder to unzip files (default = tempdir())
#' @param id.type The type of file structure ('after 2011' or 'before 2011')
#'
#' @return A list with several dataframes containing financial statements
#' @export
#'
#' @examples
#'
#' my.f <- system.file('extdata/9512_PETR_2002-12-31.zip', package = 'GetDFPData')
#'
#' #my.l <- gdfpd.read.dfp.zip.file(my.f, id.type = 'before 2011')
#' #print(my.l)
gdfpd.read.dfp.zip.file <- function(my.zip.file,
                                    folder.to.unzip = tempdir(),
                                    id.type) {

  # sanity check
  if (tools::file_ext(my.zip.file) != 'zip') {
    stop(paste('File', my.zip.file, ' is not a zip file.') )
  }

  if (!file.exists(my.zip.file)) {
    stop(paste('File', my.zip.file, ' does not exists.') )
  }

  if (file.size(my.zip.file) == 0){
    file.remove(my.zip.file)
    stop(paste('File', my.zip.file, ' has size 0! File deleted. Try again..') )
  }

  if (length(my.zip.file) != 1){
    stop('This function only works for a single zip file... check your inputs')
  }

  if (!dir.exists(folder.to.unzip)) {
    cat(paste('Folder', folder.to.unzip, 'does not exist. Creating it.'))
    dir.create(folder.to.unzip)
  }

  my.basename <- tools::file_path_sans_ext(basename(my.zip.file))
  rnd.folder.name <- file.path(folder.to.unzip, paste0('DIR-',my.basename))

  if (!dir.exists(rnd.folder.name)) dir.create(rnd.folder.name)

  utils::unzip(my.zip.file, exdir = rnd.folder.name, junkpaths = TRUE)

  # list files and check it
  my.files <- list.files(rnd.folder.name)

  if (length(my.files) == 0) {
    #browser()

    file.remove(my.zip.file)
    stop(paste0('Zipped file contains 0 files. ',
                'This is likelly a problem with the downloaded file. ',
                'Try running the code again as the corrupted zip file was deleted and will be downloaded again.',
                '\n\nIf the problem persists, my suggestions is to remove the time period with problem.') )
  }

  if (id.type == 'after 2011') {
    my.l <- gdfpd.read.dfp.zip.file.type.1(rnd.folder.name, folder.to.unzip)
  }

  if (id.type == 'before 2011') {
    my.l <- gdfpd.read.dfp.zip.file.type.2(rnd.folder.name, folder.to.unzip)
  }

  # check for empty dfs
  my.fct <- function(df.in) {
    if (nrow(df.in)==0) {
      df.out <- data.frame(acc.number = NA, acc.desc = NA, acc.value = NA)
    } else {
      df.out <- df.in
    }
    return(df.out)
  }

  my.l <- lapply(my.l, my.fct)

  return(my.l)
}

#' Reads folder for zip file post 2011 (internal)
#'
#' @inheritParams gdfpd.read.dfp.zip.file
#' @param rnd.folder.name Folder where unzipped files are available
#'
#' @return A list with financial statements
#'
#' @examples
#' # no example (this functions not used directly)
gdfpd.read.dfp.zip.file.type.1 <- function(rnd.folder.name, folder.to.unzip = tempdir()) {

  company.reg.file <- file.path(rnd.folder.name,'FormularioDemonstracaoFinanceiraDFP.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company.reg.file, encoding = 'UTF-8'))


  # get basic info
  company.name = xml_data$CompanhiaAberta$NomeRazaoSocialCompanhiaAberta
  company.cvm_code <- xml_data$CompanhiaAberta$CodigoCvm
  company.SeqNumber <- xml_data$CompanhiaAberta$NumeroSequencialRegistroCvm
  company.date.delivery <- xml_data$DataEntrega
  date.docs <- as.Date(xml_data$DataReferenciaDocumento, format = '%Y-%m-%d')

  zipped.file <- file.path(rnd.folder.name, list.files(rnd.folder.name, pattern = '*.dfp')[1])

  utils::unzip(zipped.file, exdir = rnd.folder.name)

  # check wheter thousands  are used
  flag.thousands <- switch(xml_data$CodigoEscalaMoeda,
                           '2' = FALSE,
                           '1' = TRUE)

  # Get fin data data
  fin.report.file <- file.path(rnd.folder.name, 'InfoFinaDFin.xml')

  if (!file.exists(fin.report.file)) {
    stop('Cant find file', fin.report.file)
  }

  xml_data <- XML::xmlToList(XML::xmlParse(fin.report.file, encoding = 'UTF-8'))
  file.remove(fin.report.file)

  # function to get individual DF
  my.fct <- function(x, type.df, info, flag.thousands){

    if (type.df == 'individual') my.char = '1'
    if (type.df == 'consolidated') my.char = '2'

    if (x$PlanoConta$VersaoPlanoConta$CodigoTipoInformacaoFinanceira == my.char){

      if (info == 'Descricao') return(x$DescricaoConta1)
      if (info == 'Valor') {


        my.value <- as.numeric(c(x$ValorConta1, x$ValorConta2, x$ValorConta3,x$ValorConta4))
        my.value <- my.value[my.value != 0]
        if (length(my.value)==0) {
          my.value <- 0
        } else {
          if (flag.thousands) {
            my.value <- my.value[1]*1/1000
          } else {
            my.value <- my.value[1]
          }
        }

        return(my.value)
      }
      if (info == 'id') return(x$PlanoConta$NumeroConta)

    } else {
      return(NA)
    }
  }

  # get individual dfs
  type.df <- 'individual'
  acc.desc  <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'Descricao', flag.thousands = flag.thousands))
  acc.value <-   as.numeric(sapply(xml_data, my.fct, type.df = type.df, info = 'Valor', flag.thousands = flag.thousands))
  acc.number <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'id', flag.thousands = flag.thousands))

  ind.df <- data.frame(acc.number,acc.desc,acc.value)

  # save info
  df.assets <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '1', ])
  df.liabilities <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '2', ])
  df.income    <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '3', ])
  df.cashflow    <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '6', ])
  df.value <- stats::na.omit(ind.df[stringr::str_sub(ind.df$acc.number,1,1) == '7', ])


  l.individual.dfs <- list(df.assets = df.assets,
                           df.liabilities = df.liabilities,
                           df.income = df.income,
                           df.cashflow = df.cashflow,
                           df.value = df.value)

  # get consolidated dfs
  type.df <- 'consolidated'
  acc.desc  <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'Descricao', flag.thousands = flag.thousands))
  acc.value <-   as.numeric(sapply(xml_data, my.fct, type.df = type.df, info = 'Valor', flag.thousands = flag.thousands))
  acc.number <- as.character(sapply(xml_data, my.fct, type.df = type.df, info = 'id', flag.thousands = flag.thousands))

  consolidated.df <- data.frame(acc.number,acc.desc,acc.value)

  # save info
  df.assets.cons <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '1', ])
  df.liabilities.cons <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '2', ])
  df.income.cons    <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '3', ])
  df.cashflow.cons    <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '6', ])
  df.value.cons    <- stats::na.omit(consolidated.df[stringr::str_sub(consolidated.df$acc.number,1,1) == '7', ])


  l.consolidated.dfs <- list(df.assets = df.assets,
                             df.liabilities = df.liabilities,
                             df.income = df.income,
                             df.cashflow = df.cashflow,
                             df.value = df.value)

  # auditing report

  fin.report.file <- file.path(rnd.folder.name, 'AnexoTexto.xml')

  if (!file.exists(fin.report.file)) {
    stop('Cant find file', fin.report.file)
  }

  xml_data <- NA
  try({
    xml_data <- XML::xmlToList(XML::xmlParse(fin.report.file, encoding = 'UTF-8'))

  })

  if (is.na(xml_data)) {
    warning('Cant read auditing notes..')

    df.auditing.report = data.frame(text.indep.auditor =  NA,
                                    text.fiscal.counsil = NA,
                                    text.directors.about.fr = NA,
                                    text.directors.about.auditor = NA,
                                    stringsAsFactors = FALSE)
  } else {

    parsing.fct <- function(x, n.item) {

      if (x$NumeroQuadroRelacionado == as.character(n.item)) {
        return(x$Texto)
      } else {
        return('')
      }
    }


    text.indep.auditor <- paste0(sapply(xml_data, parsing.fct, n.item = 1655 ), collapse = '')
    text.fiscal.counsil <- paste0(sapply(xml_data, parsing.fct, n.item = 1657 ), collapse = '')
    text.directors.about.fr <- paste0(sapply(xml_data, parsing.fct, n.item = 1660 ), collapse = '')
    text.directors.about.auditor <- paste0(sapply(xml_data, parsing.fct, n.item = 1662 ), collapse = '')

    df.auditing.report = data.frame(text.indep.auditor =  text.indep.auditor,
                                    text.fiscal.counsil = text.fiscal.counsil,
                                    text.directors.about.fr = text.directors.about.fr,
                                    text.directors.about.auditor = text.directors.about.auditor,
                                    stringsAsFactors = FALSE)

  }

  my.l <- list(df.assets = df.assets,
               df.liabilities = df.liabilities,
               df.income = df.income,
               df.cashflow = df.cashflow,
               df.value = df.value,
               df.assets.cons = df.assets.cons,
               df.liabilities.cons = df.liabilities.cons,
               df.income.cons = df.income.cons,
               df.cashflow.cons = df.cashflow.cons,
               df.value.cons = df.value.cons,
               df.auditing.report = df.auditing.report)


  return(my.l)
}

#' Reads folder for zip file pre 2011 (internal)
#'
#' @inheritParams gdfpd.read.dfp.zip.file
#' @param rnd.folder.name Folder where unzipped files are available
#'
#' @return A list with financial statements
#'
#' @examples
#' # no example (this functions not used directly)
gdfpd.read.dfp.zip.file.type.2 <- function(rnd.folder.name, folder.to.unzip = tempdir()) {

  # figure out flag.thousands
  fin.report.file <- file.path(rnd.folder.name, 'CONFIG.XML')

  if (!file.exists(fin.report.file)) {
    flag.thousands = TRUE
  } else {
    xml_data <- XML::xmlToList(XML::xmlParse(fin.report.file, encoding = 'UTF-8'))
    flag.thousands <- switch(xml_data$ROWDATA$ROW['MOEDA'],
                             '02' = FALSE,
                             '01' = TRUE)
  }

  my.f <- list.files(rnd.folder.name,'DFPBPA', full.names = T)[1]
  df.assets <- gdfpd.read.fwf.file(my.f, flag.thousands)

  my.f <- list.files(rnd.folder.name, 'DFPBPP', full.names = T)[1]
  df.liabilities <- gdfpd.read.fwf.file(my.f, flag.thousands)

  my.f <- list.files(rnd.folder.name, 'DFPDERE', full.names = T)[1]
  df.income <- gdfpd.read.fwf.file(my.f, flag.thousands)

  my.f <- list.files(rnd.folder.name, 'DFPDVAE', full.names = T)[1]
  df.value <- gdfpd.read.fwf.file(my.f, flag.thousands)


  my.f <- list.files(rnd.folder.name, 'DFPDFCE', full.names = T)

  if (length(my.f) == 0) {
    df.cashflow <- data.frame(acc.desc  = NA,
                              acc.value = NA,
                              acc.number = NA)
  }else {
    df.cashflow <- gdfpd.read.fwf.file(my.f[1], flag.thousands)
  }

  l.individual.dfs <- list(df.assets = df.assets,
                           df.liabilities = df.liabilities,
                           df.income = df.income,
                           df.cashflow = df.cashflow,
                           df.value = df.value)

  # get consolidated fin statements

  my.f <- list.files(rnd.folder.name,'DFPCBPA', full.names = T)[1]
  df.assets.cons <- gdfpd.read.fwf.file(my.f, flag.thousands)


  my.f <- list.files(rnd.folder.name,'DFPCBPP', full.names = T)[1]
  df.liabilities.cons <- gdfpd.read.fwf.file(my.f, flag.thousands)


  my.f <- list.files(rnd.folder.name,'DFPCDER', full.names = T)[1]
  df.income.cons <- gdfpd.read.fwf.file(my.f, flag.thousands)

  my.f <- list.files(rnd.folder.name, 'DFPCDVAE', full.names = T)[1]
  df.value.cons <- gdfpd.read.fwf.file(my.f, flag.thousands)

  my.f <- list.files(rnd.folder.name,'DFPCDFCE', full.names = T)

  if (length(my.f) == 0) {
    df.cashflow.cons <- data.frame(acc.desc  = NA,
                                   acc.value = NA,
                                   acc.number = NA)
  } else {
    df.cashflow.cons <- gdfpd.read.fwf.file(my.f[1], flag.thousands)
  }

  l.consolidated.dfs<- list(df.assets = df.assets,
                            df.liabilities = df.liabilities,
                            df.income = df.income,
                            df.cashflow = df.cashflow,
                            df.value = df.value)

  # auditing report

  df.auditing.report = data.frame(text = NA)

  # get basic info

  my.l <- list(df.assets = df.assets,
               df.liabilities = df.liabilities,
               df.income = df.income,
               df.cashflow = df.cashflow,
               df.value = df.value,
               df.assets.cons = df.assets.cons,
               df.liabilities.cons = df.liabilities.cons,
               df.income.cons = df.income.cons,
               df.cashflow.cons = df.cashflow.cons,
               df.value.cons = df.value.cons,
               df.auditing.report = df.auditing.report)


  return(my.l)
}
