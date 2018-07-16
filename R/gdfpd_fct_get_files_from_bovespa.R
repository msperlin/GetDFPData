#' Fetches ALL new files from Bovespa
#'
#' @param my.id Company's ID
#'
#' @return A dataframe with several information about files
#' @export
#'
#' @examples
#'
#'  \dontrun{
#'  df.files <- gdfpd.get.files.from.bovespa(9512)
#'  }
gdfpd.get.files.from.bovespa <- function(my.id) {

  df.info <- data.frame()
  
  my.link <- paste0('http://bvmf.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM=',
                    my.id, '&ViewDoc=0#a')

  my.html <- paste0(readLines(my.link, encoding = 'Latin1'), collapse = '\n')

  info.out <- XML::readHTMLTable(my.html)

  if (length(info.out) == 0) { # found no data

    main.sector <- NA
    sub.sector <- NA
    segment <- NA
    my.tickers <- NA
    listing.segment <- NA

  } else {
    temp.str <- stringr::str_trim(stringr::str_split(info.out[[1]][[2]][4], '/')[[1]])

    main.sector <- temp.str[1]
    sub.sector <- temp.str[2]
    segment <- temp.str[3]

    temp.str <-   stringr::str_split(info.out[[1]][2][[1]][1], 'digos')[[1]][2]
    temp.str <-   stringr::str_split(stringr::str_split(temp.str, '\\n')[[1]][1], ';')[[1]]
    temp.str <- stringr::str_trim(temp.str)
    my.tickers <- paste0(temp.str, collapse = ';')

    # current listing segments

    # build dict
    my.dict <- data.frame(segments.id = c('Bovespa Mais',
                                          'Bovespa Mais - Level 2',
                                          'Novo Mercado',
                                          'Corporate Governance - Level 2',
                                          'Corporate Governance - Level 1'),
                          pic.file = c('InfEmpSeloBovespaMaisBrasil.png',
                                       'img_logo-bovmaisn2.png',
                                       'InfEmpLogoNovoMercado.png',
                                       'InfEmpLogoMercadoNivel2.png',
                                       'InfEmpLogoMercadoNivel1.png'), stringsAsFactors = F )

    segment.test <- sapply(X = my.dict$pic.file, FUN = function(x) return(stringr::str_detect(my.html, x)) )

    if (any(segment.test)) {
      listing.segment <- my.dict$segments.id[which(segment.test)]
    } else {
      listing.segment <- 'Tradicional'
    }

  }

  # get file locations
  df.info.itr <- get_files(my.id, type.fin.report = 'itr')
  df.info.dfp <- get_files(my.id, type.fin.report = 'dfp')
  df.info.fre <- get_files(my.id, type.fin.report = 'fre')
  df.info.fca <- get_files(my.id, type.fin.report = 'fca')

  df.info <- rbind(df.info.dfp, df.info.itr, df.info.fre, df.info.fca)

  df.info$main.sector <- main.sector
  df.info$sub.sector <- sub.sector
  df.info$segment <- segment
  df.info$tickers <- my.tickers
  df.info$listing.segment <- listing.segment

  # remove any NA data
  idx <- !is.na(df.info$id.date)

  if (all(!idx)) {
    df.info <- df.info[1, ]
  } else {
    df.info <- df.info[idx, ]
  }

  return(df.info)

}

#' Fetches files for different systens (INTERNAL)
#'
#' @param my.id Company id
#' @param type.fin.report  type of financial report (dfp/itr/fre/fca)
#'
#' @return A dataframe
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df.fre.files <- get_files(9512, type.fin.report = 'dfp')
#' }
get_files <- function(my.id, type.fin.report) {
  temp.df.info <- data.frame()

  # First version
  my.link <- paste0('http://bvmf.bmfbovespa.com.br/cias-listadas/empresas-listadas/HistoricoFormularioReferencia.aspx?codigoCVM=',
                    my.id, '&tipo=', type.fin.report,'&idioma=pt-br')

  #my.html <- paste0(readLines(my.link ), collapse = '\n')
  my.html <- readLines(my.link, warn = F )
  # find first ver

  if ( type.fin.report %in% c('fre', 'fca') ) {
    idx <- stringr::str_detect(my.html, 'onclick="AbreFormularioCadastral')
  } else {
    idx <- stringr::str_detect(my.html, 'href="javascript:AbreFormularioCadastral')
  }

  temp <- my.html[idx]

  if (type.fin.report %in% c('fre', 'fca') ) {
    temp <- temp[stringr::str_detect(temp,'Ativo')]

  }

  if (length(temp) == 0) {
    id.file = NA
    id.date = NA
    id.type = NA
    dl.link = NA
    version.file = NA

  } else {

    id.file <- stringr::str_match_all(temp, pattern = 'Documento=(.*)&')
    id.file <- sapply(id.file, function(x) return(x[,2]))

    dl.link <- paste0('http://www.rad.cvm.gov.br/enetconsulta/frmDownloadDocumento.aspx?CodigoInstituicao=2&NumeroSequencialDocumento=',
                      id.file)

    if (type.fin.report == 'itr') {

      id.date <-stringr::str_match_all(temp, pattern = ">(.*) - Informa")
      id.date <- as.Date(sapply(id.date, function(x) return(x[1,2])), '%d/%m/%Y')

      version.file <- stringr::str_match_all(temp, pattern = 'Vers.o (.*)</a>')
      version.file <- as.numeric(sapply(version.file, function(x) return(x[, 2])))

    } else if (type.fin.report == 'dfp') {

      id.date <-stringr::str_match_all(temp, pattern = ">(.*) - Demonst")
      id.date <- as.Date(sapply(id.date, function(x) return(x[1,2])), '%d/%m/%Y')

      version.file <- stringr::str_match_all(temp, pattern = 'Vers.o (.*)</a>')
      version.file <- sapply(version.file, function(x) return(x[, 2]))

    } else if (type.fin.report %in% c('fre', 'fca') ) {

      id.date <- stringr::str_match_all(temp, pattern = ">(.*) - Formul")
      id.date <- paste0(sapply(id.date, function(x) return(as.numeric(x[1,2]))) - 1 ,'-12-31')

      version.file <- stringr::str_match_all(temp, pattern = 'Vers.o (.*)</a>')
      version.file <- as.numeric(sapply(version.file, function(x) return(x[, 2])))

    }

    id.type = 'after 2011'

  }

  temp.df1 <- data.frame(id.company = my.id,
                         id.file,
                         version.file,
                         dl.link,
                         id.date = as.character(id.date),
                         id.type,
                         type.fin.report,
                         stringsAsFactors = F)

  temp.df.info <- rbind(temp.df.info, temp.df1)

  # find sec version links
  idx <- stringr::str_detect(my.html, 'href="javascript:ConsultarDXW')
  temp <- my.html[idx]


  if (length(temp) == 0) {
    id.file = NA
    id.date = NA
    id.type = NA
    dl.link = NA
    version.file = NA

  } else {

    id.file <- NA

    if (type.fin.report == 'itr') {
      id.date <-stringr::str_match_all(temp, pattern = ">(.*) - Informa")
    } else if (type.fin.report == 'dfp') {
      id.date <-stringr::str_match_all(temp, pattern = ">(.*) - Demonst")
    }

    id.date <- as.Date(sapply(id.date, function(x) return(x[,2])), '%d/%m/%Y')

    dl.link <- switch(type.fin.report,
                      'itr' = paste0('http://www2.bmfbovespa.com.br/dxw/Download.asp?moeda=L&site=B&mercado=1&ccvm=',
                                     my.id,
                                     '&data=', format(id.date,'%d/%m/%Y'),
                                     '&tipo=1'),
                      'dfp' = paste0('http://www2.bmfbovespa.com.br/dxw/Download.asp?moeda=L&site=B&mercado=1&ccvm=',
                                     my.id,
                                     '&data=', format(id.date,'%d/%m/%Y'),
                                     '&tipo=2') )

    version.file = NA
    id.type = 'before 2011'
  }

  temp.df2 <- data.frame(id.company = my.id,
                         id.file,
                         version.file,
                         dl.link,
                         id.date = as.character(id.date),
                         id.type,
                         type.fin.report,
                         stringsAsFactors = F)

  temp.df.info <- rbind(temp.df.info, temp.df2)

  return(temp.df.info)

}
